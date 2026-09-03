use crate::types::HLNative;
use anyhow::{anyhow, Result};
use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::collections::HashSet;
use std::ffi::c_void;
use std::path::Path;
use std::sync::{Arc, Mutex, Once, OnceLock};
use tempfile::TempDir;

/// Whether to suppress ash's own startup diagnostics.
///
/// These lines go to stderr, and stderr is compared against an oracle's by the
/// parity harness -- which no oracle will ever match, because they are ash
/// talking about itself. Thirty-one cases failed on nothing else. `--quiet`
/// already means "do not narrate"; it just was not reaching here.
static QUIET: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Silence the startup diagnostics. Called by the CLI when `--quiet` is given.
pub fn set_quiet(on: bool) {
    QUIET.store(on, std::sync::atomic::Ordering::Relaxed);
}

fn quiet() -> bool {
    QUIET.load(std::sync::atomic::Ordering::Relaxed)
}

/// ash_std's exports, addressed directly rather than through a dynamic loader.
///
/// build.rs writes this from `std/src/*.rs`: an `extern "C"` declaration per
/// `#[no_mangle]` native plus a name -> address map. The declarations are what
/// make the linker pull each one out of the rlib archive — without a reference
/// nothing would be linked, since an archive contributes only the members that
/// satisfy an undefined symbol.
mod std_symbols {
    include!(concat!(env!("OUT_DIR"), "/std_symbols.rs"));
}

/// Whether `std@` natives resolve against the linked-in copy of ash_std.
///
/// The two copies must never both be live. ash_std owns the GC, so a program
/// running half its natives from the binary and half from a dlopened dylib has
/// two heaps, and an object allocated in one is invisible to the other's
/// collector. The choice is therefore made once, before anything allocates,
/// and never revisited.
static STATIC_STD: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// True when `std@` resolution is going through the linked-in runtime.
pub fn std_is_static() -> bool {
    STATIC_STD.load(std::sync::atomic::Ordering::Relaxed)
}

#[cfg(any(target_os = "macos", windows))]
fn program_directory(program: &Path) -> &Path {
    program
        .parent()
        .filter(|dir| !dir.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

/// Decide how ash_std will be reached, before anything touches it.
///
/// ELF uses the linked-in copy: executable exports preempt the same symbols in
/// an HDLL's `libhl.so` dependency, so both callers reach one GC. Mach-O's
/// two-level namespace instead pins each HDLL import to `libhl.dylib`; when an
/// HDLL is beside the bytecode, ash selects that compatibility-named copy for
/// its own std@ resolution as well. PE behaves like Mach-O here for the same
/// reason: an HDLL's imports name `libhl.dll` and the loader binds them to
/// that module, never to the executable, so ash loads the same file rather
/// than leaving the hdlls talking to a runtime whose GC nobody started. The
/// platform mechanisms differ, but the invariant is the same: exactly one
/// active stdlib state.
///
/// `ASH_STD_LINKAGE=dynamic` remains available for A/B diagnosis on programs
/// without HDLLs.
pub fn choose_std_linkage(program: &Path) -> bool {
    let forced_dynamic = std::env::var("ASH_STD_LINKAGE").as_deref() == Ok("dynamic");
    // Mach-O keeps the dylib ordinal on every undefined symbol. An HDLL that
    // imports `hl_gc_alloc_gen` from `libhl.dylib` therefore does not use the
    // identically named export in the main executable (ELF/Linux does). When
    // native extensions are present, route ash's own std@ calls through that
    // same compatibility image so there is exactly one GC and one set of HL
    // type singletons in active use.
    // A bare filename such as `game.hl` has `Path::new("")` as its parent.
    // `read_dir("")` fails even though later HDLL discovery correctly treats
    // `"".join("sdl.hdll")` as a file in the cwd. Make both decisions use the
    // same directory, or macOS commits to the static runtime and then loads
    // HDLLs bound to a second GC.
    #[cfg(any(target_os = "macos", windows))]
    let has_hdll = std::fs::read_dir(program_directory(program)).is_ok_and(|entries| {
        entries
            .filter_map(Result::ok)
            .any(|entry| entry.path().extension().and_then(|ext| ext.to_str()) == Some("hdll"))
    });
    #[cfg(not(any(target_os = "macos", windows)))]
    let has_hdll = false;

    let static_ok = !forced_dynamic && !has_hdll;
    STATIC_STD.store(static_ok, std::sync::atomic::Ordering::Relaxed);
    static_ok
}

/// A `std@` native's address, from whichever copy of ash_std is in use.
///
/// This is the single resolution point: the `#[load_symbol]` macro, the
/// bytecode decoder's `hlp_hash_gen`, and the native resolver all come
/// through here, so they cannot end up bound to different copies.
pub fn std_symbol_addr(name: &str) -> Option<usize> {
    if std_is_static() {
        return std_symbols::std_symbol_table().get(name).copied();
    }
    unsafe {
        let lib = STD_LIBRARY.as_ref()?;
        let sym: Symbol<*mut c_void> = lib.get(name.as_bytes()).ok()?;
        Some(*sym as usize)
    }
}

static STD_INIT: Once = Once::new();
pub static mut STD_LIBRARY: Option<Library> = None;

/// A system-wide libhl that is safe to prefer over the embedded ash_std, or
/// `None` to take the embedded/on-disk path.
///
/// Preferring the system libhl makes HDLLs and the interpreter share the SAME
/// library instance (and thus the same GC static) — C hdlls resolve their
/// libhl.dylib install-name dependency to this path regardless of what we
/// dlopen. BUT only if it is not stale: a system libhl missing the canary
/// symbol predates the current stdlib ABI and silently shadows every freshly
/// built ash_std (this cost months: all EventLoop debugging since March ran
/// against a stale root-owned build).
/// Override with ASH_LIBHL=system|embedded.
/// Bump the canary when the stdlib ABI changes materially.
#[cfg(unix)]
fn usable_system_libhl(ext: &str) -> Option<std::path::PathBuf> {
    const CANARY_SYMBOL: &[u8] = b"hlp_pump_and_sleep\0";
    let force = std::env::var("ASH_LIBHL").unwrap_or_default();
    if force == "embedded" {
        return None;
    }
    let system_libhl = std::path::Path::new("/usr/local/lib").join(format!("libhl.{}", ext));
    if !system_libhl.exists() {
        return None;
    }
    let system_ok = force == "system"
        || unsafe {
            let path_cstr = std::ffi::CString::new(system_libhl.to_str().unwrap()).unwrap();
            let probe = libc::dlopen(path_cstr.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
            if probe.is_null() {
                false
            } else {
                let has_canary =
                    !libc::dlsym(probe, CANARY_SYMBOL.as_ptr() as *const libc::c_char).is_null();
                libc::dlclose(probe);
                has_canary
            }
        };
    if system_ok {
        Some(system_libhl)
    } else {
        if !quiet() {
            eprintln!(
                "[ash] WARNING: {} exists but is STALE (missing canary symbol) — \
                 using embedded ash_std instead. C hdlls may still bind the stale \
                 copy; refresh it with: sudo cp <target>/libash_std.dylib {}",
                system_libhl.display(),
                system_libhl.display()
            );
        }
        None
    }
}

/// Windows has no system libhl to defer to: there is no /usr/local/lib install
/// convention, and DLL imports bind at load time rather than through a global
/// symbol search order, so the sharing argument above cannot arise. The
/// embedded/on-disk path is taken unconditionally; sharing one runtime with
/// the hdlls is instead `choose_std_linkage`'s job, by loading the very
/// `libhl.dll` their import tables name.
#[cfg(not(unix))]
fn usable_system_libhl(_ext: &str) -> Option<std::path::PathBuf> {
    None
}

/// Write the shared runtime this binary carries to `dest`, and return it.
///
/// The emitter embeds the runtime so that `ash` needs nothing beside it to
/// run a program; the same copy is what an AOT binary links against, so
/// nobody has to have built the workspace to compile one. The bytes are the
/// library, whatever the file they are stored under is called.
pub fn write_embedded_runtime(dest: &Path) -> Result<()> {
    const RUNTIME: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libash_std.a"));
    if let Some(dir) = dest.parent() {
        std::fs::create_dir_all(dir)?;
    }
    std::fs::write(dest, RUNTIME)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(dest, std::fs::Permissions::from_mode(0o755))?;
    }
    Ok(())
}

pub fn init_std_library() -> Result<()> {
    if std_is_static() {
        // No loader, no temp file, no codesign, no signature registration —
        // the runtime is already in this address space. Just start its GC,
        // once, the same way the dlopened path does.
        STD_INIT.call_once(|| unsafe {
            ash_std::gc::hlp_gc_init();
        });
        return Ok(());
    }
    STD_INIT.call_once(|| {
        unsafe {
            let ext = if cfg!(target_os = "windows") {
                "dll"
            } else if cfg!(any(target_os = "macos", target_os = "ios")) {
                "dylib"
            } else {
                "so"
            };

            // The system-libhl preference (and its staleness probe) is a unix
            // concern; see usable_system_libhl for the reasoning and the
            // ASH_LIBHL override. On Windows it always answers None.
            #[cfg(any(target_os = "macos", windows))]
            let sibling_libhl = {
                let compat = if cfg!(windows) {
                    "libhl.dll"
                } else {
                    "libhl.dylib"
                };
                std::env::current_exe()
                    .ok()
                    .and_then(|exe| exe.parent().map(|dir| dir.join(compat)))
                    .filter(|path| path.exists())
            };
            #[cfg(not(any(target_os = "macos", windows)))]
            let sibling_libhl: Option<std::path::PathBuf> = None;

            let lib_path = if let Some(sibling) = sibling_libhl {
                if !quiet() {
                    eprintln!(
                        "[ash] Loading HDLL-compatible ash_std at {}",
                        sibling.display()
                    );
                }
                sibling
            } else if let Some(system_libhl) = usable_system_libhl(ext) {
                eprintln!("[ash] Using system libhl at {}", system_libhl.display());
                system_libhl
            } else {
                // Fallback 1: dlopen a real on-disk build artifact directly.
                // Extracting a fresh temp copy every run forces the kernel to
                // register a new code signature on each dlopen (fcntl
                // F_ADDFILESIGS), which can stall indefinitely once many leaked
                // temp copies have accumulated. A stable on-disk path avoids
                // that entirely. Candidates are resolved relative to the
                // executable (NOT the cwd).
                // MSVC drops the `lib` prefix on a cdylib: ash_std.dll, not
                // libash_std.dll (crates/ash/build.rs names it the same way).
                let lib_name = if cfg!(windows) {
                    "ash_std.dll".to_string()
                } else {
                    format!("libash_std.{}", ext)
                };
                let mut candidates: Vec<std::path::PathBuf> = Vec::new();
                if let Ok(exe) = std::env::current_exe() {
                    if let Some(exe_dir) = exe.parent() {
                        // macOS HDLL imports name libhl.dylib explicitly. The
                        // release bundle and conformance runner stage the
                        // current ash_std under that compatibility name; load
                        // that exact image first so both sides share state.
                        #[cfg(target_os = "macos")]
                        candidates.push(exe_dir.join("libhl.dylib"));
                        // Sibling next to the executable
                        candidates.push(exe_dir.join(&lib_name));
                        // exe in target/debug/ -> dylib in target/<triple>/debug/
                        candidates.push(
                            exe_dir
                                .join("../aarch64-apple-darwin/debug")
                                .join(&lib_name),
                        );
                        // exe in target/<triple>/debug/ -> dylib in target/debug/
                        candidates.push(exe_dir.join("../../debug").join(&lib_name));
                    }
                }
                if let Some(found) = candidates.iter().find(|c| c.exists()) {
                    let canonical = found.canonicalize().unwrap_or_else(|_| found.clone());
                    if !quiet() {
                        eprintln!("[ash] Loading on-disk ash_std at {}", canonical.display());
                    }
                    canonical
                } else {
                    // Last resort: extract the embedded library to a temp file
                    let temp_dir = TempDir::new().expect("Failed to create temp dir");
                    let mut path = temp_dir.path().join("libash_std");
                    path.set_extension(ext);
                    let std_lib_bytes: &[u8] =
                        include_bytes!(concat!(env!("OUT_DIR"), "/libash_std.a"));
                    std::fs::write(&path, std_lib_bytes).expect("Failed to write std library");
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::PermissionsExt;
                        let _ =
                            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755));
                    }
                    // Ad-hoc sign the fresh copy so dlopen's code-signature
                    // registration doesn't stall in the kernel (ignore failure).
                    #[cfg(target_os = "macos")]
                    {
                        let _ = std::process::Command::new("codesign")
                            .args(["-s", "-", "-f"])
                            .arg(&path)
                            .status();
                    }
                    eprintln!(
                        "[ash] Extracted embedded ash_std to {} (last resort)",
                        path.display()
                    );
                    // Leak temp_dir so the file isn't deleted
                    std::mem::forget(temp_dir);
                    path
                }
            };

            // Load the library with RTLD_GLOBAL so hl_ symbols are visible
            // to subsequently loaded HDLLs (they link against libhl.dylib symbols).
            #[cfg(unix)]
            let lib = {
                use std::ffi::CString;
                let path_cstr = CString::new(lib_path.to_str().unwrap()).expect("invalid path");
                let handle = libc::dlopen(path_cstr.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL);
                if handle.is_null() {
                    let err = std::ffi::CStr::from_ptr(libc::dlerror());
                    panic!("Failed to load std library: {:?}", err);
                }
                // Wrap in libloading::Library for consistent API
                Library::from(libloading::os::unix::Library::from_raw(handle))
            };
            #[cfg(not(unix))]
            let lib = Library::new(&lib_path).expect("Failed to load std library");

            // Initialize the GC before any native calls
            let gc_init: Symbol<unsafe extern "C" fn()> = lib
                .get(b"hlp_gc_init")
                .expect("Failed to find hlp_gc_init in std library");
            gc_init();

            STD_LIBRARY = Some(lib);
        }
    });

    Ok(())
}

/// Process-global registry of loaded HDLL handles, keyed by clean library
/// name. Shared across ALL NativeLibraryManager instances so the
/// interpreter's resolver and the pre-warmed tiered JIT's resolver reuse the
/// same dlopen handle: each HDLL is loaded (and logged) exactly once per
/// process instead of once per resolver instance. dlopen refcounts, so the
/// per-instance registries "worked", but every hdll (uv/fmt/ui/sdl/...) was
/// dlopen'd and logged twice in hybrid mode.
static LOADED_LIBRARIES: OnceLock<Mutex<HashMap<String, Arc<Library>>>> = OnceLock::new();

fn loaded_libraries() -> &'static Mutex<HashMap<String, Arc<Library>>> {
    LOADED_LIBRARIES.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Canonical process-global symbol table: `"lib@symbol"` (clean lib name,
/// resolved symbol name like `hlp_alloc_bytes`) -> implementation address.
///
/// Built once at startup after HDLL loading (see `build_symbol_table`) from
/// the bytecode natives list — STD_LIBRARY direct exports plus every loaded
/// hdll's DEFINE_PRIM resolution — then extended lazily as new symbols are
/// resolved. Consumers:
///   - interpreter `call_native` (table hit + per-findex cache instead of a
///     per-call format!/dlsym)
///   - tiered JIT `init_native_func` via `resolve_function`
///   - a future cranelift `JITBuilder` can ingest `symbol_table_entries()`
///     wholesale at build time (addresses are final impl pointers, already
///     past the DEFINE_PRIM resolver indirection).
static SYMBOL_TABLE: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();

fn symbol_table() -> &'static Mutex<HashMap<String, usize>> {
    SYMBOL_TABLE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn symbol_key(library_name: &str, function_name: &str) -> String {
    let clean = library_name.strip_prefix('?').unwrap_or(library_name);
    format!("{}@{}", clean, function_name)
}

#[derive(Debug)]
pub struct NativeLibraryManager;

/// Read a file's magic bytes and say what it is.
///
/// An HDLL is native code, so it has to match the platform and architecture of
/// the runtime loading it. When it does not, the loader's own message says so
/// only obliquely — Windows reports "%1 is not a valid Win32 application" and
/// dyld prints every path it tried — naming neither the file's real format nor
/// the mismatch.
fn describe_object_file(path: &Path) -> Option<(&'static str, &'static str, String)> {
    let mut head = [0u8; 64];
    let read = {
        use std::io::Read;
        let mut f = std::fs::File::open(path).ok()?;
        f.read(&mut head).ok()?
    };
    if read < 4 {
        return None;
    }
    let le32 =
        |o: usize| -> u32 { u32::from_le_bytes([head[o], head[o + 1], head[o + 2], head[o + 3]]) };
    let le16 = |o: usize| -> u16 { u16::from_le_bytes([head[o], head[o + 1]]) };

    // Architectures are named the way `std::env::consts::ARCH` names them, so
    // the file's own claim can be compared with the running build's.
    let mach_arch = |cpu: u32| match cpu {
        0x0100_000C => "aarch64",
        0x0000_000C => "arm",
        0x0100_0007 => "x86_64",
        0x0000_0007 => "x86",
        _ => "an unknown architecture",
    };
    match le32(0) {
        0xFEED_FACF if read >= 8 => {
            let a = mach_arch(le32(4));
            Some(("Mach-O", a, format!("a macOS Mach-O 64-bit {a} library")))
        }
        0xFEED_FACE if read >= 8 => {
            let a = mach_arch(le32(4));
            Some(("Mach-O", a, format!("a macOS Mach-O 32-bit {a} library")))
        }
        // Universal ("fat") archives store their magic big-endian and carry
        // several architectures, so none is singled out here.
        0xBEBA_FECA | 0xCAFE_BABE => Some((
            "Mach-O",
            "several architectures",
            "a macOS universal (fat) library".to_string(),
        )),
        0x464C_457F => {
            let a = match le16(18) {
                0x3E => "x86_64",
                0xB7 => "aarch64",
                0x28 => "arm",
                0x03 => "x86",
                _ => "an unknown architecture",
            };
            Some(("ELF", a, format!("a Linux/BSD ELF {a} shared object")))
        }
        _ => {
            // PE keeps its real header behind the DOS stub.
            if head[0] == b'M' && head[1] == b'Z' && read >= 0x40 {
                let off = le32(0x3C) as usize;
                if off + 6 <= read && &head[off..off + 4] == b"PE\0\0" {
                    let a = match le16(off + 4) {
                        0x8664 => "x86_64",
                        0xAA64 => "aarch64",
                        0x014C => "x86",
                        _ => "an unknown architecture",
                    };
                    return Some(("PE", a, format!("a Windows PE {a} DLL")));
                }
                return Some((
                    "PE",
                    "an unknown architecture",
                    "a Windows PE DLL".to_string(),
                ));
            }
            None
        }
    }
}

/// The object family this build can load.
fn host_object_family() -> &'static str {
    if cfg!(target_os = "macos") || cfg!(target_os = "ios") {
        "Mach-O"
    } else if cfg!(windows) {
        "PE"
    } else {
        "ELF"
    }
}

/// Explain a failed HDLL load in terms of what the file actually is.
///
/// The loader's own text goes last: dyld in particular prints every path it
/// tried, which buries the one line that matters.
fn hdll_load_error(name: &str, path: &Path, cause: &str) -> anyhow::Error {
    let host_os = std::env::consts::OS;
    let host_arch = std::env::consts::ARCH;
    let display = path.display();
    match describe_object_file(path) {
        Some((family, arch, kind)) if family != host_object_family() || arch != host_arch => {
            anyhow!(
                "cannot load HDLL \"{name}\" from {display}\n\
                 It is {kind}, but this ash build runs on {host_os} {host_arch}.\n\
                 An HDLL is native code and has to be built for the same platform and \
                 architecture as the runtime loading it. Rebuild it for {host_os} \
                 {host_arch}, or run an ash build matching the HDLL.\n\
                 Loader said: {cause}"
            )
        }
        Some((_, _, kind)) => anyhow!(
            "cannot load HDLL \"{name}\" from {display}\n\
             It is {kind}, which suits this build ({host_os} {host_arch}), so the \
             refusal is not a platform mismatch — a library it depends on is the \
             usual cause.\n\
             Loader said: {cause}"
        ),
        None => anyhow!(
            "cannot load HDLL \"{name}\" from {display}\n\
             The file is not a native library this ash build ({host_os} {host_arch}) \
             recognises; it may be a text file, an archive, or truncated.\n\
             Loader said: {cause}"
        ),
    }
}

impl NativeLibraryManager {
    pub fn new() -> Self {
        NativeLibraryManager
    }

    /// True if a library with this (clean) name is already registered
    /// process-wide (loaded earlier by any resolver instance).
    fn is_loaded(name: &str) -> bool {
        let clean = name.strip_prefix('?').unwrap_or(name);
        loaded_libraries()
            .lock()
            .expect("HDLL registry poisoned")
            .contains_key(clean)
    }

    /// Fetch a registered library handle by (clean) name.
    fn get_registered(name: &str) -> Option<Arc<Library>> {
        let clean = name.strip_prefix('?').unwrap_or(name);
        loaded_libraries()
            .lock()
            .expect("HDLL registry poisoned")
            .get(clean)
            .cloned()
    }

    fn load_library(&mut self, name: &str, path: &Path) -> Result<()> {
        let clean = name.strip_prefix('?').unwrap_or(name).to_string();
        // Hold the registry lock across dlopen so two resolvers racing on the
        // same library cannot both load it.
        let mut registry = loaded_libraries().lock().expect("HDLL registry poisoned");
        if registry.contains_key(&clean) {
            // Already loaded by another resolver instance — share the handle.
            return Ok(());
        }
        // Load HDLLs with RTLD_GLOBAL so they can see ash_std's hl_ symbols.
        // On macOS, also use flat namespace to override libhl.dylib bindings.
        #[cfg(unix)]
        let library = {
            use std::ffi::CString;
            // Absolute, always. `dlopen` treats a name with NO SLASH in it as
            // a SONAME and looks only in the cache and the library paths --
            // never in the working directory. A program named on the command
            // line as a bare `game.hl` has "" for its parent, so the candidate
            // built from it is a bare `sdl.hdll`, which `Path::exists` happily
            // resolves against the cwd and `dlopen` then refuses to find:
            // "cannot open shared object file: No such file or directory",
            // naming a file sitting right there. macOS resolves a relative
            // path against the cwd and so never showed it; on Linux every
            // HDLL beside a bytecode file failed to load.
            let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
            let path_cstr = CString::new(absolute.to_str().unwrap_or(""))
                .map_err(|e| anyhow!("Invalid path: {}", e))?;
            let handle =
                unsafe { libc::dlopen(path_cstr.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL) };
            if handle.is_null() {
                let err = unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) };
                return Err(hdll_load_error(name, path, &err.to_string_lossy()));
            }
            unsafe { Library::from(libloading::os::unix::Library::from_raw(handle)) }
        };
        // A Windows loader resolves an HDLL's own imports -- SDL3.dll for
        // sdl.hdll, OpenAL32.dll for openal.hdll -- against the standard
        // search order, which begins at the *executable's* directory and never
        // includes the directory the HDLL itself came from. HashLink's hl.exe
        // ships inside that directory, so upstream never meets this; ash is
        // installed elsewhere, so a game's bin folder would have to copy its
        // DLLs next to ash.exe instead. LOAD_WITH_ALTERED_SEARCH_PATH puts the
        // HDLL's own directory at the head of the search, which is what lets a
        // game directory stay self-contained. It applies only to an absolute
        // path, hence the join.
        #[cfg(not(unix))]
        let library = {
            use libloading::os::windows::{Library as WinLibrary, LOAD_WITH_ALTERED_SEARCH_PATH};
            let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
            let loaded =
                unsafe { WinLibrary::load_with_flags(&absolute, LOAD_WITH_ALTERED_SEARCH_PATH) }
                    .map_err(|e| hdll_load_error(name, path, &e.to_string()))?;
            Library::from(loaded)
        };
        registry.insert(clean, Arc::new(library));
        Ok(())
    }
}

pub struct NativeFunctionResolver {
    library_manager: NativeLibraryManager,
}

impl NativeFunctionResolver {
    pub fn new() -> Self {
        let library_manager = NativeLibraryManager::new();
        NativeFunctionResolver { library_manager }
    }

    pub fn load_library(&mut self, name: &str, path: &Path) -> Result<()> {
        self.library_manager.load_library(name, path)
    }

    /// Discover and load external HDLL libraries referenced by bytecode natives.
    /// Searches for .hdll files in the given directory.
    pub fn discover_and_load_libraries(
        &mut self,
        search_dir: &Path,
        natives: &[HLNative],
    ) -> Result<()> {
        let mut libs: HashSet<String> = HashSet::new();
        for native in natives {
            let clean = native.lib.strip_prefix('?').unwrap_or(&native.lib);
            if clean != "std" {
                libs.insert(clean.to_string());
            }
        }

        for lib_name in &libs {
            if NativeLibraryManager::is_loaded(lib_name) {
                // Already loaded process-wide (e.g. by the interpreter's
                // resolver before the tiered JIT pre-warm) — don't re-dlopen
                // or re-log.
                continue;
            }
            let mut candidates = vec![search_dir.join(format!("{}.hdll", lib_name))];
            #[cfg(target_os = "windows")]
            {
                candidates.push(search_dir.join(format!("{}.dll", lib_name)));
                candidates.push(search_dir.join(format!("lib{}.dll", lib_name)));
            }
            #[cfg(any(target_os = "macos", target_os = "ios"))]
            {
                candidates.push(search_dir.join(format!("lib{}.dylib", lib_name)));
                candidates.push(search_dir.join(format!("{}.dylib", lib_name)));
            }
            #[cfg(all(unix, not(any(target_os = "macos", target_os = "ios"))))]
            {
                candidates.push(search_dir.join(format!("lib{}.so", lib_name)));
                candidates.push(search_dir.join(format!("{}.so", lib_name)));
            }
            let mut found = false;
            for candidate in candidates {
                if candidate.exists() {
                    eprintln!("[ash] Loading HDLL: {} from {:?}", lib_name, candidate);
                    self.library_manager.load_library(lib_name, &candidate)?;
                    found = true;
                    break;
                }
            }
            if !found {
                // Check if library was optional (had ? prefix)
                let is_optional = natives.iter().any(|n| {
                    n.lib.starts_with('?') && {
                        let clean = n.lib.strip_prefix('?').unwrap_or(&n.lib);
                        clean == lib_name
                    }
                });
                if is_optional {
                    eprintln!("[ash] Optional library '{}' not found, skipping", lib_name);
                } else {
                    return Err(anyhow!(
                        "Required library '{}' not found in {:?}",
                        lib_name,
                        search_dir
                    ));
                }
            }
        }

        // Build the canonical process-global symbol table now that every
        // hdll is loaded. Idempotent: the second resolver instance (tiered
        // JIT pre-warm) finds the table already populated.
        let (resolved, missing) = self.build_symbol_table(natives);
        if !missing.is_empty() {
            eprintln!(
                "[ash] symbol table: {} natives resolved, {} missing: {}",
                resolved,
                missing.len(),
                missing.join(", ")
            );
        }

        Ok(())
    }

    /// Look up a symbol in the process-global table without touching dlsym.
    pub fn lookup_symbol(library_name: &str, function_name: &str) -> Option<*mut c_void> {
        symbol_table()
            .lock()
            .expect("symbol table poisoned")
            .get(&symbol_key(library_name, function_name))
            .map(|&addr| addr as *mut c_void)
    }

    /// Snapshot of the whole symbol table as `("lib@symbol", address)` pairs.
    /// Designed for bulk consumption (e.g. a future cranelift `JITBuilder`
    /// registering every known native before codegen); addresses are the
    /// final implementation pointers.
    pub fn symbol_table_entries() -> Vec<(String, usize)> {
        symbol_table()
            .lock()
            .expect("symbol table poisoned")
            .iter()
            .map(|(k, &v)| (k.clone(), v))
            .collect()
    }

    /// Populate the canonical symbol table from the bytecode natives list.
    /// Called once after HDLL loading; idempotent and shared process-wide.
    /// Returns (resolved_count, missing_keys) — misses are left out of the
    /// table so consumers keep their lazy-resolution fallback semantics.
    pub fn build_symbol_table(&self, natives: &[HLNative]) -> (usize, Vec<String>) {
        let mut resolved = 0usize;
        let mut missing = Vec::new();
        for native in natives {
            let name = format!("hlp_{}", native.name);
            let key = symbol_key(&native.lib, &name);
            if symbol_table()
                .lock()
                .expect("symbol table poisoned")
                .contains_key(&key)
            {
                resolved += 1;
                continue;
            }
            match self.resolve_function_uncached(&native.lib, &name) {
                Ok(addr) if !addr.is_null() => {
                    symbol_table()
                        .lock()
                        .expect("symbol table poisoned")
                        .insert(key, addr as usize);
                    resolved += 1;
                }
                _ => missing.push(key),
            }
        }
        (resolved, missing)
    }

    /// Resolve a native symbol: process-global table first, then the slow
    /// dlsym/DEFINE_PRIM path (memoized into the table on success).
    pub fn resolve_function(&self, library_name: &str, function_name: &str) -> Result<*mut c_void> {
        if let Some(addr) = Self::lookup_symbol(library_name, function_name) {
            return Ok(addr);
        }
        let addr = self.resolve_function_uncached(library_name, function_name)?;
        if !addr.is_null() {
            symbol_table()
                .lock()
                .expect("symbol table poisoned")
                .insert(symbol_key(library_name, function_name), addr as usize);
        }
        Ok(addr)
    }

    fn resolve_function_uncached(
        &self,
        library_name: &str,
        function_name: &str,
    ) -> Result<*mut c_void> {
        let clean_lib = library_name.strip_prefix('?').unwrap_or(library_name);

        if clean_lib == "std" {
            // ash_std uses direct exports (Rust #[no_mangle]), reached either
            // through the linked-in copy or the dylib — see std_symbol_addr.
            return std_symbol_addr(function_name)
                .map(|a| a as *mut c_void)
                .ok_or_else(|| anyhow!("std native '{}' not found", function_name));
        }

        let library = NativeLibraryManager::get_registered(clean_lib)
            .ok_or_else(|| anyhow!("Library '{}' not found", library_name))?;

        unsafe {
            let symbol: Symbol<*mut c_void> = library.get(function_name.as_bytes())?;

            // HDLLs use DEFINE_PRIM resolver protocol:
            // hlp_xxx is a resolver: fn(*mut *const c_char) -> *mut c_void
            type ResolverFn = unsafe extern "C" fn(*mut *const std::ffi::c_char) -> *mut c_void;
            let resolver: ResolverFn = std::mem::transmute(*symbol);
            let mut sign: *const std::ffi::c_char = std::ptr::null();
            let actual_fn = resolver(&mut sign);
            Ok(actual_fn)
        }
    }
}

#[cfg(all(test, target_os = "macos"))]
mod tests {
    use super::*;

    #[test]
    fn bare_program_filename_uses_current_directory() {
        assert_eq!(program_directory(Path::new("game.hl")), Path::new("."));
        assert_eq!(
            program_directory(Path::new("games/demo.hl")),
            Path::new("games")
        );
    }
}

#[cfg(test)]
mod hdll_format_tests {
    use super::*;
    use std::io::Write;

    fn write_temp(name: &str, bytes: &[u8]) -> std::path::PathBuf {
        let p = std::env::temp_dir().join(name);
        let mut f = std::fs::File::create(&p).expect("temp file");
        f.write_all(bytes).expect("write");
        p
    }

    fn macho_arm64() -> Vec<u8> {
        // magic 0xFEEDFACF then cputype CPU_TYPE_ARM64, both little-endian.
        let mut v = 0xFEED_FACFu32.to_le_bytes().to_vec();
        v.extend_from_slice(&0x0100_000Cu32.to_le_bytes());
        v.extend_from_slice(&[0u8; 56]);
        v
    }

    fn elf_x86_64() -> Vec<u8> {
        let mut v = vec![0x7F, b'E', b'L', b'F', 2, 1, 1, 0];
        v.extend_from_slice(&[0u8; 10]);
        v.extend_from_slice(&0x3Eu16.to_le_bytes()); // e_machine at 18
        v.extend_from_slice(&[0u8; 44]);
        v
    }

    fn pe_x86_64() -> Vec<u8> {
        let mut v = vec![0u8; 64];
        v[0] = b'M';
        v[1] = b'Z';
        let off: u32 = 0x30; // e_lfanew, inside the 64 bytes we read
        v[0x3C..0x40].copy_from_slice(&off.to_le_bytes());
        v[0x30..0x34].copy_from_slice(b"PE\0\0");
        v[0x34..0x36].copy_from_slice(&0x8664u16.to_le_bytes());
        v
    }

    #[test]
    fn every_hdll_format_is_named_from_its_magic() {
        let cases: [(&str, Vec<u8>, &str); 3] = [
            ("ash_fmt_macho.hdll", macho_arm64(), "Mach-O 64-bit aarch64"),
            ("ash_fmt_elf.hdll", elf_x86_64(), "ELF x86_64"),
            ("ash_fmt_pe.hdll", pe_x86_64(), "PE x86_64"),
        ];
        for (name, bytes, expect) in cases {
            let p = write_temp(name, &bytes);
            let got = describe_object_file(&p)
                .map(|(_, _, k)| k)
                .unwrap_or_default();
            assert!(
                got.contains(expect),
                "{name}: expected {expect:?}, got {got:?}"
            );
            let _ = std::fs::remove_file(&p);
        }
    }

    #[test]
    fn a_file_that_is_not_a_library_is_not_guessed_at() {
        let p = write_temp("ash_fmt_text.hdll", b"this is not a library at all\n");
        assert!(describe_object_file(&p).is_none());
        let _ = std::fs::remove_file(&p);
    }

    /// The issue a Windows user hit in reverse: an HDLL built for another
    /// platform must fail with a message that names the file's real format and
    /// the running build, not just the loader's complaint.
    #[test]
    fn loading_a_foreign_hdll_explains_the_mismatch() {
        // Pick a format this build definitely cannot load.
        let (name, bytes) = if cfg!(windows) {
            ("ash_foreign_load.hdll", macho_arm64())
        } else {
            ("ash_foreign_load.hdll", pe_x86_64())
        };
        let p = write_temp(name, &bytes);
        let mut resolver = NativeFunctionResolver::new();
        let err = resolver
            .load_library("sdl", &p)
            .expect_err("a foreign-format library must not load");
        let msg = format!("{err:#}");
        let _ = std::fs::remove_file(&p);
        // Shown by `cargo test -- --nocapture`: this is the text a user sees.
        println!("--- HDLL mismatch message ---\n{msg}\n-----------------------------");

        assert!(msg.contains("cannot load HDLL"), "msg: {msg}");
        assert!(msg.contains("sdl"), "msg names the library: {msg}");
        assert!(
            msg.contains(std::env::consts::OS),
            "msg names the running build: {msg}"
        );
        assert!(
            msg.contains("built for the same platform"),
            "msg explains the fix: {msg}"
        );
    }
}
