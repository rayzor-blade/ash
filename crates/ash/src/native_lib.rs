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


static STD_INIT: Once = Once::new();
pub static mut STD_LIBRARY: Option<Library> = None;

pub fn init_std_library() -> Result<()> {
    STD_INIT.call_once(|| {
        unsafe {
            let ext = if cfg!(target_os = "windows") {
                "dll"
            } else if cfg!(any(target_os = "macos", target_os = "ios")) {
                "dylib"
            } else {
                "so"
            };

            // Prefer the system libhl.dylib so HDLLs and the interpreter share
            // the SAME library instance (and thus the same GC static) — C hdlls
            // resolve their libhl.dylib install-name dependency to this path
            // regardless of what we dlopen. BUT only if it is not stale: a
            // system libhl missing the canary symbol predates the current
            // stdlib ABI and silently shadows every freshly built ash_std
            // (this cost months: all EventLoop debugging since March ran
            // against a stale root-owned build).
            // Override with ASH_LIBHL=system|embedded.
            // Bump the canary when the stdlib ABI changes materially.
            const CANARY_SYMBOL: &[u8] = b"hlp_pump_and_sleep\0";
            let system_libhl =
                std::path::Path::new("/usr/local/lib").join(format!("libhl.{}", ext));

            let force = std::env::var("ASH_LIBHL").unwrap_or_default();
            let system_ok = system_libhl.exists()
                && (force == "system" || {
                    let path_cstr = std::ffi::CString::new(system_libhl.to_str().unwrap()).unwrap();
                    let probe = libc::dlopen(path_cstr.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
                    if probe.is_null() {
                        false
                    } else {
                        let has_canary =
                            !libc::dlsym(probe, CANARY_SYMBOL.as_ptr() as *const libc::c_char)
                                .is_null();
                        libc::dlclose(probe);
                        has_canary
                    }
                });

            let lib_path = if system_ok && force != "embedded" {
                eprintln!("[ash] Using system libhl at {}", system_libhl.display());
                system_libhl
            } else {
                if system_libhl.exists() && force != "embedded" {
                    if !quiet() {
                        eprintln!(
                            "[ash] WARNING: {} exists but is STALE (missing canary symbol) — \
                             using embedded ash_std instead. C hdlls may still bind the stale \
                             copy; refresh it with: sudo cp <target>/libash_std.dylib {}",
                            system_libhl.display(),
                            system_libhl.display()
                        );
                    }
                }
                // Fallback 1: dlopen a real on-disk build artifact directly.
                // Extracting a fresh temp copy every run forces the kernel to
                // register a new code signature on each dlopen (fcntl
                // F_ADDFILESIGS), which can stall indefinitely once many leaked
                // temp copies have accumulated. A stable on-disk path avoids
                // that entirely. Candidates are resolved relative to the
                // executable (NOT the cwd).
                let lib_name = format!("libash_std.{}", ext);
                let mut candidates: Vec<std::path::PathBuf> = Vec::new();
                if let Ok(exe) = std::env::current_exe() {
                    if let Some(exe_dir) = exe.parent() {
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
            let path_cstr = CString::new(path.to_str().unwrap_or(""))
                .map_err(|e| anyhow!("Invalid path: {}", e))?;
            let handle =
                unsafe { libc::dlopen(path_cstr.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL) };
            if handle.is_null() {
                let err = unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) };
                return Err(anyhow!("Failed to load {}: {:?}", name, err));
            }
            unsafe { Library::from(libloading::os::unix::Library::from_raw(handle)) }
        };
        #[cfg(not(unix))]
        let library = unsafe { Library::new(path) }?;
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
            // ash_std uses direct exports (Rust #[no_mangle])
            let library = unsafe { STD_LIBRARY.as_ref() }
                .ok_or_else(|| anyhow!("std library not initialized"))?;
            let symbol: Symbol<*mut c_void> = unsafe { library.get(function_name.as_bytes()) }?;
            return Ok(*symbol);
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
