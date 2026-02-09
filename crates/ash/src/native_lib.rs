use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Once;
use tempfile::TempDir;
use anyhow::{anyhow, Result};
use std::ffi::c_void;
use crate::types::{HLNative, Str};
use std::path::Path;


static STD_INIT: Once = Once::new();
pub static mut STD_LIBRARY: Option<Library> = None;

pub fn init_std_library() -> Result<()> {
    STD_INIT.call_once(|| {
        unsafe {
            // Create a temporary directory
            let temp_dir = TempDir::new().expect("Failed to create temp dir");
            let mut lib_path = temp_dir.path().join("libash_std");
            lib_path.set_extension(
                #[cfg(any(target_os = "macos", target_os = "ios"))]
                "dylib",
                #[cfg(target_os = "windows")]
                "dll",
                #[cfg(all(unix, not(target_os = "macos")))]
                "so",
            );

            // Extract the embedded library
            let std_lib_bytes: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libash_std.a"));
            std::fs::write(&lib_path, std_lib_bytes).expect("Failed to write std library");

            // Load the library
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


#[derive(Debug)]
pub struct NativeLibraryManager {
    libraries: HashMap<Str, Library>,
}

impl NativeLibraryManager {
    pub fn new() -> Self {
        NativeLibraryManager {
            libraries: HashMap::new(),
        }
    }

    fn load_library(&mut self, name: &str, path: &Path) -> Result<()> {
        let library = unsafe { Library::new(path) }?;
        self.libraries.insert(Str::from(name), library);
        Ok(())
    }

    fn get_library(&self, name: &str) -> Option<&Library> {
        let clean = name.strip_prefix('?').unwrap_or(name);
        if clean == "std" {
            unsafe { return STD_LIBRARY.as_ref() }
        }
        self.libraries.get(&Str::from(clean))
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
            let candidates = [
                search_dir.join(format!("{}.hdll", lib_name)),
                search_dir.join(format!("lib{}.dylib", lib_name)),
                search_dir.join(format!("{}.dylib", lib_name)),
            ];
            let mut found = false;
            for candidate in &candidates {
                if candidate.exists() {
                    eprintln!("[ash] Loading HDLL: {} from {:?}", lib_name, candidate);
                    self.library_manager.load_library(lib_name, candidate)?;
                    found = true;
                    break;
                }
            }
            if !found {
                // Check if library was optional (had ? prefix)
                let is_optional = natives.iter().any(|n| n.lib.starts_with('?') && {
                    let clean = n.lib.strip_prefix('?').unwrap_or(&n.lib);
                    clean == lib_name
                });
                if is_optional {
                    eprintln!("[ash] Optional library '{}' not found, skipping", lib_name);
                } else {
                    return Err(anyhow!("Required library '{}' not found in {:?}", lib_name, search_dir));
                }
            }
        }

        Ok(())
    }

    pub fn resolve_function(&self, library_name: &str, function_name: &str) -> Result<*mut c_void> {
        let clean_lib = library_name.strip_prefix('?').unwrap_or(library_name);
        let library = self
            .library_manager
            .get_library(library_name)
            .ok_or_else(|| anyhow!("Library '{}' not found", library_name))?;

        unsafe {
            let symbol: Symbol<*mut c_void> = library.get(function_name.as_bytes())?;

            if clean_lib == "std" {
                // ash_std uses direct exports (Rust #[no_mangle])
                Ok(*symbol)
            } else {
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
}
