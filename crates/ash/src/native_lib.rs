use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::sync::Once;
use tempfile::TempDir;
use anyhow::{anyhow, Result};
use std::ffi::c_void;
use crate::types::Str;
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
        let mut manager = NativeLibraryManager {
            libraries: HashMap::new(),
        };

        manager
    }

    fn load_library(&mut self, name: &str, path: &Path) -> Result<()> {
        let library = unsafe { Library::new(path) }?;
        self.libraries.insert(Str::from(name), library);
        Ok(())
    }

    fn get_library(&self, name: &str) -> Option<&Library> {
        if name == "std" || name == "?std" {
            unsafe { return STD_LIBRARY.as_ref() }
        }
        self.libraries.get(&Str::from(name))
    }
}

pub struct NativeFunctionResolver {
    library_manager: NativeLibraryManager,
}

impl NativeFunctionResolver {
    pub fn new() -> Self {
        let mut library_manager = NativeLibraryManager::new();
        NativeFunctionResolver { library_manager }
    }
    pub fn load_library(&mut self, name: &str, path: &Path) -> Result<()> {
        self.library_manager.load_library(name, path)
    }

    pub fn resolve_function(&self, library_name: &str, function_name: &str) -> Result<*mut c_void> {
        let library = self
            .library_manager
            .get_library(library_name)
            .ok_or_else(|| anyhow!("Library '{}' not found", library_name))?;

        unsafe {
            let symbol: Symbol<*mut c_void> = library.get(function_name.as_bytes())?;
            Ok(*symbol)
        }
    }
}
