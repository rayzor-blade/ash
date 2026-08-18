/// Cached presence check for a debug/trace env var.
///
/// macOS `getenv` takes a process-wide lock (`__findenv_locked`); these flags
/// gate hot paths (SafeCast, closure calls, allocation) where the repeated
/// lookup was a measured tax — half of all samples on one profile. Each
/// expansion reads its variable once, at first use, and caches the result.
macro_rules! env_flag {
    ($name:literal) => {{
        static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *CELL.get_or_init(|| std::env::var($name).is_ok())
    }};
    (os $name:literal) => {{
        static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *CELL.get_or_init(|| std::env::var_os($name).is_some())
    }};
}
