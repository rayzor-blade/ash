use std::sync::atomic::{AtomicBool, Ordering};



static DEBUGGER_PRESENT: AtomicBool = AtomicBool::new(false);


#[no_mangle]
#[inline(never)]
#[cold]
pub extern "C" fn hlp_breakpoint() {
    hl_debug_break();
}

pub fn hl_debug_break() {
    if hl_detect_debugger() {
        unsafe {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            {
                #[cfg(target_os = "windows")]
                {
                    core::arch::asm!("int3");
                }
                #[cfg(not(target_os = "windows"))]
                {
                    core::arch::asm!("int3",
                        ".pushsection embed-breakpoints, \"aw\", @progbits",
                        ".quad .",
                        ".popsection"
                    );
                }
            }
            #[cfg(target_arch = "aarch64")]
            {
                #[cfg(target_os = "macos")]
                {
                    // On macOS ARM64, we'll just use the brk instruction without the extra section
                    core::arch::asm!("brk #0");
                }
                #[cfg(not(target_os = "macos"))]
                {
                    core::arch::asm!("brk #0",
                        ".pushsection .debug_gdb_scripts, \"MS\",@progbits,1",
                        ".byte 1",
                        ".asciz \"breakpoint {{ . }}\"",
                        ".popsection"
                    );
                }
            }
        }
    }
}

fn hl_detect_debugger() -> bool {
    if DEBUGGER_PRESENT.load(Ordering::Relaxed) {
        return true;
    }

    let debugger_present = unsafe {
        #[cfg(target_os = "windows")]
        {
            use winapi::um::debugapi::IsDebuggerPresent;
            IsDebuggerPresent() != 0
        }
        #[cfg(target_os = "linux")]
        {
            use std::fs::File;
            use std::io::Read;
            let mut status = String::new();
            File::open("/proc/self/status")
                .and_then(|mut f| f.read_to_string(&mut status))
                .map(|_| !status.contains("TracerPid:\t0"))
                .unwrap_or(false)
        }
        #[cfg(target_os = "macos")]
        {
            use libc::{ptrace, PT_DENY_ATTACH};
            
            // Try to deny debugger attachment
            let result = ptrace(PT_DENY_ATTACH, 0, std::ptr::null_mut(), 0);
            
            // If ptrace returns -1, it means a debugger is already attached
            result == -1
        }
        #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
        {
            false // Unsupported OS, assume no debugger
        }
    };

    DEBUGGER_PRESENT.store(debugger_present, Ordering::Relaxed);
    debugger_present
}