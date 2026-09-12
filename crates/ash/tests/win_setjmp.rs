//! What Win64's `_setjmp` and `longjmp` do when called the way the JIT calls
//! them.
//!
//! The JIT arms a trap by calling `_setjmp` at the address of the runtime's
//! own binding, with a null frame, and the runtime throws with `longjmp`.
//! Every other target's pair is the one C spells; Win64's `_setjmp` is a
//! macro over one of several CRT entry points, takes a second argument the
//! header hides, and its `longjmp` unwinds with SEH when the buffer's frame
//! field is set. This test records, on the one platform where it matters,
//! which function the binding reaches, whether the frame argument is honoured,
//! and whether a jump through the pair lands. Each answer is printed so a CI
//! log carries it; the assertions hold the contract the JIT relies on.
//!
//! The round trip runs in a child process, so a fault in the CRT pair is
//! reported as an exit status rather than taking the test runner with it.

#![cfg(all(windows, target_arch = "x86_64"))]

use std::ffi::c_void;
use std::mem::{align_of, size_of, zeroed};
use std::process::Command;

use ash_core::hl_bindings as hl;

const CHILD: &str = "ASH_WIN_SETJMP_CHILD";

/// The module an address lives in, or a note that no module claims it.
unsafe fn module_of(addr: usize) -> String {
    use windows_sys::Win32::Foundation::HMODULE;
    use windows_sys::Win32::System::LibraryLoader::{
        GetModuleFileNameW, GetModuleHandleExW, GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
        GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
    };
    let mut module: HMODULE = std::ptr::null_mut();
    let ok = GetModuleHandleExW(
        GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
        addr as *const u16,
        &mut module,
    );
    if ok == 0 || module.is_null() {
        return "<no module>".to_string();
    }
    let mut name = [0u16; 512];
    let len = GetModuleFileNameW(module, name.as_mut_ptr(), name.len() as u32) as usize;
    String::from_utf16_lossy(&name[..len.min(name.len())])
}

/// Where a call to `addr` really lands: through an import thunk
/// (`jmp [rip+disp]`) or a direct `jmp rel32` if that is what sits there.
unsafe fn call_target(addr: usize) -> usize {
    let p = addr as *const u8;
    let b = std::slice::from_raw_parts(p, 8);
    if b[0] == 0xFF && b[1] == 0x25 {
        let disp = i32::from_le_bytes([b[2], b[3], b[4], b[5]]) as isize;
        let slot = (addr as isize + 6 + disp) as *const usize;
        return *slot;
    }
    if b[0] == 0xE9 {
        let disp = i32::from_le_bytes([b[1], b[2], b[3], b[4]]) as isize;
        return (addr as isize + 5 + disp) as usize;
    }
    addr
}

unsafe fn describe(name: &str, addr: usize) {
    let target = call_target(addr);
    let bytes = std::slice::from_raw_parts(target as *const u8, 16);
    let hex: Vec<String> = bytes.iter().map(|b| format!("{b:02x}")).collect();
    println!(
        "{name}: binding at {addr:#x}, code at {target:#x} in {}\n    first bytes: {}",
        module_of(target),
        hex.join(" ")
    );
}

/// The child: arm a buffer through the binding exactly as the JIT does,
/// report what `_setjmp` stored, then jump back through `longjmp`.
unsafe fn round_trip() -> ! {
    // The binding's declared arity is the header's, not the ABI's; the JIT
    // always passes the frame, so this passes it the same way.
    let arm: unsafe extern "C" fn(*mut c_void, *mut c_void) -> i32 =
        std::mem::transmute(hl::_setjmp as usize);
    let mut buf: hl::jmp_buf = zeroed();
    let words = buf.as_mut_ptr() as *mut u64;
    let landed = arm(buf.as_mut_ptr().cast(), std::ptr::null_mut());
    if landed == 0 {
        println!(
            "armed: buf={:#x} frame={:#x} rsp={:#x} rip={:#x}",
            words as usize,
            *words,
            *words.add(2),
            *words.add(10)
        );
        hl::longjmp(buf.as_mut_ptr(), 7);
        // Only reached if the binding is not the noreturn the header
        // declares and the jump returned, which is its own finding.
        #[allow(unreachable_code)]
        {
            println!("longjmp returned");
            std::process::exit(4);
        }
    }
    println!("landed: {landed}");
    std::process::exit(if landed == 7 { 0 } else { 3 });
}

#[test]
fn setjmp_binding_and_round_trip() {
    if std::env::var_os(CHILD).is_some() {
        unsafe { round_trip() }
    }
    println!(
        "jmp_buf: {} bytes, align {}",
        size_of::<hl::jmp_buf>(),
        align_of::<hl::jmp_buf>()
    );
    unsafe {
        describe("_setjmp", hl::_setjmp as usize);
        describe("longjmp", hl::longjmp as usize);
    }
    // A `vdynamic`-style buffer read with `movdqa` needs 16; the CRT's
    // `_setjmp` writes the XMM registers that way.
    assert_eq!(align_of::<hl::jmp_buf>(), 16, "jmp_buf must be 16-aligned");
    assert_eq!(size_of::<hl::jmp_buf>(), 256, "jmp_buf is _JUMP_BUFFER");

    let exe = std::env::current_exe().expect("own path");
    let out = Command::new(&exe)
        .env(CHILD, "1")
        .args(["--exact", "setjmp_binding_and_round_trip", "--nocapture"])
        .output()
        .expect("spawn child");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = out.status.code();
    println!(
        "child exit {} ({})\n--- child stdout ---\n{stdout}--- child stderr ---\n{stderr}",
        code.map_or("<signal>".to_string(), |c| format!("{c:#x}")),
        code.map_or("killed".to_string(), |c| match c as u32 {
            0 => "landed".to_string(),
            0xC000_0005 => "STATUS_ACCESS_VIOLATION".to_string(),
            0xC000_001D => "STATUS_ILLEGAL_INSTRUCTION".to_string(),
            0xC000_0028 => "STATUS_BAD_STACK".to_string(),
            0xC000_0029 => "STATUS_INVALID_UNWIND_TARGET".to_string(),
            0xC000_0409 => "STATUS_STACK_BUFFER_OVERRUN".to_string(),
            0xC000_00FD => "STATUS_STACK_OVERFLOW".to_string(),
            other => format!("status {other:#x}"),
        })
    );
    assert!(
        stdout.contains("frame=0x0 "),
        "the null frame the JIT passes must be what _setjmp stores"
    );
    assert!(
        out.status.success() && stdout.contains("landed: 7"),
        "a longjmp through the binding must land at the _setjmp site"
    );
}
