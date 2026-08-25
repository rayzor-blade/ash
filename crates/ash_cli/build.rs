use std::env;

fn main() {
    // HDLLs are ordinary shared objects with undefined HashLink ABI symbols
    // such as `hl_blocking`.  ash provides those symbols from the statically
    // linked ash_std compatibility layer, but ELF executables do not place
    // their globals in .dynsym unless the final link asks for it.  Without
    // these targeted exports a Linux HDLL follows its DT_NEEDED edge to a
    // stock libhl.so instead, giving one process two runtime states; fmt's
    // first hl_blocking(true) then dereferences the uninitialised stock state.
    //
    // Keep this narrower than --export-dynamic: ash links LLVM statically and
    // exporting every global would needlessly expose a very large symbol set.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux") {
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic-symbol=hl_*");
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic-symbol=hlt_*");
    }
}
