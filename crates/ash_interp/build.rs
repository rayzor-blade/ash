fn main() {
    cc::Build::new()
        .file("src/trap_boundary.c")
        .warnings(true)
        .compile("ash_interp_trap_boundary");

    println!("cargo:rerun-if-changed=src/trap_boundary.c");
}
