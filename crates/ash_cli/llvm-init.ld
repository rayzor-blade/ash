/* LLVM's static constructors, kept out of .init_array so the loader does not
   run them at startup; ash runs them on first use (ash_core::llvm_init).
   LLVM's objects reach the link inside llvm-sys's rlib. */
SECTIONS {
  ash_llvm_init_array : {
    PROVIDE_HIDDEN(__ash_llvm_init_start = .);
    KEEP(*libllvm_sys-*.rlib:*(SORT_BY_INIT_PRIORITY(.init_array.*) .init_array .ctors .ctors.*))
    PROVIDE_HIDDEN(__ash_llvm_init_end = .);
  }
} INSERT AFTER .init_array;
