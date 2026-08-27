#include <stddef.h>

/*
 * glibc's backtrace() stops at anonymous JIT code because a JITModule does
 * not publish an .eh_frame section. Cranelift does preserve RBP, however.
 * Keep this tiny native boundary's frame pointer and hand it to Rust, which
 * can walk the generated callers without requiring frame pointers in Rust
 * itself. This is only built for x86_64 Linux.
 */
extern int hlp_call_stack_raw_from_frame(void *array, void **frame);

__attribute__((noinline)) int hlp_call_stack_raw(void *array) {
    return hlp_call_stack_raw_from_frame(array, __builtin_frame_address(0));
}
