#include <setjmp.h>
#include <stddef.h>

typedef void *(*ash_trap_setup_fn)(void);
typedef void (*ash_trap_remove_fn)(void);
typedef void (*ash_trap_callback_fn)(void *context);

/*
 * Keep the setjmp frame alive while Rust invokes JIT or native code.
 *
 * HashLink exceptions use longjmp. Establishing the jump buffer in a helper
 * that returns before the callee starts leaves the buffer pointing at a dead
 * stack frame. The resulting jump can resume at arbitrary data addresses.
 * This C frame owns both setjmp and the callback invocation, so it remains a
 * valid landing point until the call has finished.
 */
int ash_interp_run_with_hl_trap(
    ash_trap_setup_fn setup,
    ash_trap_remove_fn remove,
    ash_trap_callback_fn callback,
    void *context) {
    jmp_buf *buffer;

    if (callback == NULL) {
        return -1;
    }
    if (setup == NULL) {
        callback(context);
        return 0;
    }

    buffer = (jmp_buf *)setup();
    if (buffer == NULL) {
        callback(context);
        return 0;
    }

#if defined(_WIN32)
    if (setjmp(*buffer) != 0) {
#else
    if (_setjmp(*buffer) != 0) {
#endif
        /* hlp_throw already popped and retired the active trap. */
        return 1;
    }

    callback(context);
    if (remove != NULL) {
        remove();
    }
    return 0;
}
