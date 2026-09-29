#include "ash_future.h"

/* Hide the pending pointer from conservative global scanning. The runtime's
 * pending root, not this native data word, must keep the handle alive. */
static uintptr_t hidden_pending;

ash_future *future_test_create(void) {
    return hlp_future_create();
}

bool future_test_resolve(ash_future *future, void *value) {
    return hlp_future_resolve(future, value);
}

bool future_test_reject(ash_future *future, void *error) {
    return hlp_future_reject(future, error);
}

bool future_test_create_abandoned(void) {
    ash_future *future = hlp_future_create();
    hidden_pending = (uintptr_t)future ^ UINTPTR_MAX;
    return future != 0;
}

bool future_test_finish_abandoned(void) {
    ash_future *future = (ash_future *)(hidden_pending ^ UINTPTR_MAX);
    hidden_pending = 0;
    return hlp_future_resolve(future, 0);
}
