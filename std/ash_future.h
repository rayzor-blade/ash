#ifndef ASH_FUTURE_H
#define ASH_FUTURE_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque, GC-owned handle. The creator may complete it once. A pending
 * handle stays rooted until that completion, even if Haxe drops its copy. */
typedef struct ash_future ash_future;

ash_future *hlp_future_create(void);
/* value and error are HashLink vdynamic* values (or null). */
bool hlp_future_resolve(ash_future *future, void *value);
bool hlp_future_reject(ash_future *future, void *error);
/* 0 pending, 1 resolved, 2 rejected. */
int32_t hlp_future_state(ash_future *future);
/* Waits for completion. Ash raises on rejection; stock HashLink returns the
 * stored error, and callers should check hlp_future_state() == 2. The Haxe
 * Future.await() method raises the error on both runtimes. */
void *hlp_future_await(ash_future *future);

#ifdef __cplusplus
}
#endif
#endif
