#include <stdbool.h>
#include <stdint.h>

/* The data word `after_flag` names: while it is not zero, `after` runs. */
int links_test_pending;

static int ticks;
static void *bound_object;
static void *bound_type;
static int bound_made;

int links_test_add(int a, int b) { return a + b; }
int64_t links_test_twice64(int64_t v) { return v * 2; }
bool links_test_negate(bool flag) { return !flag; }
double links_test_half(double x) { return x / 2.0; }
int links_test_id(int v) { return v; }

/* A cast: the value, and the program's type of it. */
int links_test_adjust(int v, void *type) { return type ? v + 100 : -1; }

void links_test_tick(void) { ticks++; }
int links_test_ticks(void) { return ticks; }
void links_test_set_pending(int v) { links_test_pending = v; }

int links_test_make(int seed) { return seed * 3; }
void links_test_bind(void *object, int made, void *type) {
    bound_object = object;
    bound_made = made;
    bound_type = type;
}
int links_test_bound_made(void) { return bound_made; }
bool links_test_bound_is(void *object) { return object == bound_object; }
bool links_test_bound_typed(void) { return bound_type != 0; }
