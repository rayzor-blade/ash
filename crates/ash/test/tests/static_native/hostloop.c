/* A window host's loop, as an app links it: `xwindow_run_turns` gives the
 * program its turns, and `hostloop_wait` suspends the program until the next
 * one through the function the program registered, as the host's wait does. */
static int turns;
static void (*suspend)(void);

void xwindow_set_turn_yield(void (*fn)(void)) { suspend = fn; }

int xwindow_run_turns(int (*turn)(void *), void *data) {
    for (;;) {
        turns++;
        if (!turn(data)) return 0;
    }
}

static int hostloop_wait(void) {
    if (suspend) suspend();
    return turns;
}

const void *hlp_hostloop_wait(const char **sign) {
    *sign = "_i";
    return (const void *)&hostloop_wait;
}
