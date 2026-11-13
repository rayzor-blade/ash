/* The native library `test_static_native.hl` calls, as an app would link it:
 * each primitive's `hlp_<name>` resolver reports the signature and returns the
 * function. `toy_absent` is deliberately not defined. */
static int toy_add(int a, int b) { return a + b; }
static double toy_scale(double x, double k) { return x * k; }

const void *hlp_toy_add(const char **sign) {
    *sign = "ii_i";
    return (const void *)&toy_add;
}

const void *hlp_toy_scale(const char **sign) {
    *sign = "dd_d";
    return (const void *)&toy_scale;
}
