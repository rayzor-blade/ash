/* Natives of many integer arguments, as an HDLL exports them: each primitive's
 * hlp_<name> reports the signature and returns the function. Each argument is
 * weighted by its position. */
#include <stdint.h>
static int wide_9(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8) {
    return a0*1 + a1*2 + a2*3 + a3*4 + a4*5 + a5*6 + a6*7 + a7*8 + a8*9;
}
static int wide_12(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9, int a10, int a11) {
    return a0*1 + a1*2 + a2*3 + a3*4 + a4*5 + a5*6 + a6*7 + a7*8 + a8*9 + a9*10 + a10*11 + a11*12;
}
static int wide_14(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9, int a10, int a11, int a12, int a13) {
    return a0*1 + a1*2 + a2*3 + a3*4 + a4*5 + a5*6 + a6*7 + a7*8 + a8*9 + a9*10 + a10*11 + a11*12 + a12*13 + a13*14;
}
/* Booleans are one byte on the stack of Apple's arm64 ABI. */
static int wide_flags(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7,
                      _Bool b0, _Bool b1, _Bool b2, int a8, _Bool b3) {
    return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 * 100 + (b0 ? 1000 : 0) + (b1 ? 2000 : 0) + (b2 ? 4000 : 0) + (b3 ? 8000 : 0);
}
const void *hlp_wide_9(const char **sign) { *sign = "iiiiiiiii_i"; return (const void *)&wide_9; }
const void *hlp_wide_12(const char **sign) { *sign = "iiiiiiiiiiii_i"; return (const void *)&wide_12; }
const void *hlp_wide_14(const char **sign) { *sign = "iiiiiiiiiiiiii_i"; return (const void *)&wide_14; }
const void *hlp_wide_flags(const char **sign) { *sign = "iiiiiiiibbbib_i"; return (const void *)&wide_flags; }

/* Integers, floats and doubles interleaved: the floats take their own
 * registers and the overflow of each bank goes to the stack. */
static double wide_mixed(int a0, double b1, int a2, float f3, int a4, double b5, int a6, float f7,
                         int a8, double b9, int a10, float f11, int a12, double b13) {
    return a0*1 + b1*2 + a2*3 + f3*4 + a4*5 + b5*6 + a6*7 + f7*8 + a8*9 + b9*10 + a10*11 + f11*12 + a12*13 + b13*14;
}
/* A float result from a call whose last argument is on the stack. */
static float wide_single(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, float f9) {
    return (float)(a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8) * f9;
}
const void *hlp_wide_mixed(const char **sign) { *sign = "idifidifidifid_d"; return (const void *)&wide_mixed; }
const void *hlp_wide_single(const char **sign) { *sign = "iiiiiiiiif_f"; return (const void *)&wide_single; }
