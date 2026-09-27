/* Calls the members Exports.hx exports, as a host's object would. Each
   check sets one bit of the answer. */
typedef unsigned long long u64;

u64 exports_test_add(u64, u64);
u64 exports_test_new_counter(u64);
u64 exports_test_new_twice(u64);
u64 exports_test_bump(u64, u64);
u64 exports_test_get_count(u64);
u64 exports_test_set_count(u64, u64);
u64 exports_test_get_made(void);
u64 exports_test_fail(u64);
u64 exports_test_half(u64);
u64 exports_test_half_boxed(u64);

static int raised;

void exports_test_raise(void *exception) {
    if (exception)
        raised++;
}

static u64 bits(double d) {
    union { double d; u64 u; } x;
    x.d = d;
    return x.u;
}

static double real(u64 u) {
    union { double d; u64 u; } x;
    x.u = u;
    return x.d;
}

int exports_test_drive(void) {
    int ok = 0;
    if ((int)exports_test_add(2, 3) == 5) ok |= 1;
    u64 c = exports_test_new_counter(10);
    if ((int)exports_test_bump(c, 5) == 15) ok |= 2;
    u64 t = exports_test_new_twice(10);
    if ((int)exports_test_bump(t, 5) == 20) ok |= 4;
    if (exports_test_set_count(c, 40) == 7 && (int)exports_test_get_count(c) == 40) ok |= 8;
    if ((int)exports_test_get_made() == 2) ok |= 16;
    if (exports_test_fail(1) == 7 && raised == 1) ok |= 32;
    if ((int)exports_test_fail(0) == 0 && raised == 1) ok |= 64;
    if (real(exports_test_half(bits(3.0))) == 1.5) ok |= 128;
    if ((int)exports_test_add((u64)-4, 1) == -3) ok |= 256;
    /* The inline NaN-boxing casts: a number is its bits, anything else
       reads as NaN, and a NaN comes back canonical. */
    if (real(exports_test_half_boxed(bits(3.0))) == 1.5) ok |= 512;
    if (exports_test_half_boxed(0x7ffc000000000001ULL) == 0x7ff8000000000000ULL) ok |= 1024;
    return ok;
}
