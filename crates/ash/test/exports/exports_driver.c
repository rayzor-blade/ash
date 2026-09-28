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
u64 exports_test_grow(u64, u64);
u64 exports_test_adder(u64);
u64 exports_test_thrower(void);
u64 exports_test_grow_of(u64);
u64 exports_test_call_ff(u64, u64);
u64 exports_test_call_ii(u64, u64);

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

/* A host function value is its factor's bits; the closure holds it in a
   cell, which a real host would allocate on the collected heap. */
static u64 cell;

void *exports_test_hold(u64 word) {
    cell = word;
    return &cell;
}

u64 exports_test_held(void *bound) {
    return *(u64 *)bound;
}

u64 exports_test_make_scaler(double k) {
    return bits(k);
}

/* Set while a call has left something pending; `after` clears it. */
int exports_test_pending;
static int afters;

void exports_test_after(void) {
    afters++;
    exports_test_pending = 0;
}

int exports_test_afters(void) {
    return afters;
}

u64 exports_test_call_fn_1(u64 fn, u64 x) {
    if (real(x) < 0)
        exports_test_pending = 1;
    return bits(real(fn) * real(x));
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
    /* A null receiver raises and answers unit, whether or not the export
       needs a trap for anything else. */
    if (exports_test_get_count(0) == 7 && raised == 2) ok |= 2048;
    if (exports_test_bump(0, 1) == 7 && raised == 3) ok |= 4096;
    if ((int)exports_test_grow(c, 1) == 41) ok |= 8192;
    if (exports_test_grow(0, 1) == 7 && raised == 4) ok |= 16384;
    /* Closures, called as their function type: a plain one, one that
       throws, a null one, and a method bound to its object. */
    if (real(exports_test_call_ff(exports_test_adder(bits(2.0)), bits(3.0))) == 5.0) ok |= 32768;
    if (exports_test_call_ff(exports_test_thrower(), bits(1.0)) == 7 && raised == 5) ok |= 65536;
    if (exports_test_call_ff(0, bits(1.0)) == 7 && raised == 6) ok |= 131072;
    if ((int)exports_test_call_ii(exports_test_grow_of(c), 2) == 43) ok |= 262144;
    return ok;
}
