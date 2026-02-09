#include <stdio.h>

#define HL_NAME(n) mytest_##n
#include "hl.h"

HL_PRIM int HL_NAME(add)(int a, int b) {
    return a + b;
}
DEFINE_PRIM(_I32, add, _I32 _I32);

HL_PRIM int HL_NAME(mul)(int a, int b) {
    return a * b;
}
DEFINE_PRIM(_I32, mul, _I32 _I32);

HL_PRIM void HL_NAME(hello)() {
    printf("Hello from mytest HDLL!\n");
    fflush(stdout);
}
DEFINE_PRIM(_VOID, hello, _NO_ARG);
