// cb16test.hdll -- CASE #16: a primitive raises with hl_throw.
//
// The Haxe program calls raise() inside a try. hl_throw must unwind to that
// catch, as it does under stock HashLink; it once ended in "hlp_throw called
// without active trap; aborting" and the program carried on as if nothing was
// thrown (git-bug d017dd4).

#define HL_NAME(n) cb16test_##n
#include "hl.h"

HL_PRIM void HL_NAME(raise)( void ) {
	hl_throw(hl_alloc_strbytes(USTR("raised by a native")));
}

DEFINE_PRIM(_VOID, raise, _NO_ARG);
