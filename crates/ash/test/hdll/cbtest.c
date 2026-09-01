// cbtest.hdll -- minimal reproduction of the hxDatachannel callback pattern:
//
//   1. Haxe hands a closure to the native library.
//   2. The native library stores the raw vclosure* in a malloc'd struct and
//      returns. No GC root is registered (hxDatachannel does not register one
//      either); the library just relies on the pointer staying valid.
//   3. Later, on a separate native call, the library invokes the stored
//      closure through hl_dyn_call(c, args, nargs) and returns the result.
//
// Between (2) and (3) the Haxe program allocates heavily, so the collector has
// a reason to run and an unrooted closure would be reclaimed.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cbtest_##n
#include "hl.h"

// A/B switch, purely diagnostic. `hlp_dyn_call` is ash_std's own function
// with upstream's (vclosure*, vdynamic**, int) shape; `hl_dyn_call` is the
// name every real HDLL calls. Setting CBTEST_HLP=1 routes through hlp_dyn_call
// so the two can be compared against one ash binary.
HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

typedef struct {
	int       magic;
	vclosure *cb;      // Int -> Int
	void     *fun_at_store;
} stash;

#define STASH_MAGIC 0x5AFEC0DE

static stash *g_stash = NULL;

// ---------------------------------------------------------------------------
// 1. store: keep the vclosure* in a malloc'd struct, no GC root.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	if( g_stash == NULL ) g_stash = (stash*)malloc(sizeof(stash));
	g_stash->magic = STASH_MAGIC;
	g_stash->cb = c;
	g_stash->fun_at_store = c ? c->fun : NULL;
	// CBTEST_ROOT=slot  : upstream HashLink spelling -- hl_add_root takes the
	//                     ADDRESS OF THE SLOT holding the pointer, and the
	//                     collector dereferences *slot each cycle.
	// CBTEST_ROOT=object: pass the object itself (what ash's hl_add_root
	//                     currently expects).
	{
		const char *mode = getenv("CBTEST_ROOT");
		if( mode && strcmp(mode,"slot") == 0 ) {
			printf("[cbtest] root    : hl_add_root(&stash->cb) = %p\n", (void*)&g_stash->cb);
			hl_add_root(&g_stash->cb);
		} else if( mode && strcmp(mode,"object") == 0 ) {
			printf("[cbtest] root    : hl_add_root(closure) = %p\n", (void*)c);
			hl_add_root(c);
		}
		fflush(stdout);
	}
	printf("[cbtest] store   : vclosure=%p t=%p fun=%p hasValue=%d\n",
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// 2. invoke: call the stored closure with one Int argument via hl_dyn_call.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;

	if( g_stash == NULL || g_stash->magic != STASH_MAGIC ) {
		printf("[cbtest] invoke  : nothing stored\n");
		fflush(stdout);
		return -1;
	}
	c = g_stash->cb;
	printf("[cbtest] invoke  : vclosure=%p t=%p fun=%p (fun at store=%p) hasValue=%d arg=%d\n",
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		g_stash->fun_at_store,
		c ? (int)c->hasValue : -1,
		arg);
	fflush(stdout);
	if( c == NULL ) return -2;

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CBTEST_HLP") ) {
		printf("[cbtest] path    : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cbtest] path    : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) {
		printf("[cbtest] result  : hl_dyn_call returned NULL\n");
		fflush(stdout);
		return -3;
	}
	printf("[cbtest] result  : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32);
