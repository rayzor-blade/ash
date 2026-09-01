// cb0.hdll -- CASE #0 (control): static Haxe function, ONE Int argument,
// handed to a native library, stored as a raw vclosure* in a malloc'd struct
// (no GC root, exactly like hxDatachannel), then invoked later through
// hl_dyn_call(c, args, 1).
//
//   Haxe:  static function triple(x:Int):Int { return x * 3 + 1; }
//   store(triple)  ->  ... heavy Haxe allocation ...  ->  invoke(14)
//   The only correct answer is 43.
//
// Diagnostic switches (all default OFF; getenv("")=="" still counts as set,
// so clear them with `env -u VAR`):
//   CB0_HLP=1        call hlp_dyn_call instead of hl_dyn_call (ABI A/B)
//   CB0_ROOT=slot    hl_add_root(&stash->cb)   <- upstream HashLink spelling
//   CB0_ROOT=object  hl_add_root(stash->cb)    <- what ash currently expects

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb0_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

typedef struct {
	int       magic;
	vclosure *cb;            // Int -> Int
	void     *t_at_store;
	void     *fun_at_store;
	int       hasValue_at_store;
} cb0_stash;

#define CB0_MAGIC 0x0CB00000

static cb0_stash *g_cb0 = NULL;

HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g_cb0 == NULL ) g_cb0 = (cb0_stash*)malloc(sizeof(cb0_stash));
	g_cb0->magic = CB0_MAGIC;
	g_cb0->cb = c;
	g_cb0->t_at_store = c ? (void*)c->t : NULL;
	g_cb0->fun_at_store = c ? c->fun : NULL;
	g_cb0->hasValue_at_store = c ? (int)c->hasValue : -1;

	printf("[cb0] store  : vclosure=%p t=%p fun=%p hasValue=%d\n",
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);

	mode = getenv("CB0_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb0] root   : hl_add_root(&stash->cb) slot=%p (upstream spelling)\n",
			(void*)&g_cb0->cb);
		fflush(stdout);
		hl_add_root(&g_cb0->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb0] root   : hl_add_root(closure) obj=%p\n", (void*)c);
		fflush(stdout);
		hl_add_root(c);
	} else {
		printf("[cb0] root   : none\n");
		fflush(stdout);
	}
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

HL_PRIM int HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;

	if( g_cb0 == NULL || g_cb0->magic != CB0_MAGIC ) {
		printf("[cb0] invoke : nothing stored\n");
		fflush(stdout);
		return -1;
	}
	c = g_cb0->cb;
	printf("[cb0] invoke : vclosure=%p t=%p (was %p) fun=%p (was %p) hasValue=%d (was %d) arg=%d\n",
		(void*)c,
		c ? (void*)c->t : NULL, g_cb0->t_at_store,
		c ? (void*)c->fun : NULL, g_cb0->fun_at_store,
		c ? (int)c->hasValue : -1, g_cb0->hasValue_at_store,
		arg);
	fflush(stdout);
	if( c == NULL ) return -2;
	if( (void*)c->t != g_cb0->t_at_store || c->fun != g_cb0->fun_at_store ) {
		printf("[cb0] WARN   : stored vclosure CONTENTS CHANGED since store()\n");
		fflush(stdout);
	}

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB0_HLP") ) {
		printf("[cb0] path   : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cb0] path   : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) {
		printf("[cb0] result : NULL\n");
		fflush(stdout);
		return -3;
	}
	printf("[cb0] result : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32);
