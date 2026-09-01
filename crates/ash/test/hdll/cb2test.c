// cb2test.hdll -- CASE #2: static Haxe functions with MANY arguments, called
// back from native code through hl_dyn_call.
//
// Same hxDatachannel shape as cbtest.c (malloc'd struct holds the raw
// vclosure*, no GC root by default), but the closures have 6..10 parameters,
// which walks the argument marshaller across the fixed-arity ladder and into
// whatever the "uniform entry" path is.
//
// Slots:
//   0..3  arity  6, 7, 8, 9   -- all _I32, return _I32
//   4     arity  9 MIXED      -- 5x_I32 + 4x_F64 interleaved, return _F64
//                                (fits AAPCS64 registers: x0-x4 / d0-d3)
//   5     arity 10            -- all _I32; only exercised with CB2_TEN=1,
//                                since HL_MAX_ARGS is 9 upstream too.
//
// Diagnostics:
//   CB2_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB2_ROOT=object  hl_add_root(closure)      (what ash's hl_add_root expects)
//   CB2_ROOT=slot    hl_add_root(&stash->cb)   (upstream HashLink spelling)
//   CB2_TEN=1        also store/invoke the 10-argument closure

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb2test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define SLOT_MAGIC 0x0CB20002
#define NSLOT 6

typedef struct {
	int       magic;
	vclosure *cb;
	int       nargs;
	int       mixed;      // 1 => interleave I32/F64 arguments, F64 return
	void     *fun_at_store;
} stash2;

static stash2 *g_slots[NSLOT];

static const char *cb2_slot_name( int idx ) {
	switch( idx ) {
	case 0: return "f6";
	case 1: return "f7";
	case 2: return "f8";
	case 3: return "f9";
	case 4: return "fmix9";
	case 5: return "f10";
	}
	return "?";
}

static void store_common( int idx, vclosure *c, int nargs, int mixed ) {
	stash2 *s;
	if( idx < 0 || idx >= NSLOT ) return;
	if( g_slots[idx] == NULL ) g_slots[idx] = (stash2*)malloc(sizeof(stash2));
	s = g_slots[idx];
	s->magic = SLOT_MAGIC;
	s->cb = c;
	s->nargs = nargs;
	s->mixed = mixed;
	s->fun_at_store = c ? c->fun : NULL;
	{
		const char *mode = getenv("CB2_ROOT");
		if( mode && strcmp(mode,"slot") == 0 ) {
			hl_add_root(&s->cb);
			printf("[cb2] root   %-5s: hl_add_root(&stash->cb) = %p\n", cb2_slot_name(idx), (void*)&s->cb);
		} else if( mode && strcmp(mode,"object") == 0 ) {
			hl_add_root(c);
			printf("[cb2] root   %-5s: hl_add_root(closure) = %p\n", cb2_slot_name(idx), (void*)c);
		}
	}
	printf("[cb2] store  %-5s: nargs=%d vclosure=%p t=%p fun=%p hasValue=%d\n",
		cb2_slot_name(idx), nargs, (void*)c,
		c ? (void*)c->t : NULL, c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
}

HL_PRIM void HL_NAME(store6)( vclosure *c )  { store_common(0, c, 6, 0); }
DEFINE_PRIM(_VOID, store6, _FUN(_I32, _I32 _I32 _I32 _I32 _I32 _I32));

HL_PRIM void HL_NAME(store7)( vclosure *c )  { store_common(1, c, 7, 0); }
DEFINE_PRIM(_VOID, store7, _FUN(_I32, _I32 _I32 _I32 _I32 _I32 _I32 _I32));

HL_PRIM void HL_NAME(store8)( vclosure *c )  { store_common(2, c, 8, 0); }
DEFINE_PRIM(_VOID, store8, _FUN(_I32, _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32));

HL_PRIM void HL_NAME(store9)( vclosure *c )  { store_common(3, c, 9, 0); }
DEFINE_PRIM(_VOID, store9, _FUN(_I32, _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32));

HL_PRIM void HL_NAME(storemix)( vclosure *c ) { store_common(4, c, 9, 1); }
DEFINE_PRIM(_VOID, storemix, _FUN(_F64, _I32 _F64 _I32 _F64 _I32 _F64 _I32 _F64 _I32));

HL_PRIM void HL_NAME(store10)( vclosure *c ) { store_common(5, c, 10, 0); }
DEFINE_PRIM(_VOID, store10, _FUN(_I32, _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32 _I32));

// ---------------------------------------------------------------------------
// invoke: build the boxed argument vector and call back into Haxe.
// Argument i is (100 + i) as Int, or (100 + i) + 0.5 as Float in mixed slots.
// ---------------------------------------------------------------------------

static vdynamic *do_invoke( int idx, int *ok ) {
	stash2   *s;
	vdynamic  boxes[10];
	vdynamic *args[10];
	int i;

	*ok = 0;
	if( idx < 0 || idx >= NSLOT || g_slots[idx] == NULL || g_slots[idx]->magic != SLOT_MAGIC ) {
		printf("[cb2] invoke %-5s: nothing stored\n", cb2_slot_name(idx));
		fflush(stdout);
		return NULL;
	}
	s = g_slots[idx];
	printf("[cb2] invoke %-5s: nargs=%d vclosure=%p t=%p fun=%p (fun at store=%p) hasValue=%d\n",
		cb2_slot_name(idx), s->nargs, (void*)s->cb,
		s->cb ? (void*)s->cb->t : NULL,
		s->cb ? (void*)s->cb->fun : NULL,
		s->fun_at_store,
		s->cb ? (int)s->cb->hasValue : -1);
	fflush(stdout);
	if( s->cb == NULL ) return NULL;

	for( i = 0; i < s->nargs; i++ ) {
		if( s->mixed && (i & 1) ) {
			boxes[i].t = &hlt_f64;
			boxes[i].v.d = (double)(100 + i) + 0.5;
		} else {
			boxes[i].t = &hlt_i32;
			boxes[i].v.i = 100 + i;
		}
		args[i] = &boxes[i];
	}

	*ok = 1;
	if( getenv("CB2_HLP") )
		return hlp_dyn_call(s->cb, args, s->nargs);
	return hl_dyn_call(s->cb, args, s->nargs);
}

HL_PRIM int HL_NAME(invokei)( int idx ) {
	int ok;
	vdynamic *ret = do_invoke(idx, &ok);
	if( !ok ) return -1;
	if( ret == NULL ) {
		printf("[cb2] result %-5s: dyn_call returned NULL\n", cb2_slot_name(idx));
		fflush(stdout);
		return -3;
	}
	printf("[cb2] result %-5s: vdynamic=%p t=%p kind=%d v.i=%d v.d=%f\n",
		cb2_slot_name(idx), (void*)ret, (void*)ret->t,
		ret->t ? (int)ret->t->kind : -1, ret->v.i, ret->v.d);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invokei, _I32);

HL_PRIM double HL_NAME(invoked)( int idx ) {
	int ok;
	vdynamic *ret = do_invoke(idx, &ok);
	if( !ok ) return -1.0;
	if( ret == NULL ) {
		printf("[cb2] result %-5s: dyn_call returned NULL\n", cb2_slot_name(idx));
		fflush(stdout);
		return -3.0;
	}
	printf("[cb2] result %-5s: vdynamic=%p t=%p kind=%d v.i=%d v.d=%f\n",
		cb2_slot_name(idx), (void*)ret, (void*)ret->t,
		ret->t ? (int)ret->t->kind : -1, ret->v.i, ret->v.d);
	fflush(stdout);
	return ret->v.d;
}
DEFINE_PRIM(_F64, invoked, _I32);
