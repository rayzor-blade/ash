// cb12test.hdll -- CASE #12: TWO closures stored natively, called INTERLEAVED.
//
// Why this shape matters: with a single stored closure a rooting bug is
// all-or-nothing. With two, the collector can reclaim one while the other
// survives (different allocation sites, different blocks, different lines),
// so a partial failure is visible -- exactly the hxDatachannel symptom where
// "some callbacks work and some come back wrong".
//
// Pattern, faithful to hxDatachannel:
//   1. Haxe hands closure A and closure B to the native library.
//   2. The library keeps both raw vclosure* in ONE malloc'd struct. No GC
//      root is registered by default.
//   3. Later calls invoke them through hl_dyn_call(c, args, 1), interleaved:
//      A, B, A, B ... both from separate native calls driven by Haxe and
//      from a native pump() loop that alternates without returning to Haxe.
//
// Diagnostic env switches (all default OFF; getenv("")=set, use `env -u`):
//   CB12_ROOT=object   hl_add_root(closure)     on BOTH closures
//   CB12_ROOT=slot     hl_add_root(&pair->cb_x) on BOTH  (upstream spelling)
//   CB12_ROOT=a_object hl_add_root(closure) on A ONLY -- asymmetric probe:
//                      if rooting works, A must survive and B may not.
//   CB12_HLP=1         route through hlp_dyn_call instead of hl_dyn_call

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define HL_NAME(n) cb12_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define PAIR_MAGIC 0x0C12BEEF

typedef struct {
	int       magic;
	vclosure *cb_a;
	vclosure *cb_b;
	void     *fun_a_at_store;
	void     *fun_b_at_store;
	void     *val_a_at_store;
	void     *val_b_at_store;
	hl_type  *t_a_at_store;
	hl_type  *t_b_at_store;
} pair12;

static pair12 *g_pair = NULL;

static void ensure_pair( void ) {
	if( g_pair == NULL ) {
		g_pair = (pair12*)malloc(sizeof(pair12));
		memset(g_pair, 0, sizeof(pair12));
		g_pair->magic = PAIR_MAGIC;
	}
}

static const char *root_mode( void ) {
	const char *m = getenv("CB12_ROOT");
	return m ? m : "none";
}

// ---------------------------------------------------------------------------
// store_a / store_b -- keep the raw vclosure* in the malloc'd struct.
// ---------------------------------------------------------------------------
static void store_common( const char *which, vclosure *c, vclosure **slot ) {
	const char *mode = root_mode();
	*slot = c;
	printf("[cb12] store %s : vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		which,
		(void*)c,
		c ? (void*)c->t   : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? (void*)c->value  : NULL);
	if( strcmp(mode,"object") == 0 ) {
		printf("[cb12] root  %s : hl_add_root(closure=%p)\n", which, (void*)c);
		hl_add_root(c);
	} else if( strcmp(mode,"slot") == 0 ) {
		printf("[cb12] root  %s : hl_add_root(&slot=%p)  [upstream spelling]\n",
			which, (void*)slot);
		hl_add_root(slot);
	} else if( strcmp(mode,"a_object") == 0 ) {
		if( strcmp(which,"A") == 0 ) {
			printf("[cb12] root  %s : hl_add_root(closure=%p)  [A only]\n", which, (void*)c);
			hl_add_root(c);
		} else {
			printf("[cb12] root  %s : (deliberately unrooted)\n", which);
		}
	}
	fflush(stdout);
}

HL_PRIM void HL_NAME(store_a)( vclosure *c ) {
	ensure_pair();
	store_common("A", c, &g_pair->cb_a);
	g_pair->fun_a_at_store = c ? c->fun   : NULL;
	g_pair->val_a_at_store = c ? c->value : NULL;
	g_pair->t_a_at_store   = c ? c->t     : NULL;
}
DEFINE_PRIM(_VOID, store_a, _FUN(_I32,_I32));

HL_PRIM void HL_NAME(store_b)( vclosure *c ) {
	ensure_pair();
	store_common("B", c, &g_pair->cb_b);
	g_pair->fun_b_at_store = c ? c->fun   : NULL;
	g_pair->val_b_at_store = c ? c->value : NULL;
	g_pair->t_b_at_store   = c ? c->t     : NULL;
}
DEFINE_PRIM(_VOID, store_b, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// the shared invocation path
// ---------------------------------------------------------------------------
static int call_one( const char *which, vclosure *c, void *fun_at_store,
                     void *val_at_store, hl_type *t_at_store, int arg ) {
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;

	if( c == NULL ) {
		printf("[cb12] call  %s : NULL closure\n", which);
		fflush(stdout);
		return -2;
	}
	printf("[cb12] call  %s : arg=%d vclosure=%p t=%p%s fun=%p%s value=%p%s hasValue=%d\n",
		which, arg, (void*)c,
		(void*)c->t,   c->t     == t_at_store   ? "" : " <<CHANGED",
		(void*)c->fun, c->fun   == fun_at_store ? "" : " <<CHANGED",
		(void*)c->value, c->value == val_at_store ? "" : " <<CHANGED",
		(int)c->hasValue);
	fflush(stdout);

	// Case #12 exists to show ONE closure dying while the OTHER survives, so
	// a corrupted closure must not abort the process before the second one is
	// exercised. Refuse to hand obvious garbage to hl_dyn_call and report it
	// instead; CB12_UNSAFE=1 removes the guard and takes the real crash.
	if( !getenv("CB12_UNSAFE") ) {
		int bad = 0;
		if( c->t != t_at_store )                 bad = 1;
		if( ((uintptr_t)c->t & 7) != 0 )         bad = 1;
		if( c->t == NULL )                       bad = 1;
		if( !bad && c->t->kind != HFUN && c->t->kind != HMETHOD ) bad = 1;
		if( bad ) {
			printf("[cb12] CORRUPT %s : closure header no longer describes a function"
			       " -- refusing hl_dyn_call\n", which);
			fflush(stdout);
			return -4;
		}
	}

	a.t   = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB12_HLP") )
		ret = hlp_dyn_call(c, args, 1);
	else
		ret = hl_dyn_call(c, args, 1);

	if( ret == NULL ) {
		printf("[cb12] ret   %s : NULL\n", which);
		fflush(stdout);
		return -3;
	}
	printf("[cb12] ret   %s : vdynamic=%p kind=%d v.i=%d\n",
		which, (void*)ret, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}

HL_PRIM int HL_NAME(call_a)( int arg ) {
	if( g_pair == NULL || g_pair->magic != PAIR_MAGIC ) return -1;
	return call_one("A", g_pair->cb_a, g_pair->fun_a_at_store,
	                g_pair->val_a_at_store, g_pair->t_a_at_store, arg);
}
DEFINE_PRIM(_I32, call_a, _I32);

HL_PRIM int HL_NAME(call_b)( int arg ) {
	if( g_pair == NULL || g_pair->magic != PAIR_MAGIC ) return -1;
	return call_one("B", g_pair->cb_b, g_pair->fun_b_at_store,
	                g_pair->val_b_at_store, g_pair->t_b_at_store, arg);
}
DEFINE_PRIM(_I32, call_b, _I32);

// ---------------------------------------------------------------------------
// pump(n) -- the hxDatachannel process_events() shape: alternate A and B
// inside ONE native call, never returning to Haxe between invocations.
// Returns the sum of all results.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(pump)( int rounds ) {
	int i, sum = 0;
	if( g_pair == NULL || g_pair->magic != PAIR_MAGIC ) return -1;
	for( i = 1; i <= rounds; i++ ) {
		sum += call_one("A", g_pair->cb_a, g_pair->fun_a_at_store,
		                g_pair->val_a_at_store, g_pair->t_a_at_store, i);
		sum += call_one("B", g_pair->cb_b, g_pair->fun_b_at_store,
		                g_pair->val_b_at_store, g_pair->t_b_at_store, i);
	}
	printf("[cb12] pump    : rounds=%d sum=%d\n", rounds, sum);
	fflush(stdout);
	return sum;
}
DEFINE_PRIM(_I32, pump, _I32);

// distinct pointers? (a collector that moved/merged them would show here)
HL_PRIM int HL_NAME(distinct)( void ) {
	if( g_pair == NULL || g_pair->magic != PAIR_MAGIC ) return -1;
	printf("[cb12] ident   : A=%p B=%p distinct=%d\n",
		(void*)g_pair->cb_a, (void*)g_pair->cb_b,
		g_pair->cb_a != g_pair->cb_b);
	fflush(stdout);
	return g_pair->cb_a != g_pair->cb_b;
}
DEFINE_PRIM(_I32, distinct, _NO_ARG);
