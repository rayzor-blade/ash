// cb13test.hdll -- CASE 13: a stored Haxe closure called REPEATEDLY from a
// native loop.
//
// This is the hxDatachannel process_events() shape taken to its limit:
//
//   1. Haxe hands a closure to the native library.
//   2. The library keeps the raw vclosure* in a malloc'd struct. No GC root
//      (hxDatachannel registers none either).
//   3. Later the library runs a LOOP entirely in native code, calling
//      hl_dyn_call(c, args, 1) once per iteration.
//
// The callback itself allocates on every call, so the collector has many
// chances to run *inside* the native loop. If the stored closure is not
// rooted, the interesting failure is not "call 1 fails" but "call 1..k
// succeed and call k+1 returns garbage / NULL / crashes" -- the closure gets
// collected mid-run.
//
// Diagnostic switches (all default OFF; use `env -u VAR` to clear one --
// getenv("") still counts as set):
//   CB13_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB13_ROOT=object  hl_add_root(closure)      (what ash's hl_add_root expects)
//   CB13_ROOT=slot    hl_add_root(&stash->cb)   (the upstream HashLink spelling)
//   CB13_VERBOSE=1    print every iteration
//   CB13_SCRUB=1      between target calls, drive allocation through a SECOND
//                     (explicitly rooted) generator closure from a helper
//                     frame, and zero the pump frame's own copy of the target
//                     pointer, so no conservatively-scanned native slot is
//                     holding the target when the collection lands. This is
//                     what turns "collected mid-run" from a theory into an
//                     observation.
//   CB13_GEN=<k>      generator calls per target call (default 200)
// Combine with ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb13test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define CB13_MAGIC 0x13BEEF13
#define CB13_BASE  7

typedef struct {
	int       magic;
	vclosure *cb;
	void     *fun_at_store;
	void     *t_at_store;
	int       hasvalue_at_store;
	void     *value_at_store;
} cb13_stash;

static cb13_stash *g13         = NULL;
static cb13_stash *g13gen      = NULL;   // rooted garbage-generator closure
static int         g13_bad     = 0;    // number of wrong results
static int         g13_firstbad= -1;   // first wrong iteration index
static int         g13_firstgot= 0;    // what it returned there
static int         g13_firstexp= 0;    // what it should have returned
static int         g13_nullret = -1;   // first iteration where dyn_call gave NULL
static int         g13_drift   = -1;   // first iteration where the vclosure changed
static int         g13_done    = 0;    // iterations actually completed

// ---------------------------------------------------------------------------
// store: keep the raw vclosure* in a malloc'd struct.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	if( g13 == NULL ) g13 = (cb13_stash*)malloc(sizeof(cb13_stash));
	g13->magic             = CB13_MAGIC;
	g13->cb                = c;
	g13->fun_at_store      = c ? c->fun : NULL;
	g13->t_at_store        = c ? (void*)c->t : NULL;
	g13->hasvalue_at_store = c ? (int)c->hasValue : -1;
	g13->value_at_store    = c ? c->value : NULL;

	{
		const char *mode = getenv("CB13_ROOT");
		if( mode && strcmp(mode,"slot") == 0 ) {
			printf("[cb13] root   : hl_add_root(&stash->cb) slot=%p\n", (void*)&g13->cb);
			hl_add_root(&g13->cb);
		} else if( mode && strcmp(mode,"object") == 0 ) {
			printf("[cb13] root   : hl_add_root(closure) obj=%p\n", (void*)c);
			hl_add_root(c);
		}
	}
	printf("[cb13] store  : vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? c->value : NULL);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// store_gen: a second closure used only to drive allocation from inside the
// native loop. Always rooted the way that WORKS in ash (object pointer), so
// the only unrooted thing in the experiment is the target closure.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store_gen)( vclosure *c ) {
	if( g13gen == NULL ) g13gen = (cb13_stash*)malloc(sizeof(cb13_stash));
	g13gen->magic = CB13_MAGIC;
	g13gen->cb    = c;
	hl_add_root(c);
	printf("[cb13] gen    : vclosure=%p (rooted as object)\n", (void*)c);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store_gen, _FUN(_I32,_I32));

// Runs the generator closure k times. The TARGET closure is never named in
// this frame, so nothing here can conservatively retain it.
__attribute__((noinline)) static int cb13_generate( int k ) {
	int i, s = 0;
	if( g13gen == NULL || g13gen->cb == NULL ) return 0;
	for( i = 0; i < k; i++ ) {
		vdynamic  a;
		vdynamic *args[1];
		vdynamic *r;
		a.t = &hlt_i32; a.v.i = i; args[0] = &a;
		r = hl_dyn_call(g13gen->cb, args, 1);
		if( r ) s += r->v.i;
	}
	return s;
}

// Overwrites the stack region the dyn_call frames just used, so a stale copy
// of the target pointer left in a dead callee frame cannot mark it either.
__attribute__((noinline)) static void cb13_wipe( void ) {
	volatile long buf[768];
	int i;
	for( i = 0; i < 768; i++ ) buf[i] = 0;
}

// ---------------------------------------------------------------------------
// pump: the native loop. n calls, one per iteration, checked as we go.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(pump)( int n ) {
	int i;
	int sum     = 0;
	int use_hlp = getenv("CB13_HLP") != NULL;
	int verbose = getenv("CB13_VERBOSE") != NULL;
	int scrub   = getenv("CB13_SCRUB") != NULL;
	int gen_k   = getenv("CB13_GEN") ? atoi(getenv("CB13_GEN")) : 200;

	g13_bad = 0; g13_firstbad = -1; g13_nullret = -1; g13_drift = -1; g13_done = 0;

	if( g13 == NULL || g13->magic != CB13_MAGIC ) {
		printf("[cb13] pump   : nothing stored\n");
		fflush(stdout);
		return -1;
	}
	printf("[cb13] pump   : n=%d path=%s\n", n, use_hlp ? "hlp_dyn_call" : "hl_dyn_call");
	fflush(stdout);

	for( i = 0; i < n; i++ ) {
		vclosure * volatile c;
		vdynamic  a;
		vdynamic *args[1];
		vdynamic *ret;
		int       expect = i * 3 + 1 + CB13_BASE;
		int       got;

		if( scrub ) {
			cb13_generate(gen_k);
			cb13_wipe();
		}
		// Re-read the pointer out of the malloc'd struct every iteration,
		// exactly as a process_events() pump would.
		c = g13->cb;
		if( c == NULL ) {
			printf("[cb13] iter %d : stored closure became NULL\n", i);
			fflush(stdout);
			break;
		}
		if( (void*)c->fun != g13->fun_at_store || (void*)c->t != g13->t_at_store ) {
			if( g13_drift < 0 ) {
				g13_drift = i;
				printf("[cb13] iter %d : CLOSURE DRIFT t=%p (was %p) fun=%p (was %p) hasValue=%d (was %d) value=%p (was %p)\n",
					i, (void*)c->t, g13->t_at_store,
					(void*)c->fun, g13->fun_at_store,
					(int)c->hasValue, g13->hasvalue_at_store,
					c->value, g13->value_at_store);
				fflush(stdout);
			}
		}

		a.t     = &hlt_i32;
		a.v.i   = i;
		args[0] = &a;

		ret = use_hlp ? hlp_dyn_call((vclosure*)c, args, 1) : hl_dyn_call((vclosure*)c, args, 1);

		if( ret == NULL ) {
			if( g13_nullret < 0 ) {
				g13_nullret = i;
				printf("[cb13] iter %d : hl_dyn_call returned NULL\n", i);
				fflush(stdout);
			}
			g13_bad++;
			if( g13_firstbad < 0 ) { g13_firstbad = i; g13_firstgot = 0; g13_firstexp = expect; }
			continue;
		}
		got = ret->v.i;
		if( verbose )
			printf("[cb13] iter %d : ret=%p t=%p kind=%d v.i=%d (expect %d)\n",
				i, (void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, got, expect);
		if( got != expect ) {
			g13_bad++;
			if( g13_firstbad < 0 ) {
				g13_firstbad = i; g13_firstgot = got; g13_firstexp = expect;
				printf("[cb13] iter %d : WRONG got=%d expect=%d (ret=%p t=%p kind=%d)\n",
					i, got, expect, (void*)ret, (void*)ret->t,
					ret->t ? (int)ret->t->kind : -1);
				fflush(stdout);
			}
		}
		sum += got;
		g13_done = i + 1;
		c = NULL;   // volatile: really clears this frame's copy
	}

	printf("[cb13] pump   : completed %d/%d, bad=%d firstbad=%d (got %d expect %d) firstnull=%d drift=%d sum=%d\n",
		g13_done, n, g13_bad, g13_firstbad, g13_firstgot, g13_firstexp, g13_nullret, g13_drift, sum);
	fflush(stdout);
	return sum;
}
DEFINE_PRIM(_I32, pump, _I32);

HL_PRIM int HL_NAME(badcount)( void ) { return g13_bad; }
DEFINE_PRIM(_I32, badcount, _NO_ARG);

HL_PRIM int HL_NAME(firstbad)( void ) { return g13_firstbad; }
DEFINE_PRIM(_I32, firstbad, _NO_ARG);

HL_PRIM int HL_NAME(completed)( void ) { return g13_done; }
DEFINE_PRIM(_I32, completed, _NO_ARG);
