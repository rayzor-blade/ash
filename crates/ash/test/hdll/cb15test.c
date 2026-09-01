// CASE 15 -- closure passed, native returns, Haxe DROPS its reference,
//            native calls it later.  The exact use-after-free candidate.
//
//   1. Haxe builds a closure inside a helper function and hands it to
//      cb15test.store().  The native side keeps the raw vclosure* in a
//      malloc'd C struct -- memory the collector never scans.
//   2. store() returns; the helper returns; Haxe holds NO reference at all.
//      The Haxe program then churns the heap and clobbers its own stack with
//      deep recursion so no stale conservative root can keep the closure
//      alive by accident.
//   3. cb15test.invoke(x) calls hl_dyn_call(c, args, 1) on the stored pointer.
//
// Diagnostics: the first 48 bytes of the vclosure are snapshotted at store
// time and compared at invoke time, so a reclaimed-and-reused (or zeroed, or
// poisoned) block is reported as corruption rather than only as a segfault.
//
// Env switches (all default off; getenv("")==set, so use `env -u`):
//   CB15_ROOT=object   hl_add_root(closure)      -- ash's current expectation
//   CB15_ROOT=slot     hl_add_root(&stash->cb)   -- upstream HashLink spelling
//   CB15_HLP=1         call hlp_dyn_call instead of hl_dyn_call

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb15test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );
// NOTE: ash's libhl.dylib does NOT export hl_gc_major (nor hl_gc_enable /
// hl_gc_stats / hl_gc_get_flags); only hl_gc_alloc_gen carries the hl_
// prefix. A stock HashLink HDLL that calls hl_gc_major fails to link here.
// Use ash's own spelling for this diagnostic.
HL_API void hlp_gc_major( void );

#define CB15_MAGIC 0x15AFE15A
#define SNAP_BYTES 32  /* sizeof(vclosure) on HL_64: t,fun,hasValue+stackCount,value */

typedef struct {
	int        magic;
	vclosure  *cb;
	hl_type   *t_at_store;
	void      *fun_at_store;
	int        hasValue_at_store;
	void      *value_at_store;
	unsigned char snap[SNAP_BYTES];
	int        rooted;          // 0 none, 1 object, 2 slot
	int        corrupt;         // set by invoke()
	// CONTROL: a GC object allocated by the NATIVE side and referenced only
	// from this malloc'd struct -- exactly the reachability the stored
	// closure has. If the decoy is reclaimed but the closure is not, the
	// closure has a real root; if neither is reclaimed, nothing in this
	// heap shape can be collected and the test proves nothing.
	unsigned char *decoy;
	unsigned char  decoy_snap[SNAP_BYTES];
	// snapshot of the closure's captured environment (c->value), which the
	// collector can only keep alive by TRACING through the closure
	unsigned char  env_snap[SNAP_BYTES];
	int            have_env;
} cb15_stash;

static cb15_stash *g_s = NULL;

// CONTROL 2: a plain HAXE-allocated object handed across the same native
// boundary and then dropped by Haxe. If this dies while the closure lives,
// the closure has a closure-specific root; if it also lives, everything that
// crosses a native call is being pinned.
static void         *g_obj      = NULL;
static unsigned char g_obj_snap[SNAP_BYTES];
static int           g_have_obj = 0;

static void dump_bytes( const char *label, const unsigned char *p ) {
	int i;
	printf("[cb15] %s:", label);
	for( i = 0; i < SNAP_BYTES; i++ ) {
		if( (i & 7) == 0 ) printf(" ");
		printf("%02x", p[i]);
	}
	printf("\n");
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// store: keep the raw vclosure*, register no root by default.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g_s == NULL ) g_s = (cb15_stash*)malloc(sizeof(cb15_stash));
	memset(g_s, 0, sizeof(cb15_stash));
	g_s->magic = CB15_MAGIC;
	g_s->cb = c;
	if( c != NULL ) {
		g_s->t_at_store        = c->t;
		g_s->fun_at_store      = c->fun;
		g_s->hasValue_at_store = c->hasValue;
		g_s->value_at_store    = c->value;
		memcpy(g_s->snap, (const unsigned char*)c, SNAP_BYTES);
	}

	if( c != NULL && c->value != NULL ) {
		memcpy(g_s->env_snap, (const unsigned char*)c->value, SNAP_BYTES);
		g_s->have_env = 1;
	}

	// allocate the control decoy and keep it ONLY here
	g_s->decoy = (unsigned char*)hl_gc_alloc_gen(&hlt_bytes, SNAP_BYTES, 0);
	if( g_s->decoy != NULL ) {
		int i;
		for( i = 0; i < SNAP_BYTES; i++ ) g_s->decoy[i] = (unsigned char)(0x40 + i);
		memcpy(g_s->decoy_snap, g_s->decoy, SNAP_BYTES);
		printf("[cb15] decoy  : native-only GC alloc at %p\n", (void*)g_s->decoy);
		fflush(stdout);
	}

	mode = getenv("CB15_ROOT");
	if( mode != NULL && strcmp(mode, "object") == 0 ) {
		g_s->rooted = 1;
		printf("[cb15] root   : hl_add_root(closure)=%p AND hl_add_root(decoy)=%p\n",
			(void*)c, (void*)g_s->decoy);
		fflush(stdout);
		hl_add_root(c);
		hl_add_root(g_s->decoy);      // decoy is PROVABLY collectible unrooted
	} else if( mode != NULL && strcmp(mode, "slot") == 0 ) {
		g_s->rooted = 2;
		printf("[cb15] root   : hl_add_root(&stash->cb)=%p AND hl_add_root(&stash->decoy)=%p  (upstream spelling)\n",
			(void*)&g_s->cb, (void*)&g_s->decoy);
		fflush(stdout);
		hl_add_root(&g_s->cb);
		hl_add_root(&g_s->decoy);
	} else {
		printf("[cb15] root   : none (hxDatachannel behaviour)\n");
		fflush(stdout);
	}

	printf("[cb15] store  : vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? c->value : NULL);
	fflush(stdout);
	if( c != NULL ) dump_bytes("snap-@store ", g_s->snap);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

HL_PRIM void HL_NAME(storeobj)( vdynamic *o ) {
	g_obj = o;
	if( o != NULL ) {
		memcpy(g_obj_snap, (const unsigned char*)o, SNAP_BYTES);
		g_have_obj = 1;
		printf("[cb15] storeobj: haxe object=%p (dropped by Haxe after this)\n", (void*)o);
		fflush(stdout);
	}
}
DEFINE_PRIM(_VOID, storeobj, _DYN);

// ---------------------------------------------------------------------------
// invoke: call the stored closure long after Haxe dropped its reference.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;
	unsigned char now[SNAP_BYTES];

	if( g_s == NULL || g_s->magic != CB15_MAGIC ) {
		printf("[cb15] invoke : nothing stored\n");
		fflush(stdout);
		return -1;
	}
	c = g_s->cb;
	if( c == NULL ) {
		printf("[cb15] invoke : stored closure is NULL\n");
		fflush(stdout);
		return -2;
	}

	memcpy(now, (const unsigned char*)c, SNAP_BYTES);
	printf("[cb15] invoke : vclosure=%p t=%p(was %p) fun=%p(was %p) hasValue=%d(was %d) value=%p(was %p) arg=%d\n",
		(void*)c, (void*)c->t, (void*)g_s->t_at_store,
		(void*)c->fun, g_s->fun_at_store,
		(int)c->hasValue, g_s->hasValue_at_store,
		c->value, g_s->value_at_store, arg);
	fflush(stdout);
	dump_bytes("snap-@invoke", now);

	if( memcmp(now, g_s->snap, SNAP_BYTES) != 0 ) {
		g_s->corrupt = 1;
		printf("[cb15] CORRUPT: the stored vclosure's bytes changed between store and invoke\n");
		fflush(stdout);
	} else {
		printf("[cb15] intact : vclosure bytes unchanged since store\n");
		fflush(stdout);
	}

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB15_HLP") ) {
		printf("[cb15] path   : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cb15] path   : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) {
		printf("[cb15] result : hl_dyn_call returned NULL\n");
		fflush(stdout);
		return -3;
	}
	printf("[cb15] result : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32);

// 1 if the stored vclosure's raw bytes changed between store and invoke.
HL_PRIM int HL_NAME(corrupt)() {
	return (g_s != NULL && g_s->magic == CB15_MAGIC) ? g_s->corrupt : -1;
}
DEFINE_PRIM(_I32, corrupt, _NO_ARG);

// Force a full collection from the native side, at the exact moment Haxe
// holds no reference to the stored closure. This removes any dependence on
// ASH_GC_STRESS actually firing inside the mutator loop.
HL_PRIM void HL_NAME(gcmajor)() {
	void *probe;
	printf("[cb15] gcmajor: calling hlp_gc_major() with cb=%p\n",
		g_s ? (void*)g_s->cb : NULL);
	fflush(stdout);
	hlp_gc_major();
	// allocate from the native side afterwards: if this lands in the same
	// address range as the closure, both sides share one heap.
	probe = hl_gc_alloc_gen(&hlt_bytes, 64, 0);
	printf("[cb15] gcmajor: done; native probe alloc=%p (closure=%p)\n",
		probe, g_s ? (void*)g_s->cb : NULL);
	fflush(stdout);
	if( g_s != NULL && g_s->cb != NULL ) {
		unsigned char now[SNAP_BYTES];
		memcpy(now, (const unsigned char*)g_s->cb, SNAP_BYTES);
		dump_bytes("snap-@gcmajor", now);
		printf("[cb15] gcmajor: closure bytes %s since store\n",
			memcmp(now, g_s->snap, SNAP_BYTES) == 0 ? "UNCHANGED" : "CHANGED");
		fflush(stdout);
	}
	if( g_s != NULL && g_s->have_env && g_s->cb != NULL && g_s->cb->value != NULL ) {
		printf("[cb15] gcmajor: captured ENV at %p bytes %s\n",
			g_s->cb->value,
			memcmp(g_s->cb->value, g_s->env_snap, SNAP_BYTES) == 0 ? "UNCHANGED" : "CHANGED");
		fflush(stdout);
	}
	if( g_have_obj && g_obj != NULL ) {
		printf("[cb15] gcmajor: HAXE-OBJ(control2) at %p bytes %s\n", g_obj,
			memcmp(g_obj, g_obj_snap, SNAP_BYTES) == 0 ? "UNCHANGED" : "CHANGED");
		fflush(stdout);
	}
	if( g_s != NULL && g_s->decoy != NULL ) {
		printf("[cb15] gcmajor: DECOY(control) bytes %s  first8=%02x%02x%02x%02x%02x%02x%02x%02x\n",
			memcmp(g_s->decoy, g_s->decoy_snap, SNAP_BYTES) == 0 ? "UNCHANGED" : "CHANGED",
			g_s->decoy[0], g_s->decoy[1], g_s->decoy[2], g_s->decoy[3],
			g_s->decoy[4], g_s->decoy[5], g_s->decoy[6], g_s->decoy[7]);
		fflush(stdout);
	}
}
DEFINE_PRIM(_VOID, gcmajor, _NO_ARG);
