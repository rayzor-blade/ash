// cb11test.hdll -- CASE #11: a closure held ONLY by native code across an
// explicit gc_major().
//
// Shape (this is hxDatachannel's shape, minus the churn):
//   1. Haxe builds a closure inside a @:noinline helper and hands it to the
//      native library. The helper returns; NOTHING on the Haxe side -- no
//      local, no field, no static, no array -- still references it.
//   2. The native library keeps the raw vclosure* in a malloc'd struct.
//      It registers no GC root (CB11_ROOT can turn one on, see below).
//   3. Haxe calls hl.Gc.major() explicitly, several times.
//   4. Native code calls hl_dyn_call(c, args, 1).
//
// If the closure has no root, step 3 is entitled to reclaim it and step 4
// reads freed memory.
//
// Env switches (all default OFF; getenv("")-is-set, so clear with `env -u`):
//   CB11_ROOT=object   native calls hl_add_root(closure)        (ash spelling)
//   CB11_ROOT=slot     native calls hl_add_root(&stash->cb)     (upstream spelling)
//   CB11_HLP=1         route through hlp_dyn_call instead of hl_dyn_call
//   CB11_NATIVE_GC=1   invoke() runs hlp_gc_major() itself right before the
//                      call, i.e. a collection with only C holding the pointer

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb11test_##n
#include "hl.h"

// ash_std exports these; hl.h declares hl_gc_major but ash_std does NOT export
// that name, so a two-level HDLL must reach the hlp_ spelling directly.
HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );
HL_API void      hlp_gc_major( void );

// ---------------------------------------------------------------------------
// ROOTING PROBE. The closure test above cannot separate "ash rooted it" from
// "ash's block-granular sweep never reclaimed that block": both look like an
// intact object. So allocate GC memory from NATIVE code and keep the pointer
// ONLY in malloc'd memory -- no Haxe stack slot, no interpreter register
// array, no global. Nothing conservative can reach it. Then:
//   CB11_ROOT unset   -> must be reclaimed (proves reclamation bites here)
//   CB11_ROOT=object  -> must survive      (proves ash's hl_add_root works)
//   CB11_ROOT=slot    -> upstream's documented spelling; does it survive?
// The three answers together decide the prime suspect.
// ---------------------------------------------------------------------------
#define PROBE_N     16
#define PROBE_BYTES 4096
// Root a buffer that empirically lands in a block the sweep DOES reclaim
// (probe[0] shares a block with live data and is retained regardless, so it
// cannot tell rooted from merely-lucky).
#define PROBE_ROOTED 4
static void *g_probe[PROBE_N];
static int   g_probe_live = 0;

typedef struct {
	int       magic;
	vclosure *cb;            // Int -> Int
	// identity recorded at store time, so invoke() can tell whether the
	// object under the pointer changed while only C referenced it
	void     *t_at_store;
	void     *fun_at_store;
	void     *value_at_store;
	int       hasvalue_at_store;
	unsigned char bytes_at_store[sizeof(vclosure)];
	// CONTROL: a Haxe object dropped at exactly the same moment as the
	// closure, whose address is likewise held only here. If the collector
	// really reclaims unreachable memory in this region, THIS is what gets
	// poisoned/reused. If the control survives byte-intact too, then the
	// closure's survival proves nothing about rooting.
	void     *ctl;
	unsigned char ctl_at_store[32];
	int       has_ctl;
} cb11_stash;

#define CB11_MAGIC 0x0BADC0B1

static cb11_stash *g_cb11 = NULL;

static void cb11_dump( const char *tag, vclosure *c ) {
	printf("[cb11] %-8s: vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		tag,
		(void*)c,
		c ? (void*)c->t   : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? (void*)c->value : NULL);
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// store: keep the raw vclosure* in a malloc'd struct. No GC root by default.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g_cb11 == NULL ) g_cb11 = (cb11_stash*)malloc(sizeof(cb11_stash));
	memset(g_cb11, 0, sizeof(cb11_stash));
	g_cb11->magic          = CB11_MAGIC;
	g_cb11->cb             = c;
	g_cb11->t_at_store     = c ? (void*)c->t   : NULL;
	g_cb11->fun_at_store   = c ? (void*)c->fun : NULL;
	g_cb11->value_at_store = c ? (void*)c->value : NULL;
	g_cb11->hasvalue_at_store = c ? (int)c->hasValue : -1;
	if( c ) memcpy(g_cb11->bytes_at_store, (void*)c, sizeof(g_cb11->bytes_at_store));

	mode = getenv("CB11_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		// The spelling every real HDLL uses: hl_add_root takes the ADDRESS OF
		// THE SLOT that holds the pointer; upstream's collector dereferences
		// *slot on every cycle.
		printf("[cb11] root    : hl_add_root(&stash->cb) slot=%p -> holds %p\n",
			(void*)&g_cb11->cb, (void*)c);
		fflush(stdout);
		hl_add_root(&g_cb11->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb11] root    : hl_add_root(closure) = %p\n", (void*)c);
		fflush(stdout);
		hl_add_root(c);
	} else {
		printf("[cb11] root    : none (unrooted, like hxDatachannel)\n");
		fflush(stdout);
	}
	cb11_dump("store", c);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// store_ctl: same deal for a plain object -- the control for "does anything
// in this region actually get reclaimed?"
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store_ctl)( vdynamic *o ) {
	if( g_cb11 == NULL ) { printf("[cb11] ctl     : no stash yet\n"); fflush(stdout); return; }
	g_cb11->ctl = (void*)o;
	g_cb11->has_ctl = o != NULL;
	if( o ) memcpy(g_cb11->ctl_at_store, (void*)o, sizeof(g_cb11->ctl_at_store));
	printf("[cb11] ctl     : control object=%p t=%p\n", (void*)o, o ? (void*)o->t : NULL);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store_ctl, _DYN);

// ---------------------------------------------------------------------------
// invoke: call the stored closure with one Int through hl_dyn_call.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;
	int changed = 0;

	if( g_cb11 == NULL || g_cb11->magic != CB11_MAGIC ) {
		printf("[cb11] invoke  : nothing stored\n");
		fflush(stdout);
		return -1;
	}
	c = g_cb11->cb;
	if( getenv("CB11_NATIVE_GC") ) {
		printf("[cb11] gc      : hlp_gc_major() from native, only C holds %p\n", (void*)c);
		fflush(stdout);
		hlp_gc_major();
	}

	cb11_dump("invoke", c);
	if( c == NULL ) { printf("[cb11] result  : stored pointer is NULL\n"); fflush(stdout); return -2; }

	if( (void*)c->t   != g_cb11->t_at_store )   { printf("[cb11] CHANGED : t   %p -> %p\n", g_cb11->t_at_store,   (void*)c->t);   changed = 1; }
	if( (void*)c->fun != g_cb11->fun_at_store ) { printf("[cb11] CHANGED : fun %p -> %p\n", g_cb11->fun_at_store, (void*)c->fun); changed = 1; }
	if( (void*)c->value != g_cb11->value_at_store ) { printf("[cb11] CHANGED : value %p -> %p\n", g_cb11->value_at_store, (void*)c->value); changed = 1; }
	if( (int)c->hasValue != g_cb11->hasvalue_at_store ) { printf("[cb11] CHANGED : hasValue %d -> %d\n", g_cb11->hasvalue_at_store, (int)c->hasValue); changed = 1; }
	if( memcmp(g_cb11->bytes_at_store, (void*)c, sizeof(g_cb11->bytes_at_store)) != 0 ) {
		printf("[cb11] CHANGED : first %d bytes of the vclosure differ from store time\n",
			(int)sizeof(g_cb11->bytes_at_store));
		changed = 1;
	}
	if( !changed ) { printf("[cb11] identity: unchanged since store\n"); }

	if( g_cb11->has_ctl ) {
		unsigned char *p = (unsigned char*)g_cb11->ctl;
		int i, poison = 0, diff = 0;
		for( i = 0; i < (int)sizeof(g_cb11->ctl_at_store); i++ ) {
			if( p[i] == 0xA5 ) poison++;
			if( p[i] != g_cb11->ctl_at_store[i] ) diff++;
		}
		printf("[cb11] control : obj=%p poisoned_bytes=%d/%d changed_bytes=%d/%d\n",
			g_cb11->ctl, poison, (int)sizeof(g_cb11->ctl_at_store),
			diff, (int)sizeof(g_cb11->ctl_at_store));
		printf("[cb11] control : now ");
		for( i = 0; i < 16; i++ ) printf("%02x", p[i]);
		printf("  was ");
		for( i = 0; i < 16; i++ ) printf("%02x", g_cb11->ctl_at_store[i]);
		printf("\n");
	}
	fflush(stdout);

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB11_HLP") ) {
		printf("[cb11] path    : hlp_dyn_call arg=%d\n", arg); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cb11] path    : hl_dyn_call arg=%d\n", arg); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) {
		printf("[cb11] result  : dyn_call returned NULL\n");
		fflush(stdout);
		return -3;
	}
	printf("[cb11] result  : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32);

// ---------------------------------------------------------------------------
// probe_alloc: GC memory reachable ONLY from a C static array.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(probe_alloc)( void ) {
	const char *mode = getenv("CB11_ROOT");
	int i;
	for( i = 0; i < PROBE_N; i++ ) {
		unsigned char *p = (unsigned char*)hl_gc_alloc_gen(&hlt_bytes, PROBE_BYTES, 0);
		int j;
		g_probe[i] = (void*)p;
		if( p == NULL ) { printf("[cb11] probe   : alloc %d FAILED\n", i); fflush(stdout); continue; }
		// leave the first 8 bytes (the type word hl_gc_alloc_gen wrote) alone
		for( j = 8; j < PROBE_BYTES; j++ ) p[j] = (unsigned char)(0x5A ^ (i + j));
	}
	g_probe_live = 1;
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb11] probe   : hl_add_root(&g_probe[%d]) slot=%p -> holds %p\n",
			PROBE_ROOTED, (void*)&g_probe[PROBE_ROOTED], g_probe[PROBE_ROOTED]);
		hl_add_root(&g_probe[PROBE_ROOTED]);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb11] probe   : hl_add_root(g_probe[%d]) = %p\n",
			PROBE_ROOTED, g_probe[PROBE_ROOTED]);
		hl_add_root(g_probe[PROBE_ROOTED]);
	} else {
		printf("[cb11] probe   : unrooted\n");
	}
	printf("[cb11] probe   : %d x %d bytes at %p .. %p\n",
		PROBE_N, PROBE_BYTES, g_probe[0], g_probe[PROBE_N-1]);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, probe_alloc, _NO_ARG);

// ---------------------------------------------------------------------------
// probe_check: did the collector reclaim / poison / reuse those buffers?
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(probe_check)( void ) {
	int i;
	if( !g_probe_live ) { printf("[cb11] probe   : none\n"); fflush(stdout); return; }
	for( i = 0; i < PROBE_N; i++ ) {
		unsigned char *p = (unsigned char*)g_probe[i];
		int j, bad = 0, poison = 0, zero = 0;
		if( p == NULL ) continue;
		for( j = 8; j < PROBE_BYTES; j++ ) {
			unsigned char want = (unsigned char)(0x5A ^ (i + j));
			if( p[j] != want ) bad++;
			if( p[j] == 0xA5 ) poison++;
			if( p[j] == 0x00 ) zero++;
		}
		printf("[cb11] probe[%2d]: %p intact=%s corrupt=%d poison=%d zero=%d\n",
			i, (void*)p, bad ? "NO" : "yes", bad, poison, zero);
	}
	fflush(stdout);
}
DEFINE_PRIM(_VOID, probe_check, _NO_ARG);
