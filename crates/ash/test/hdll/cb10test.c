// cb10test.hdll -- CASE #10: a closure STORED by native code, then CALLED
// after heavy Haxe allocation.  This is the rooting shape from hxDatachannel:
//
//   1. Haxe hands a closure to the native library.
//   2. The library copies the raw vclosure* into a malloc'd struct.  malloc'd
//      memory is OFF-HEAP and is never scanned by the collector, so this
//      struct is not a GC root by any mechanism.  hxDatachannel registers no
//      root either -- it simply relies on the pointer staying valid.
//   3. Haxe drops every reference to the closure and allocates heavily, so a
//      collection happens BETWEEN store and call.
//   4. The library invokes the stored closure through hl_dyn_call().
//
// Three closure kinds are stored in three slots, because they have different
// reachability requirements:
//   slot 0  static closure      hasValue=0, only the vclosure must survive
//   slot 1  bound instance      hasValue=1, vclosure AND the receiver object
//   slot 2  capturing lambda    hasValue=1, vclosure AND the captured env
//
// Diagnostic env switches (all default off; use `env -u VAR` to clear one --
// an empty value still reads as set to getenv):
//   CB10_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB10_ROOT=object  hl_add_root(closure)            -- ash's object spelling
//   CB10_ROOT=slot    hl_add_root(&slot->cb)          -- upstream HashLink
//                     spelling: the ADDRESS OF THE SLOT holding the pointer,
//                     which the collector dereferences each cycle
//   CB10_ROOT=deep    hl_add_root(closure) AND hl_add_root(closure->value)
//   CB10_MAJOR=1      force a collection from native code between store and
//                     call (hlp_gc_major) instead of only churning
//   CB10_DUMP=1       hexdump the first 32 bytes of the closure / its value

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb10test_##n
#include "hl.h"

// ash_std's own spellings.  hl_dyn_call is what every real HDLL calls;
// hlp_dyn_call is the same function under ash's internal name, used here only
// to A/B the ABI shim against ONE binary.  hl_gc_major is DECLARED in hl.h but
// ash's libhl.dylib exports only hlp_gc_major, so use that name.
HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );
HL_API void hlp_gc_major( void );

#define CB10_MAGIC 0x0CA10DE1
#define CB10_N     3

typedef struct {
	int       magic;
	int       used;
	vclosure *cb;
	void     *fun_at_store;
	void     *value_at_store;
	void     *t_at_store;
	int       hasValue_at_store;
} cb10_slot;

// deliberately malloc'd: off-heap, never scanned, exactly like hxDatachannel's
// per-connection struct.
static cb10_slot *g10 = NULL;

static const char *kind_name( int i ) {
	switch( i ) {
	case 0: return "static";
	case 1: return "bound";
	case 2: return "capture";
	}
	return "?";
}

static void cb10_dump( const char *tag, void *p ) {
	unsigned char *b;
	int i;
	if( !getenv("CB10_DUMP") ) return;
	if( p == NULL ) { printf("[cb10] %-10s: NULL\n", tag); fflush(stdout); return; }
	b = (unsigned char*)p;
	printf("[cb10] %-10s: %p bytes:", tag, p);
	for( i = 0; i < 32; i++ ) printf(" %02x", b[i]);
	printf("\n");
	fflush(stdout);
}

static void cb10_report( const char *tag, int idx, vclosure *c ) {
	printf("[cb10] %-10s: slot %d (%s) vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		tag, idx, kind_name(idx),
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? (void*)c->value : NULL);
	fflush(stdout);
}

static void cb10_ensure( void ) {
	if( g10 == NULL ) {
		g10 = (cb10_slot*)malloc(sizeof(cb10_slot) * CB10_N);
		memset(g10, 0, sizeof(cb10_slot) * CB10_N);
	}
}

// ---------------------------------------------------------------------------
// store(idx, closure): keep the raw vclosure*.  No GC root unless CB10_ROOT.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( int idx, vclosure *c ) {
	const char *mode;
	cb10_slot *s;
	if( idx < 0 || idx >= CB10_N ) return;
	cb10_ensure();
	s = &g10[idx];
	memset(s, 0, sizeof(cb10_slot));
	s->magic = CB10_MAGIC;
	s->used = 1;
	s->cb = c;
	s->fun_at_store      = c ? c->fun : NULL;
	s->value_at_store    = c ? c->value : NULL;
	s->t_at_store        = c ? (void*)c->t : NULL;
	s->hasValue_at_store = c ? (int)c->hasValue : -1;

	mode = getenv("CB10_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb10] root      : slot %d hl_add_root(&slot->cb) = %p  [upstream slot spelling]\n",
			idx, (void*)&s->cb);
		fflush(stdout);
		hl_add_root(&s->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb10] root      : slot %d hl_add_root(closure) = %p  [ash object spelling]\n",
			idx, (void*)c);
		fflush(stdout);
		hl_add_root(c);
	} else if( mode && strcmp(mode,"deep") == 0 ) {
		printf("[cb10] root      : slot %d hl_add_root(closure)=%p + hl_add_root(value)=%p\n",
			idx, (void*)c, c ? (void*)c->value : NULL);
		fflush(stdout);
		hl_add_root(c);
		if( c && c->value ) hl_add_root(c->value);
	}

	cb10_report("store", idx, c);
	cb10_dump("store.cl", (void*)c);
	if( c && c->hasValue && c->value ) cb10_dump("store.val", c->value);
}
DEFINE_PRIM(_VOID, store, _I32 _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// collect(): force a collection from NATIVE code, i.e. from inside an HDLL
// call, which is where hxDatachannel's pump runs.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(collect)( void ) {
	if( !getenv("CB10_MAJOR") ) {
		printf("[cb10] collect   : skipped (set CB10_MAJOR=1 to force)\n");
		fflush(stdout);
		return;
	}
	printf("[cb10] collect   : hlp_gc_major()\n"); fflush(stdout);
	hlp_gc_major();
	printf("[cb10] collect   : done\n"); fflush(stdout);
}
DEFINE_PRIM(_VOID, collect, _NO_ARG);

// ---------------------------------------------------------------------------
// peek(idx): report the stored closure WITHOUT calling it, so a reclaimed or
// overwritten closure shows up in the trace and not only in the arithmetic.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(peek)( int idx ) {
	cb10_slot *s;
	if( idx < 0 || idx >= CB10_N || g10 == NULL || g10[idx].magic != CB10_MAGIC ) {
		printf("[cb10] peek      : slot %d empty\n", idx); fflush(stdout); return;
	}
	s = &g10[idx];
	cb10_report("peek", idx, s->cb);
	printf("[cb10] peek.cmp  : slot %d fun %s  value %s  t %s  hasValue %s\n", idx,
		(s->cb && s->cb->fun   == s->fun_at_store)      ? "SAME" : "CHANGED",
		(s->cb && s->cb->value == s->value_at_store)    ? "SAME" : "CHANGED",
		(s->cb && (void*)s->cb->t == s->t_at_store)     ? "SAME" : "CHANGED",
		(s->cb && (int)s->cb->hasValue == s->hasValue_at_store) ? "SAME" : "CHANGED");
	fflush(stdout);
	if( s->cb ) cb10_dump("peek.cl", (void*)s->cb);
	if( s->cb && s->cb->hasValue && s->cb->value ) cb10_dump("peek.val", s->cb->value);
}
DEFINE_PRIM(_VOID, peek, _I32);

// ---------------------------------------------------------------------------
// invoke(idx, arg): hl_dyn_call(stored, { arg }, 1)
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( int idx, int arg ) {
	cb10_slot *s;
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;

	if( idx < 0 || idx >= CB10_N || g10 == NULL || g10[idx].magic != CB10_MAGIC ) {
		printf("[cb10] invoke    : slot %d empty\n", idx); fflush(stdout); return -1;
	}
	s = &g10[idx];
	c = s->cb;
	printf("[cb10] invoke    : slot %d (%s) arg=%d\n", idx, kind_name(idx), arg);
	fflush(stdout);
	cb10_report("invoke", idx, c);
	printf("[cb10] cmp       : slot %d fun %s (store=%p)  value %s (store=%p)\n", idx,
		(c && c->fun   == s->fun_at_store)   ? "SAME" : "CHANGED", s->fun_at_store,
		(c && c->value == s->value_at_store) ? "SAME" : "CHANGED", s->value_at_store);
	fflush(stdout);
	if( c == NULL ) return -2;
	if( c->hasValue && c->value ) cb10_dump("inv.val", c->value);

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB10_HLP") ) {
		printf("[cb10] path      : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cb10] path      : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) {
		printf("[cb10] result    : slot %d hl_dyn_call returned NULL\n", idx);
		fflush(stdout);
		return -3;
	}
	printf("[cb10] result    : slot %d vdynamic=%p t=%p kind=%d v.i=%d\n",
		idx, (void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32 _I32);
