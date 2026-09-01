// cb9test.hdll -- CASE #9: an anonymous lambda capturing locals, stored raw in
// a malloc'd C struct and later invoked through hl_dyn_call.
//
// A capturing lambda is a vclosure with hasValue=1 whose `value` field points
// at the captured environment. Two things must survive between store and
// invoke: the vclosure itself AND that environment. This library holds the
// vclosure* only, exactly like hxDatachannel, and prints the closure's fun /
// hasValue / value at store time and again at invoke time so a swapped or
// reclaimed environment is visible in the trace rather than only in the
// arithmetic.
//
// Diagnostic env switches (all default off; use `env -u VAR` to clear -- an
// empty value still counts as set to getenv):
//   CB9_HLP=1        call hlp_dyn_call instead of hl_dyn_call
//   CB9_ROOT=object  hl_add_root(closure)          -- ash's object spelling
//   CB9_ROOT=slot    hl_add_root(&stash->cb)       -- upstream HashLink
//                    spelling: the ADDRESS OF THE SLOT holding the pointer
//   CB9_ROOT=value   hl_add_root(closure) AND hl_add_root(closure->value)
//                    -- root the captured environment explicitly too
//   CB9_DUMP=1       hexdump the first 32 bytes of the closure

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb9test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );
// ash's heap census (upstream's hl_gc_dump_memory): marks, then writes
// heap-base/heap-size, the root-set sizes and a per-block live-line census.
// Used here to prove, from inside the process, whether an address handed to
// hl_add_root can ever be marked at all.
HL_API void hlp_gc_dump_memory( vbyte *filename );

typedef struct {
	int       magic;
	vclosure *cb;              // Int -> Int, capturing lambda
	void     *fun_at_store;
	void     *value_at_store;  // the captured environment
	void     *t_at_store;
	int       hasValue_at_store;
	// Control arm: a NON-capturing static closure handed over in the same
	// call, so one run compares "capturing lambda" against "plain function"
	// on one GC timeline.
	vclosure *plain;
	void     *plain_fun_at_store;
	void     *plain_t_at_store;
	int       plain_hasValue_at_store;
} cb9_stash;

#define CB9_MAGIC 0x0B9EC0DE

static cb9_stash *g9 = NULL;

static void cb9_dump( const char *tag, void *p ) {
	unsigned char *b;
	int i;
	if( !getenv("CB9_DUMP") ) return;
	if( p == NULL ) { printf("[cb9] %-8s: NULL\n", tag); fflush(stdout); return; }
	b = (unsigned char*)p;
	printf("[cb9] %-8s: %p bytes:", tag, p);
	for( i = 0; i < 32; i++ ) printf(" %02x", b[i]);
	printf("\n");
	fflush(stdout);
}

static void cb9_report( const char *tag, vclosure *c ) {
	printf("[cb9] %-8s: vclosure=%p t=%p fun=%p hasValue=%d value=%p\n",
		tag,
		(void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1,
		c ? (void*)c->value : NULL);
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// store: keep the raw vclosure*, no GC root by default.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store_plain)( vclosure *p ) {
	if( g9 == NULL || g9->magic != CB9_MAGIC ) { printf("[cb9] storeP  : store() first\n"); fflush(stdout); return; }
	g9->plain = p;
	g9->plain_fun_at_store = p ? p->fun : NULL;
	g9->plain_t_at_store = p ? (void*)p->t : NULL;
	g9->plain_hasValue_at_store = p ? (int)p->hasValue : -1;
	cb9_report("storeP", p);
	if( getenv("CB9_ROOT") && strcmp(getenv("CB9_ROOT"),"object") == 0 ) hl_add_root(p);
	if( getenv("CB9_ROOT") && strcmp(getenv("CB9_ROOT"),"slot") == 0 ) hl_add_root(&g9->plain);
}
DEFINE_PRIM(_VOID, store_plain, _FUN(_I32,_I32));

HL_PRIM int HL_NAME(invoke_plain)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;
	if( g9 == NULL || g9->magic != CB9_MAGIC || g9->plain == NULL ) { printf("[cb9] invokeP : nothing stored\n"); fflush(stdout); return -1; }
	c = g9->plain;
	cb9_report("invokeP", c);
	printf("[cb9] cmpP    : fun %s (store=%p)  t %s (store=%p)  hasValue %d (store=%d)\n",
		(c->fun == g9->plain_fun_at_store) ? "SAME" : "CHANGED", g9->plain_fun_at_store,
		((void*)c->t == g9->plain_t_at_store) ? "SAME" : "CHANGED", g9->plain_t_at_store,
		(int)c->hasValue, g9->plain_hasValue_at_store);
	fflush(stdout);
	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;
	ret = getenv("CB9_HLP") ? hlp_dyn_call(c, args, 1) : hl_dyn_call(c, args, 1);
	if( ret == NULL ) { printf("[cb9] resultP : NULL\n"); fflush(stdout); return -3; }
	printf("[cb9] resultP : v.i=%d\n", ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke_plain, _I32);

HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g9 == NULL ) g9 = (cb9_stash*)malloc(sizeof(cb9_stash));
	memset(g9, 0, sizeof(cb9_stash));
	g9->magic = CB9_MAGIC;
	g9->cb = c;
	g9->fun_at_store = c ? c->fun : NULL;
	g9->value_at_store = c ? c->value : NULL;
	g9->t_at_store = c ? (void*)c->t : NULL;
	g9->hasValue_at_store = c ? (int)c->hasValue : -1;

	mode = getenv("CB9_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb9] root    : hl_add_root(&stash->cb) = %p  [upstream slot spelling]\n", (void*)&g9->cb);
		fflush(stdout);
		hl_add_root(&g9->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb9] root    : hl_add_root(closure) = %p  [ash object spelling]\n", (void*)c);
		fflush(stdout);
		hl_add_root(c);
	} else if( mode && strcmp(mode,"value") == 0 ) {
		printf("[cb9] root    : hl_add_root(closure)=%p + hl_add_root(env)=%p\n",
			(void*)c, c ? (void*)c->value : NULL);
		fflush(stdout);
		hl_add_root(c);
		if( c && c->value ) hl_add_root(c->value);
	}

	cb9_report("store", c);
	cb9_dump("storeb", (void*)c);
	if( c && c->hasValue && c->value ) cb9_dump("envb", c->value);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// peek: report the stored closure without calling it.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(peek)( void ) {
	if( g9 == NULL || g9->magic != CB9_MAGIC ) { printf("[cb9] peek    : nothing stored\n"); fflush(stdout); return; }
	cb9_report("peek", g9->cb);
	printf("[cb9] peekcmp : fun %s  value %s  t %s\n",
		(g9->cb && g9->cb->fun == g9->fun_at_store) ? "SAME" : "CHANGED",
		(g9->cb && g9->cb->value == g9->value_at_store) ? "SAME" : "CHANGED",
		(g9->cb && (void*)g9->cb->t == g9->t_at_store) ? "SAME" : "CHANGED");
	fflush(stdout);
	if( g9->cb && g9->cb->hasValue && g9->cb->value ) cb9_dump("envb", g9->cb->value);
}
DEFINE_PRIM(_VOID, peek, _NO_ARG);

// ---------------------------------------------------------------------------
// invoke: hl_dyn_call(c, { arg }, 1)
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic  a;
	vdynamic *args[1];
	vdynamic *ret;

	if( g9 == NULL || g9->magic != CB9_MAGIC ) { printf("[cb9] invoke  : nothing stored\n"); fflush(stdout); return -1; }
	c = g9->cb;
	printf("[cb9] invoke  : arg=%d\n", arg); fflush(stdout);
	cb9_report("invoke", c);
	printf("[cb9] cmp     : fun %s (store=%p)  value %s (store=%p)\n",
		(c && c->fun == g9->fun_at_store) ? "SAME" : "CHANGED", g9->fun_at_store,
		(c && c->value == g9->value_at_store) ? "SAME" : "CHANGED", g9->value_at_store);
	fflush(stdout);
	if( c == NULL ) return -2;
	if( c->hasValue && c->value ) cb9_dump("envb", c->value);

	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;

	if( getenv("CB9_HLP") ) {
		printf("[cb9] path    : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, 1);
	} else {
		printf("[cb9] path    : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, 1);
	}

	if( ret == NULL ) { printf("[cb9] result  : hl_dyn_call returned NULL\n"); fflush(stdout); return -3; }
	printf("[cb9] result  : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _I32);

// ---------------------------------------------------------------------------
// dump: heap census, plus the addresses this library is holding, so the two
// hl_add_root spellings can be compared against the heap bounds.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(dump)( vbyte *path ) {
	printf("[cb9] dump    : file=%s closure=%p env=%p slot(&stash->cb)=%p\n",
		(char*)path,
		g9 ? (void*)g9->cb : NULL,
		(g9 && g9->cb) ? (void*)g9->cb->value : NULL,
		g9 ? (void*)&g9->cb : NULL);
	fflush(stdout);
	hlp_gc_dump_memory(path);
}
DEFINE_PRIM(_VOID, dump, _BYTES);
