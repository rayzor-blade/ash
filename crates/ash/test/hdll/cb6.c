// cb6.hdll -- CASE #6 of the hl_dyn_call HDLL callback matrix: the callback
// RETURNS A STRING.
//
// Why this case matters: the return value is heap-allocated INSIDE the callee
// (Haxe builds a fresh String), and then crosses back out to native code that
// holds it in plain C memory the collector knows nothing about.
//
//   1. Haxe hands native a closure   Int -> String
//   2. native stashes the raw vclosure* in a malloc'd struct (no GC root,
//      exactly like hxDatachannel)
//   3. native later calls hl_dyn_call(c, args, 1) and inspects/returns the
//      String object it gets back
//
// Diagnostic switches (all default off; use `env -u VAR` to clear -- an empty
// value still counts as set):
//   CB6_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB6_ROOT=slot    hl_add_root(&stash->cb)  -- upstream HashLink spelling
//   CB6_ROOT=object  hl_add_root(closure)     -- what ash's hl_add_root expects
//   CB6_ROOTRET=1    hl_add_root() the returned String in hold_check
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb6_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

// HL's String object: { hl_type *t; vbyte *bytes; int length; }
typedef struct {
	hl_type *t;
	uchar   *bytes;
	int      length;
} hstring;

typedef struct {
	int       magic;
	vclosure *cb;
	void     *fun_at_store;
	vdynamic *ret;      // a String the callee returned, parked in malloc'd memory
	char     *ret_snap; // its text, copied into plain C memory
} stash6;

#define STASH6_MAGIC 0x5A6E0006
static stash6 *g_stash = NULL;

static vdynamic *do_call( vclosure *c, int arg ) {
	vdynamic  a;
	vdynamic *args[1];
	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;
	if( getenv("CB6_HLP") ) return hlp_dyn_call(c, args, 1);
	return hl_dyn_call(c, args, 1);
}

// Describe whatever hl_dyn_call handed back, defensively.
static void describe( const char *tag, vdynamic *ret ) {
	hstring *s = (hstring*)ret;
	char *u8;
	if( ret == NULL ) {
		printf("[cb6] %-8s: NULL\n", tag);
		fflush(stdout);
		return;
	}
	printf("[cb6] %-8s: obj=%p t=%p kind=%d bytes=%p length=%d",
		tag, (void*)ret, (void*)s->t, s->t ? (int)s->t->kind : -1,
		(void*)s->bytes, s->length);
	if( s->bytes ) {
		u8 = hl_to_utf8(s->bytes);
		printf(" utf8=\"%s\"", u8 ? u8 : "(null)");
	}
	printf("\n");
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// store: keep the raw vclosure* in malloc'd memory, no GC root by default
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g_stash == NULL ) g_stash = (stash6*)malloc(sizeof(stash6));
	g_stash->magic = STASH6_MAGIC;
	g_stash->cb = c;
	g_stash->fun_at_store = c ? c->fun : NULL;
	g_stash->ret = NULL;
	g_stash->ret_snap = NULL;
	mode = getenv("CB6_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb6] root    : hl_add_root(&stash->cb) = %p\n", (void*)&g_stash->cb);
		fflush(stdout);
		hl_add_root(&g_stash->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb6] root    : hl_add_root(closure) = %p\n", (void*)c);
		fflush(stdout);
		hl_add_root(c);
	}
	printf("[cb6] store   : vclosure=%p t=%p fun=%p hasValue=%d\n",
		(void*)c, c ? (void*)c->t : NULL, c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store, _FUN(_STRING,_I32));

// ---------------------------------------------------------------------------
// invoke: call the stored closure, hand the String straight back to Haxe
// ---------------------------------------------------------------------------
HL_PRIM vdynamic *HL_NAME(invoke)( int arg ) {
	vclosure *c;
	vdynamic *ret;
	if( g_stash == NULL || g_stash->magic != STASH6_MAGIC ) {
		printf("[cb6] invoke  : nothing stored\n"); fflush(stdout);
		return NULL;
	}
	c = g_stash->cb;
	printf("[cb6] invoke  : vclosure=%p fun=%p (fun at store=%p) hasValue=%d arg=%d\n",
		(void*)c, c ? (void*)c->fun : NULL, g_stash->fun_at_store,
		c ? (int)c->hasValue : -1, arg);
	fflush(stdout);
	if( c == NULL ) return NULL;
	ret = do_call(c, arg);
	describe("result", ret);
	return ret;
}
DEFINE_PRIM(_STRING, invoke, _I32);

// ---------------------------------------------------------------------------
// hold_check: the rooting probe for the RETURN VALUE.
//   r1 = closure(a)          <- heap String, held only by this C frame
//   snapshot r1's bytes into malloc'd memory
//   r2 = closure(b)          <- more Haxe allocation (a GC opportunity)
//   re-read r1 and compare with the snapshot
// returns 1 = survived, 0 = changed under us, negative = structural failure
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(hold_check)( int a, int b ) {
	vclosure *c;
	vdynamic *r1, *r2;
	hstring  *s1;
	char     *snap;
	char     *after;
	int       rv;

	if( g_stash == NULL || g_stash->magic != STASH6_MAGIC ) return -1;
	c = g_stash->cb;
	if( c == NULL ) return -2;

	r1 = do_call(c, a);
	describe("hold r1", r1);
	if( r1 == NULL ) return -3;
	s1 = (hstring*)r1;
	if( s1->bytes == NULL ) return -4;
	if( getenv("CB6_ROOTRET") ) {
		printf("[cb6] rootret : hl_add_root(r1=%p)\n", (void*)r1); fflush(stdout);
		hl_add_root(r1);
	}
	snap = strdup(hl_to_utf8(s1->bytes));

	r2 = do_call(c, b);
	describe("hold r2", r2);

	// r1 must be untouched
	describe("hold r1'", r1);
	if( s1->bytes == NULL ) { free(snap); return -5; }
	after = hl_to_utf8(s1->bytes);
	rv = (after && strcmp(after, snap) == 0) ? 1 : 0;
	printf("[cb6] hold    : before=\"%s\" after=\"%s\" -> %s\n",
		snap, after ? after : "(null)", rv ? "SAME" : "CHANGED");
	fflush(stdout);
	free(snap);
	return rv;
}
DEFINE_PRIM(_I32, hold_check, _I32 _I32);

// ---------------------------------------------------------------------------
// stash_ret / check_ret: the same probe as hold_check, but the returned String
// is parked in MALLOC'D memory across a return to Haxe -- no C stack slot and
// no register can keep it alive there, so only a GC root can. This is the
// exact hxDatachannel shape applied to a RETURN value.
//   CB6_ROOTRET=object  hl_add_root(the String)   (what ash's hl_add_root wants)
//   CB6_ROOTRET=slot    hl_add_root(&stash->ret)  (what upstream documents)
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(stash_ret)( int arg ) {
	vclosure   *c;
	vdynamic   *r;
	const char *rr;
	if( g_stash == NULL || g_stash->magic != STASH6_MAGIC ) return;
	c = g_stash->cb;
	if( c == NULL ) return;
	r = do_call(c, arg);
	describe("stash r", r);
	if( r == NULL ) return;
	g_stash->ret = r;
	g_stash->ret_snap = strdup(hl_to_utf8(((hstring*)r)->bytes));
	rr = getenv("CB6_ROOTRET");
	if( rr && strcmp(rr,"object") == 0 ) {
		printf("[cb6] rootret : hl_add_root(String=%p)\n", (void*)r); fflush(stdout);
		hl_add_root(r);
	} else if( rr && strcmp(rr,"slot") == 0 ) {
		printf("[cb6] rootret : hl_add_root(&stash->ret=%p)\n", (void*)&g_stash->ret); fflush(stdout);
		hl_add_root(&g_stash->ret);
	}
}
DEFINE_PRIM(_VOID, stash_ret, _I32);

HL_PRIM int HL_NAME(check_ret)( void ) {
	hstring *s;
	char    *now;
	int      rv;
	if( g_stash == NULL || g_stash->magic != STASH6_MAGIC ) return -1;
	if( g_stash->ret == NULL || g_stash->ret_snap == NULL ) return -2;
	describe("check r", g_stash->ret);
	s = (hstring*)g_stash->ret;
	if( s->t == NULL || s->bytes == NULL ) {
		printf("[cb6] check   : header destroyed (t=%p bytes=%p)\n",
			(void*)s->t, (void*)s->bytes);
		fflush(stdout);
		return 0;
	}
	now = hl_to_utf8(s->bytes);
	rv = (now && strcmp(now, g_stash->ret_snap) == 0) ? 1 : 0;
	printf("[cb6] check   : before=\"%s\" after=\"%s\" -> %s\n",
		g_stash->ret_snap, now ? now : "(null)", rv ? "SAME" : "CHANGED");
	fflush(stdout);
	return rv;
}
DEFINE_PRIM(_I32, check_ret, _NO_ARG);
