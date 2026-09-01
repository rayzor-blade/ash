// cb8test.hdll -- CASE #8: INSTANCE METHOD (bound) closures called from a
// native library through hl_dyn_call.
//
// A bound closure has hasValue=1: `c->value` is the receiver and `c->t` is the
// STRIPPED function type (the signature Haxe sees, e.g. Int->Int), whose
// `fun->parent` is the FULL type that the underlying `c->fun` actually
// implements (e.g. (Adder8,Int)->Int). hl_dyn_call must notice that, prepend
// the receiver as argument 0, and dispatch against the full type. That is the
// "stripped-vs-full type shape".
//
// Shape mirrors hxDatachannel: Haxe hands the closure to native code, native
// code keeps the RAW vclosure* in a malloc'd struct with NO GC root, the Haxe
// program churns the heap, and only afterwards does native code call it. For a
// bound closure the receiver is reachable ONLY through that raw pointer, so a
// missing root loses two objects, not one.
//
// Diagnostic switches (all default off; use `env -u VAR` to clear -- an empty
// value is still "set"):
//   CB8_HLP=1        call hlp_dyn_call instead of hl_dyn_call
//   CB8_ROOT=object  hl_add_root(closure)        (what ash's hl_add_root wants)
//   CB8_ROOT=slot    hl_add_root(&slot->cb)      (upstream HashLink spelling)
//   CB8_ROOT=value   hl_add_root(closure->value) (root only the receiver)
//   CB8_ROOT=both    root the closure AND its receiver
//   CB8_VERBOSE=1    print every store/invoke; otherwise only anomalies
// combine with ASH_GC_STRESS=1 / ASH_GC_NO_RECLAIM=1 / ASH_GC_STATS=1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb8test_##n
#include "hl.h"

// ash_std's own upstream-shaped entry point, so hl_dyn_call and hlp_dyn_call
// can be A/B'd against ONE ash binary.
HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define NSLOT      16
#define SLOT_MAGIC 0x0B0111D8

// Sentinels, so a broken call is a distinctive number instead of plausible data.
#define R_NOSLOT   -111111
#define R_NULLCB   -222222
#define R_NULLRET  -333333
#define R_BADKIND  -444444

typedef struct {
	int       magic;
	vclosure *cb;              // the raw pointer, exactly like hxDatachannel
	void     *fun_at_store;
	void     *value_at_store;
	hl_type  *t_at_store;
	int       hasvalue_at_store;
	int       base_at_store;   // receiver field readback at store time (-1 = n/a)
	char      name[40];
} slot;

// malloc'd: invisible to the collector unless something roots it.
static slot *g_slots[NSLOT];

static int verbose( void ) {
	static int v = -1;
	if( v < 0 ) v = getenv("CB8_VERBOSE") != NULL;
	return v;
}

static const char *kind_name( int k ) {
	switch( k ) {
	case HVOID: return "HVOID"; case HUI8: return "HUI8"; case HUI16: return "HUI16";
	case HI32: return "HI32"; case HI64: return "HI64"; case HF32: return "HF32";
	case HF64: return "HF64"; case HBOOL: return "HBOOL"; case HBYTES: return "HBYTES";
	case HDYN: return "HDYN"; case HFUN: return "HFUN"; case HOBJ: return "HOBJ";
	case HARRAY: return "HARRAY"; case HTYPE: return "HTYPE"; case HREF: return "HREF";
	case HVIRTUAL: return "HVIRTUAL"; case HDYNOBJ: return "HDYNOBJ";
	case HABSTRACT: return "HABSTRACT"; case HENUM: return "HENUM"; case HNULL: return "HNULL";
	case HMETHOD: return "HMETHOD"; case HSTRUCT: return "HSTRUCT"; default: return "?";
	}
}

// Read an Int field off the receiver, straight from the native side. If the
// receiver has been reclaimed under the collector this either reads back a
// different number or faults -- a direct probe of the bound closure's value.
// Off by default: not every receiver in the suite has a `base` field, and a
// dynamic get on a receiver that lacks one is itself an event we do not want
// mixed into the baseline. CB8_PROBE=1 turns it on.
static int probe_base( vclosure *c ) {
	static int on = -1;
	if( on < 0 ) on = getenv("CB8_PROBE") != NULL;
	if( !on ) return -1;
	if( c == NULL || c->hasValue == 0 || c->value == NULL ) return -1;
	return hl_dyn_geti((vdynamic*)c->value, hl_hash_utf8("base"), &hlt_i32);
}

// Print the stripped/full type shape of a bound closure.
static void describe( const char *tag, int idx, vclosure *c ) {
	hl_type_fun *sf = NULL, *pf = NULL;
	hl_type *parent = NULL;
	if( c != NULL && c->t != NULL && c->t->kind == HFUN ) {
		sf = c->t->fun;
		if( sf != NULL ) {
			parent = sf->parent;
			if( parent != NULL && (parent->kind == HFUN || parent->kind == HMETHOD) )
				pf = parent->fun;
		}
	}
	printf("[cb8] %s[%2d]: vclosure=%p t=%p kind=%s hasValue=%d value=%p fun=%p\n",
		tag, idx, (void*)c,
		c ? (void*)c->t : NULL,
		(c && c->t) ? kind_name(c->t->kind) : "-",
		c ? c->hasValue : -1,
		c ? c->value : NULL,
		c ? c->fun : NULL);
	printf("[cb8]         stripped nargs=%d ret=%s | parent=%p full nargs=%d arg0=%s ret=%s\n",
		sf ? sf->nargs : -1,
		(sf && sf->ret) ? kind_name(sf->ret->kind) : "-",
		(void*)parent,
		pf ? pf->nargs : -1,
		(pf && pf->nargs > 0 && pf->args[0]) ? kind_name(pf->args[0]->kind) : "-",
		(pf && pf->ret) ? kind_name(pf->ret->kind) : "-");
	fflush(stdout);
}

static void do_root( slot *s, vclosure *c ) {
	const char *mode = getenv("CB8_ROOT");
	if( mode == NULL || c == NULL ) return;
	if( strcmp(mode,"slot") == 0 ) {
		hl_add_root(&s->cb);
		if( verbose() ) printf("[cb8] root    : hl_add_root(&slot->cb) = %p\n", (void*)&s->cb);
	} else if( strcmp(mode,"object") == 0 ) {
		hl_add_root(c);
		if( verbose() ) printf("[cb8] root    : hl_add_root(closure) = %p\n", (void*)c);
	} else if( strcmp(mode,"value") == 0 ) {
		if( c->value ) hl_add_root(c->value);
		if( verbose() ) printf("[cb8] root    : hl_add_root(value) = %p\n", c->value);
	} else if( strcmp(mode,"both") == 0 ) {
		hl_add_root(c);
		if( c->value ) hl_add_root(c->value);
		if( verbose() ) printf("[cb8] root    : hl_add_root(closure=%p) + hl_add_root(value=%p)\n",
			(void*)c, c->value);
	}
	fflush(stdout);
}

static void store_at( int idx, vclosure *c, const char *name ) {
	slot *s;
	if( idx < 0 || idx >= NSLOT ) return;
	if( g_slots[idx] == NULL ) g_slots[idx] = (slot*)malloc(sizeof(slot));
	s = g_slots[idx];
	memset(s, 0, sizeof(*s));
	s->magic = SLOT_MAGIC;
	s->cb = c;
	s->fun_at_store = c ? c->fun : NULL;
	s->value_at_store = c ? c->value : NULL;
	s->t_at_store = c ? c->t : NULL;
	s->hasvalue_at_store = c ? c->hasValue : -1;
	s->base_at_store = probe_base(c);
	snprintf(s->name, sizeof(s->name), "%s", name ? name : "?");
	if( verbose() ) describe("store", idx, c);
	do_root(s, c);
}

// ---------------------------------------------------------------------------
// store prims, one per closure signature under test
// ---------------------------------------------------------------------------
// The label is a Haxe String (HOBJ): read it through the vstring layout, the
// same way any HDLL reads a String argument.
static const char *label_of( vstring *name ) {
	static char buf[40];
	char *u = (name && name->bytes) ? (char*)hl_to_utf8(name->bytes) : NULL;
	snprintf(buf, sizeof(buf), "%s", u ? u : "?");
	return buf;
}

HL_PRIM void HL_NAME(store_ii)( int idx, vclosure *c, vstring *name ) {
	store_at(idx, c, label_of(name));
}
DEFINE_PRIM(_VOID, store_ii, _I32 _FUN(_I32,_I32) _STRING);

HL_PRIM void HL_NAME(store_is)( int idx, vclosure *c, vstring *name ) {
	store_at(idx, c, label_of(name));
}
DEFINE_PRIM(_VOID, store_is, _I32 _FUN(_STRING,_I32) _STRING);

HL_PRIM void HL_NAME(store_mix)( int idx, vclosure *c, vstring *name ) {
	store_at(idx, c, label_of(name));
}
DEFINE_PRIM(_VOID, store_mix, _I32 _FUN(_I32, _I32 _F64 _STRING) _STRING);

HL_PRIM void HL_NAME(store_void)( int idx, vclosure *c, vstring *name ) {
	store_at(idx, c, label_of(name));
}
DEFINE_PRIM(_VOID, store_void, _I32 _FUN(_VOID,_STRING) _STRING);

// ---------------------------------------------------------------------------
// the actual dyn_call
// ---------------------------------------------------------------------------
static vdynamic *call_slot( slot *s, vdynamic **args, int nargs ) {
	vclosure *c = s->cb;
	int drift = (c != NULL) &&
		(c->fun != s->fun_at_store || c->value != s->value_at_store ||
		 c->t != s->t_at_store || c->hasValue != s->hasvalue_at_store);
	if( verbose() || drift ) {
		if( drift )
			printf("[cb8] DRIFT   : %s closure fields changed since store "
				"(fun %p->%p value %p->%p t %p->%p hasValue %d->%d)\n",
				s->name, s->fun_at_store, c->fun, s->value_at_store, c->value,
				(void*)s->t_at_store, (void*)c->t, s->hasvalue_at_store, c->hasValue);
		describe("invoke", -1, c);
	}
	if( s->base_at_store >= 0 ) {
		int now = probe_base(c);
		if( now != s->base_at_store )
			printf("[cb8] RECEIVER: %s .base read back %d, was %d at store\n",
				s->name, now, s->base_at_store);
	}
	if( getenv("CB8_HLP") ) return hlp_dyn_call(c, args, nargs);
	return hl_dyn_call(c, args, nargs);
}

static slot *get_slot( int idx ) {
	if( idx < 0 || idx >= NSLOT || g_slots[idx] == NULL || g_slots[idx]->magic != SLOT_MAGIC )
		return NULL;
	return g_slots[idx];
}

static int unbox_i( slot *s, vdynamic *ret ) {
	if( ret == NULL ) {
		printf("[cb8] ANOMALY : %s hl_dyn_call returned NULL\n", s->name);
		fflush(stdout);
		return R_NULLRET;
	}
	if( ret->t == NULL || ret->t->kind != HI32 ) {
		printf("[cb8] ANOMALY : %s result kind=%s (expected HI32) raw=%d\n",
			s->name, ret->t ? kind_name(ret->t->kind) : "<null t>", ret->v.i);
		fflush(stdout);
		return R_BADKIND;
	}
	if( verbose() ) { printf("[cb8] result  : %s -> %d\n", s->name, ret->v.i); fflush(stdout); }
	return ret->v.i;
}

// Int -> Int
HL_PRIM int HL_NAME(invoke_i)( int idx, int arg ) {
	slot *s = get_slot(idx);
	vdynamic a, *args[1];
	if( s == NULL ) { printf("[cb8] ANOMALY : invoke_i[%d] nothing stored\n", idx); fflush(stdout); return R_NOSLOT; }
	if( s->cb == NULL ) { printf("[cb8] ANOMALY : %s stored closure is NULL\n", s->name); fflush(stdout); return R_NULLCB; }
	memset(&a, 0, sizeof(a));
	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;
	return unbox_i(s, call_slot(s, args, 1));
}
DEFINE_PRIM(_I32, invoke_i, _I32 _I32);

// Int -> String  (pointer return; handed back to Haxe as Dynamic)
HL_PRIM vdynamic *HL_NAME(invoke_s)( int idx, int arg ) {
	slot *s = get_slot(idx);
	vdynamic a, *args[1], *ret;
	if( s == NULL || s->cb == NULL ) { printf("[cb8] ANOMALY : invoke_s[%d] nothing usable stored\n", idx); fflush(stdout); return NULL; }
	memset(&a, 0, sizeof(a));
	a.t = &hlt_i32;
	a.v.i = arg;
	args[0] = &a;
	ret = call_slot(s, args, 1);
	if( ret == NULL ) { printf("[cb8] ANOMALY : %s hl_dyn_call returned NULL\n", s->name); fflush(stdout); }
	else if( verbose() ) {
		printf("[cb8] result  : %s -> ret=%p kind=%s\n", s->name, (void*)ret,
			ret->t ? kind_name(ret->t->kind) : "<null t>");
		fflush(stdout);
	}
	return ret;
}
DEFINE_PRIM(_DYN, invoke_s, _I32 _I32);

// (Int, Float, String) -> Int -- three args in front of the prepended receiver
HL_PRIM int HL_NAME(invoke_mix)( int idx, int i, double f, vdynamic *str ) {
	slot *s = get_slot(idx);
	vdynamic ai, af, *args[3];
	if( s == NULL ) { printf("[cb8] ANOMALY : invoke_mix[%d] nothing stored\n", idx); fflush(stdout); return R_NOSLOT; }
	if( s->cb == NULL ) { printf("[cb8] ANOMALY : %s stored closure is NULL\n", s->name); fflush(stdout); return R_NULLCB; }
	memset(&ai, 0, sizeof(ai));
	memset(&af, 0, sizeof(af));
	ai.t = &hlt_i32; ai.v.i = i;
	af.t = &hlt_f64; af.v.d = f;
	args[0] = &ai;
	args[1] = &af;
	args[2] = str;              // an HOBJ (String) is its own boxed form
	return unbox_i(s, call_slot(s, args, 3));
}
DEFINE_PRIM(_I32, invoke_mix, _I32 _I32 _F64 _DYN);

// Same call, but the Float arrives BOXED. ash's interpreter dispatches a
// native prim through a hand-written table of (arity, float-mask) shapes
// (crates/ash_interp/src/interpreter/natives.rs), and (4 args, float in slot
// 2) is not in it -- so `invoke_mix` above cannot even be entered under
// --mode interp/hybrid. Taking the double as a Dynamic keeps every prim
// argument integer/pointer-shaped, so the bound closure with a Float
// parameter can still be exercised through hl_dyn_call on every engine.
HL_PRIM int HL_NAME(invoke_mixd)( int idx, int i, vdynamic *fbox, vdynamic *str ) {
	slot *s = get_slot(idx);
	vdynamic ai, af, *args[3];
	if( s == NULL ) { printf("[cb8] ANOMALY : invoke_mixd[%d] nothing stored\n", idx); fflush(stdout); return R_NOSLOT; }
	if( s->cb == NULL ) { printf("[cb8] ANOMALY : %s stored closure is NULL\n", s->name); fflush(stdout); return R_NULLCB; }
	memset(&ai, 0, sizeof(ai));
	memset(&af, 0, sizeof(af));
	ai.t = &hlt_i32; ai.v.i = i;
	af.t = &hlt_f64;
	af.v.d = (fbox && fbox->t && fbox->t->kind == HF64) ? fbox->v.d
	       : (fbox && fbox->t && fbox->t->kind == HI32) ? (double)fbox->v.i : 0.0;
	args[0] = &ai;
	args[1] = &af;
	args[2] = str;
	return unbox_i(s, call_slot(s, args, 3));
}
DEFINE_PRIM(_I32, invoke_mixd, _I32 _I32 _DYN _DYN);

// String -> Void -- a void-returning bound method (side effect only)
HL_PRIM void HL_NAME(invoke_void)( int idx, vdynamic *str ) {
	slot *s = get_slot(idx);
	vdynamic *args[1];
	vdynamic *ret;
	if( s == NULL || s->cb == NULL ) { printf("[cb8] ANOMALY : invoke_void[%d] nothing usable stored\n", idx); fflush(stdout); return; }
	args[0] = str;
	ret = call_slot(s, args, 1);
	if( verbose() ) { printf("[cb8] result  : %s -> void (ret=%p)\n", s->name, (void*)ret); fflush(stdout); }
}
DEFINE_PRIM(_VOID, invoke_void, _I32 _DYN);

// Dump one slot's shape on demand (used once per run so the report shows the
// stripped-vs-full types even when CB8_VERBOSE is off).
HL_PRIM void HL_NAME(dump)( int idx ) {
	slot *s = get_slot(idx);
	if( s == NULL ) { printf("[cb8] dump[%2d]: nothing stored\n", idx); fflush(stdout); return; }
	printf("[cb8] --- %s ---\n", s->name);
	describe("dump ", idx, s->cb);
}
DEFINE_PRIM(_VOID, dump, _I32);
