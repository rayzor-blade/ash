// cb3test.hdll -- CASE #3: MIXED SCALAR ARGUMENTS through hl_dyn_call.
//
// Same shape as cbtest.c (hxDatachannel pattern: native stores a raw
// vclosure* in a malloc'd struct, no GC root, calls it later through
// hl_dyn_call), but every callback mixes Int / Float / Bool so that the
// per-type boxing path is exercised:
//
//   slot 0  (Int,Float,Bool) -> Int
//   slot 1  (Int,Float,Bool) -> Float
//   slot 2  (Int,Float,Bool) -> Bool
//   slot 3  (Bool,Float,Int) -> Float      reordered: different reg classes
//   slot 4  (I,F,I,F,B,F,I)  -> Float      7 args, alternating int/fp
//   slot 5  bound (Int,Float,Bool) -> Float  hasValue=1 receiver
//
// Diagnostics (all default-off; use `env -u VAR` to clear):
//   CB3_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB3_ROOT=object  hl_add_root(closure)
//   CB3_ROOT=slot    hl_add_root(&g_slots[i].cb)   <- upstream HashLink spelling

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb3test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define NSLOT   8
#define MAGIC   0x0CB30003

typedef struct {
	int         magic;
	vclosure   *cb;
	void       *fun_at_store;
	hl_type    *t_at_store;
	const char *name;
} slot_t;

static slot_t *g_slots = NULL;

static void ensure_slots( void ) {
	if( g_slots == NULL ) g_slots = (slot_t*)calloc(NSLOT, sizeof(slot_t));
}

static void store_slot( int idx, const char *name, vclosure *c ) {
	const char *mode;
	ensure_slots();
	g_slots[idx].magic        = MAGIC;
	g_slots[idx].cb           = c;
	g_slots[idx].fun_at_store = c ? c->fun : NULL;
	g_slots[idx].t_at_store   = c ? c->t   : NULL;
	g_slots[idx].name         = name;

	mode = getenv("CB3_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb3] root    : hl_add_root(&slots[%d].cb) = %p\n", idx, (void*)&g_slots[idx].cb);
		hl_add_root(&g_slots[idx].cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb3] root    : hl_add_root(closure) = %p\n", (void*)c);
		hl_add_root(c);
	}
	printf("[cb3] store   : slot=%d %-5s vclosure=%p t=%p fun=%p hasValue=%d\n",
		idx, name, (void*)c,
		c ? (void*)c->t : NULL, c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
}

static const char *kindname( int k ) {
	switch( k ) {
	case HVOID: return "HVOID"; case HUI8: return "HUI8"; case HUI16: return "HUI16";
	case HI32: return "HI32"; case HI64: return "HI64"; case HF32: return "HF32";
	case HF64: return "HF64"; case HBOOL: return "HBOOL"; case HBYTES: return "HBYTES";
	case HDYN: return "HDYN"; case HFUN: return "HFUN"; case HOBJ: return "HOBJ";
	case HNULL: return "HNULL"; case HMETHOD: return "HMETHOD";
	default: return "?";
	}
}

// Call the stored closure and dump the raw box we got back.
static vdynamic *call_slot( int idx, vdynamic **args, int n ) {
	slot_t   *s;
	vclosure *c;
	vdynamic *ret;

	ensure_slots();
	s = &g_slots[idx];
	if( s->magic != MAGIC ) {
		printf("[cb3] invoke  : slot %d empty\n", idx);
		fflush(stdout);
		return NULL;
	}
	c = s->cb;
	printf("[cb3] invoke  : slot=%d %-5s vclosure=%p t=%p (store %p) fun=%p (store %p) hasValue=%d nargs=%d\n",
		idx, s->name, (void*)c,
		c ? (void*)c->t : NULL, (void*)s->t_at_store,
		c ? (void*)c->fun : NULL, s->fun_at_store,
		c ? (int)c->hasValue : -1, n);
	{
		int i;
		for( i = 0; i < n; i++ )
			printf("[cb3]   arg[%d] : t=%p kind=%s i=%d d=%.17g b=%d raw=0x%016llx\n",
				i, (void*)args[i]->t,
				args[i]->t ? kindname(args[i]->t->kind) : "NULL",
				args[i]->v.i, args[i]->v.d, (int)args[i]->v.b,
				(unsigned long long)args[i]->v.i64);
	}
	fflush(stdout);
	if( c == NULL ) return NULL;

	if( getenv("CB3_HLP") ) {
		printf("[cb3] path    : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(c, args, n);
	} else {
		printf("[cb3] path    : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(c, args, n);
	}

	if( ret == NULL ) {
		printf("[cb3] result  : NULL\n"); fflush(stdout);
		return NULL;
	}
	printf("[cb3] result  : vdynamic=%p t=%p kind=%s | i=%d d=%.17g f=%.9g b=%d i64=%lld raw=0x%016llx\n",
		(void*)ret, (void*)ret->t,
		ret->t ? kindname(ret->t->kind) : "NULL",
		ret->v.i, ret->v.d, (double)ret->v.f, (int)ret->v.b,
		(long long)ret->v.i64, (unsigned long long)ret->v.i64);
	fflush(stdout);
	return ret;
}

static double unbox_num( vdynamic *r ) {
	if( r == NULL || r->t == NULL ) return 0;
	switch( r->t->kind ) {
	case HI32:  return (double)r->v.i;
	case HI64:  return (double)r->v.i64;
	case HF32:  return (double)r->v.f;
	case HF64:  return r->v.d;
	case HBOOL: return r->v.b ? 1 : 0;
	case HUI8:  return (double)r->v.ui8;
	case HUI16: return (double)r->v.ui16;
	default:    return 0;
	}
}

// ---------------------------------------------------------------------------
// store prims -- one per closure signature
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store_i)( vclosure *c ) { store_slot(0, "fnI", c); }
DEFINE_PRIM(_VOID, store_i, _FUN(_I32, _I32 _F64 _BOOL));

HL_PRIM void HL_NAME(store_f)( vclosure *c ) { store_slot(1, "fnF", c); }
DEFINE_PRIM(_VOID, store_f, _FUN(_F64, _I32 _F64 _BOOL));

HL_PRIM void HL_NAME(store_b)( vclosure *c ) { store_slot(2, "fnB", c); }
DEFINE_PRIM(_VOID, store_b, _FUN(_BOOL, _I32 _F64 _BOOL));

HL_PRIM void HL_NAME(store_r)( vclosure *c ) { store_slot(3, "fnR", c); }
DEFINE_PRIM(_VOID, store_r, _FUN(_F64, _BOOL _F64 _I32));

HL_PRIM void HL_NAME(store_w)( vclosure *c ) { store_slot(4, "fnW", c); }
DEFINE_PRIM(_VOID, store_w, _FUN(_F64, _I32 _F64 _I32 _F64 _BOOL _F64 _I32));

HL_PRIM void HL_NAME(store_m)( vclosure *c ) { store_slot(5, "bnd", c); }
DEFINE_PRIM(_VOID, store_m, _FUN(_F64, _I32 _F64 _BOOL));

// ---------------------------------------------------------------------------
// invoke prims
// ---------------------------------------------------------------------------
static void mk3( vdynamic *ai, vdynamic *ad, vdynamic *ab,
                 int i, double d, bool b, vdynamic **args ) {
	memset(ai, 0, sizeof(vdynamic));
	memset(ad, 0, sizeof(vdynamic));
	memset(ab, 0, sizeof(vdynamic));
	ai->t = &hlt_i32;  ai->v.i = i;
	ad->t = &hlt_f64;  ad->v.d = d;
	ab->t = &hlt_bool; ab->v.b = b;
	args[0] = ai; args[1] = ad; args[2] = ab;
}

HL_PRIM int HL_NAME(invoke_i)( int i, double d, bool b ) {
	vdynamic ai, ad, ab; vdynamic *args[3];
	mk3(&ai,&ad,&ab,i,d,b,args);
	return (int)unbox_num(call_slot(0, args, 3));
}
DEFINE_PRIM(_I32, invoke_i, _I32 _F64 _BOOL);

HL_PRIM double HL_NAME(invoke_f)( int i, double d, bool b ) {
	vdynamic ai, ad, ab; vdynamic *args[3];
	mk3(&ai,&ad,&ab,i,d,b,args);
	return unbox_num(call_slot(1, args, 3));
}
DEFINE_PRIM(_F64, invoke_f, _I32 _F64 _BOOL);

HL_PRIM bool HL_NAME(invoke_b)( int i, double d, bool b ) {
	vdynamic ai, ad, ab; vdynamic *args[3];
	mk3(&ai,&ad,&ab,i,d,b,args);
	return unbox_num(call_slot(2, args, 3)) != 0;
}
DEFINE_PRIM(_BOOL, invoke_b, _I32 _F64 _BOOL);

HL_PRIM double HL_NAME(invoke_r)( bool b, double d, int i ) {
	vdynamic ab, ad, ai; vdynamic *args[3];
	memset(&ab,0,sizeof(ab)); memset(&ad,0,sizeof(ad)); memset(&ai,0,sizeof(ai));
	ab.t = &hlt_bool; ab.v.b = b;
	ad.t = &hlt_f64;  ad.v.d = d;
	ai.t = &hlt_i32;  ai.v.i = i;
	args[0] = &ab; args[1] = &ad; args[2] = &ai;
	return unbox_num(call_slot(3, args, 3));
}
DEFINE_PRIM(_F64, invoke_r, _BOOL _F64 _I32);

HL_PRIM double HL_NAME(invoke_w)( int i1, double d1, int i2, double d2, bool b, double d3, int i3 ) {
	vdynamic v[7]; vdynamic *args[7]; int k;
	for( k = 0; k < 7; k++ ) { memset(&v[k],0,sizeof(vdynamic)); args[k] = &v[k]; }
	v[0].t = &hlt_i32;  v[0].v.i = i1;
	v[1].t = &hlt_f64;  v[1].v.d = d1;
	v[2].t = &hlt_i32;  v[2].v.i = i2;
	v[3].t = &hlt_f64;  v[3].v.d = d2;
	v[4].t = &hlt_bool; v[4].v.b = b;
	v[5].t = &hlt_f64;  v[5].v.d = d3;
	v[6].t = &hlt_i32;  v[6].v.i = i3;
	return unbox_num(call_slot(4, args, 7));
}
DEFINE_PRIM(_F64, invoke_w, _I32 _F64 _I32 _F64 _BOOL _F64 _I32);

HL_PRIM double HL_NAME(invoke_m)( int i, double d, bool b ) {
	vdynamic ai, ad, ab; vdynamic *args[3];
	mk3(&ai,&ad,&ab,i,d,b,args);
	return unbox_num(call_slot(5, args, 3));
}
DEFINE_PRIM(_F64, invoke_m, _I32 _F64 _BOOL);

// ---------------------------------------------------------------------------
// int-only entry points.
//
// The mixed-signature prims above (invoke_i/f/b/r/w/m) cannot be reached at
// all in --mode interp / --mode hybrid: ash_interp's native FFI dispatch is a
// hand-written match on (argc, ret_is_float, float_mask) and
// (3, false, 0b010) -- (i32, f64, bool) -> i32 -- is not in it, so the call
// dies with "Float native dispatch: ... not yet supported" before hl_dyn_call
// is ever entered. To compare the hl_dyn_call marshalling across all three
// engines, drive it from prims whose OWN signature is int-only: the argument
// values live here in C, and the result comes back as round(x * 1000).
// ---------------------------------------------------------------------------
static int g_last_kind = -1;
static int g_last_null = 0;

static int case_run( int idx ) {
	vdynamic v[7]; vdynamic *args[7]; int k, n = 3, slot = 0;
	vdynamic *ret;
	double d;
	for( k = 0; k < 7; k++ ) { memset(&v[k], 0, sizeof(vdynamic)); args[k] = &v[k]; }
	g_last_kind = -1; g_last_null = 0;

	switch( idx ) {
	case 0: case 1: case 2: case 3: case 4: case 5: case 9: {
		int    i = (idx == 1 || idx == 3 || idx == 5) ? -3 : 5;
		double d0 = (idx == 1 || idx == 3 || idx == 5) ? 0.5 : 2.5;
		bool   b = (idx == 1 || idx == 3 || idx == 5) ? false : true;
		slot = (idx <= 1) ? 0 : (idx <= 3) ? 1 : (idx <= 5) ? 2 : 5;
		v[0].t = &hlt_i32;  v[0].v.i = i;
		v[1].t = &hlt_f64;  v[1].v.d = d0;
		v[2].t = &hlt_bool; v[2].v.b = b;
		n = 3;
		break;
	}
	case 6: case 7:
		slot = 3;
		v[0].t = &hlt_bool; v[0].v.b = (idx == 6);
		v[1].t = &hlt_f64;  v[1].v.d = (idx == 6) ? 1.25 : -2.5;
		v[2].t = &hlt_i32;  v[2].v.i = (idx == 6) ? 9 : 7;
		n = 3;
		break;
	case 8:
		slot = 4;
		v[0].t = &hlt_i32;  v[0].v.i = 1;
		v[1].t = &hlt_f64;  v[1].v.d = 1.5;
		v[2].t = &hlt_i32;  v[2].v.i = 2;
		v[3].t = &hlt_f64;  v[3].v.d = 2.5;
		v[4].t = &hlt_bool; v[4].v.b = true;
		v[5].t = &hlt_f64;  v[5].v.d = 3.5;
		v[6].t = &hlt_i32;  v[6].v.i = 4;
		n = 7;
		break;
	default:
		printf("[cb3] case    : unknown case %d\n", idx);
		fflush(stdout);
		return 0;
	}

	printf("[cb3] case %d  : slot=%d nargs=%d\n", idx, slot, n);
	fflush(stdout);
	ret = call_slot(slot, args, n);
	if( ret == NULL ) { g_last_null = 1; return 0; }
	g_last_kind = ret->t ? (int)ret->t->kind : -1;
	d = unbox_num(ret);
	return (int)(d < 0 ? d * 1000 - 0.5 : d * 1000 + 0.5);
}

HL_PRIM int HL_NAME(case_run)( int idx ) { return case_run(idx); }
DEFINE_PRIM(_I32, case_run, _I32);

HL_PRIM int HL_NAME(last_kind)( void ) { return g_last_kind; }
DEFINE_PRIM(_I32, last_kind, _NO_ARG);

HL_PRIM int HL_NAME(last_null)( void ) { return g_last_null; }
DEFINE_PRIM(_I32, last_null, _NO_ARG);
