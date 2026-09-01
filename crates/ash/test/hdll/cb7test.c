// cb7test.hdll -- CASE #7: closures that RETURN Dynamic.
//
// Shape (hxDatachannel): Haxe hands closures to the native library, the native
// library stores the raw vclosure* in a malloc'd struct with NO GC root, and
// later calls hl_dyn_call(c, args, nargs) from a pump. The interesting part
// here is the RETURN value: hl_dyn_call hands back a vdynamic* that the native
// side must UNBOX itself (read ->t->kind, then ->v.i / ->v.d / ->v.b / the
// object payload).
//
// Diagnostic switches (all default off; use `env -u VAR` to clear):
//   CB7_HLP=1          call hlp_dyn_call instead of hl_dyn_call
//   CB7_ROOT=object    hl_add_root(closure)      (what ash's hl_add_root wants)
//   CB7_ROOT=slot      hl_add_root(&slot->cb)    (upstream HashLink spelling)
//   CB7_CAST=1         also cross-check through hlp_dyn_casti/hlp_dyn_castd

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb7test_##n
#include "hl.h"

// ash_std's own, upstream-shaped entry points. hl_dyn_casti/hl_dyn_castd are
// NOT exported from ash's libhl under their hl_ names (only hlp_), so declare
// the hlp_ spellings here rather than fail to link.
HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );
HL_API int       hlp_dyn_casti( void *data, hl_type *t, hl_type *to );
HL_API double    hlp_dyn_castd( void *data, hl_type *t );

#define NSLOT 12
#define SLOT_MAGIC 0x0C0FFEE7

typedef struct {
	int       magic;
	vclosure *cb;
	void     *fun_at_store;
	char      name[32];
} slot;

// malloc'd, exactly like hxDatachannel's per-connection struct: invisible to
// the collector unless something roots it.
static slot *g_slots[NSLOT];

static void do_root( slot *s, vclosure *c ) {
	const char *mode = getenv("CB7_ROOT");
	if( mode == NULL ) return;
	if( strcmp(mode,"slot") == 0 ) {
		printf("[cb7] root    : hl_add_root(&slot->cb) = %p\n", (void*)&s->cb);
		fflush(stdout);
		hl_add_root(&s->cb);
	} else if( strcmp(mode,"object") == 0 ) {
		printf("[cb7] root    : hl_add_root(closure) = %p\n", (void*)c);
		fflush(stdout);
		hl_add_root(c);
	}
}

static void store_at( int idx, vclosure *c, const char *name ) {
	slot *s;
	if( idx < 0 || idx >= NSLOT ) return;
	if( g_slots[idx] == NULL ) g_slots[idx] = (slot*)malloc(sizeof(slot));
	s = g_slots[idx];
	s->magic = SLOT_MAGIC;
	s->cb = c;
	s->fun_at_store = c ? c->fun : NULL;
	snprintf(s->name, sizeof(s->name), "%s", name ? name : "?");
	printf("[cb7] store[%d] : %-9s vclosure=%p t=%p fun=%p hasValue=%d\n",
		idx, s->name, (void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
	do_root(s, c);
}

// ---------------------------------------------------------------------------
// store prims. One per closure signature we exercise.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store_d)( int idx, vclosure *c ) { store_at(idx, c, "Int->Dyn"); }
DEFINE_PRIM(_VOID, store_d, _I32 _FUN(_DYN,_I32));

HL_PRIM void HL_NAME(store_i)( int idx, vclosure *c ) { store_at(idx, c, "Int->Int"); }
DEFINE_PRIM(_VOID, store_i, _I32 _FUN(_I32,_I32));

HL_PRIM void HL_NAME(store_dd)( int idx, vclosure *c ) { store_at(idx, c, "Dyn->Dyn"); }
DEFINE_PRIM(_VOID, store_dd, _I32 _FUN(_DYN,_DYN));

// ---------------------------------------------------------------------------
// unboxing on the native side
// ---------------------------------------------------------------------------
static const char *kind_name( int k ) {
	switch( k ) {
	case HVOID: return "HVOID";
	case HUI8: return "HUI8";
	case HUI16: return "HUI16";
	case HI32: return "HI32";
	case HI64: return "HI64";
	case HF32: return "HF32";
	case HF64: return "HF64";
	case HBOOL: return "HBOOL";
	case HBYTES: return "HBYTES";
	case HDYN: return "HDYN";
	case HFUN: return "HFUN";
	case HOBJ: return "HOBJ";
	case HARRAY: return "HARRAY";
	case HTYPE: return "HTYPE";
	case HREF: return "HREF";
	case HVIRTUAL: return "HVIRTUAL";
	case HDYNOBJ: return "HDYNOBJ";
	case HABSTRACT: return "HABSTRACT";
	case HENUM: return "HENUM";
	case HNULL: return "HNULL";
	case HMETHOD: return "HMETHOD";
	case HSTRUCT: return "HSTRUCT";
	default: return "?";
	}
}

static void unbox( const char *tag, vdynamic *ret ) {
	int kind;
	if( ret == NULL ) {
		printf("[cb7] unbox   : %-9s ret=NULL\n", tag);
		fflush(stdout);
		return;
	}
	kind = ret->t ? (int)ret->t->kind : -1;
	printf("[cb7] unbox   : %-9s ret=%p t=%p kind=%d(%s)",
		tag, (void*)ret, (void*)ret->t, kind, kind_name(kind));
	switch( kind ) {
	case HI32:  printf(" i=%d", ret->v.i); break;
	case HBOOL: printf(" b=%d", (int)ret->v.b); break;
	case HF64:  printf(" d=%.17g", ret->v.d); break;
	case HF32:  printf(" f=%.9g", (double)ret->v.f); break;
	case HI64:  printf(" i64=%lld", (long long)ret->v.i64); break;
	case HOBJ:
	case HSTRUCT: {
		const uchar *n = ret->t->obj ? ret->t->obj->name : NULL;
		char *nu = n ? hl_to_utf8(n) : NULL;
		printf(" objname=%s", nu ? nu : "?");
		if( nu && strcmp(nu,"String") == 0 ) {
			vstring *s = (vstring*)ret;
			char *u = s->bytes ? hl_to_utf8(s->bytes) : NULL;
			printf(" len=%d str=\"%s\"", s->length, u ? u : "<null>");
		}
		break;
	}
	case HDYNOBJ:
	case HVIRTUAL: {
		int a = hl_dyn_geti(ret, hl_hash_utf8("a"), &hlt_i32);
		printf(" .a=%d", a);
		break;
	}
	case HARRAY: {
		varray *a = (varray*)ret;
		printf(" size=%d", a->size);
		break;
	}
	default: break;
	}
	if( getenv("CB7_CAST") ) {
		int ci = hlp_dyn_casti(&ret, &hlt_dyn, &hlt_i32);
		double cd = hlp_dyn_castd(&ret, &hlt_dyn);
		printf(" | casti=%d castd=%.17g", ci, cd);
	}
	printf("\n");
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// invoke: hl_dyn_call the stored closure with one Int arg, unbox the result,
// and hand the raw vdynamic* back to Haxe as a Dynamic.
// ---------------------------------------------------------------------------
static vdynamic *invoke_common( int idx, vdynamic *boxed_arg, const char *argdesc ) {
	slot *s;
	vclosure *c;
	vdynamic *args[1];
	vdynamic *ret;

	if( idx < 0 || idx >= NSLOT || g_slots[idx] == NULL || g_slots[idx]->magic != SLOT_MAGIC ) {
		printf("[cb7] invoke[%d]: nothing stored\n", idx);
		fflush(stdout);
		return NULL;
	}
	s = g_slots[idx];
	c = s->cb;
	printf("[cb7] invoke[%d]: %-9s vclosure=%p t=%p fun=%p (fun@store=%p) hasValue=%d arg=%s\n",
		idx, s->name, (void*)c,
		c ? (void*)c->t : NULL,
		c ? (void*)c->fun : NULL,
		s->fun_at_store,
		c ? (int)c->hasValue : -1,
		argdesc);
	fflush(stdout);
	if( c == NULL ) return NULL;

	args[0] = boxed_arg;
	if( getenv("CB7_HLP") ) {
		ret = hlp_dyn_call(c, args, 1);
	} else {
		ret = hl_dyn_call(c, args, 1);
	}
	unbox(s->name, ret);
	return ret;
}

HL_PRIM vdynamic *HL_NAME(invoke)( int idx, int arg ) {
	vdynamic a;
	char buf[32];
	memset(&a, 0, sizeof(a));
	a.t = &hlt_i32;
	a.v.i = arg;
	snprintf(buf, sizeof(buf), "i32:%d", arg);
	return invoke_common(idx, &a, buf);
}
DEFINE_PRIM(_DYN, invoke, _I32 _I32);

// GC-heap allocated, because a Dyn->Dyn echo closure hands this very pointer
// back to Haxe: a pointer into this function's own stack frame would dangle
// the moment it returns (that is a native-side bug, not a runtime bug).
HL_PRIM vdynamic *HL_NAME(invoke_f)( int idx, double arg ) {
	vdynamic *a = hl_alloc_dynamic(&hlt_f64);
	char buf[64];
	a->v.d = arg;
	snprintf(buf, sizeof(buf), "heap-f64:%.17g@%p", arg, (void*)a);
	return invoke_common(idx, a, buf);
}
DEFINE_PRIM(_DYN, invoke_f, _I32 _F64);

// Same as invoke(), but the boxed Int argument is GC-HEAP allocated through
// hl_alloc_dynamic instead of living on the native stack. Discriminates
// "engine cannot render a non-GC vdynamic" from "engine does not recognise
// libhl's hlt_i32 singleton".
HL_PRIM vdynamic *HL_NAME(invoke_heap)( int idx, int arg ) {
	vdynamic *a = hl_alloc_dynamic(&hlt_i32);
	char buf[48];
	a->v.i = arg;
	snprintf(buf, sizeof(buf), "heap-i32:%d@%p", arg, (void*)a);
	return invoke_common(idx, a, buf);
}
DEFINE_PRIM(_DYN, invoke_heap, _I32 _I32);

// invoke with a NULL argument -- Dyn->Dyn closures must tolerate it.
HL_PRIM vdynamic *HL_NAME(invoke_null)( int idx ) {
	return invoke_common(idx, NULL, "null");
}
DEFINE_PRIM(_DYN, invoke_null, _I32);
