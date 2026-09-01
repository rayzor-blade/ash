// cb5test.hdll -- CASE #5: OBJECT ARGUMENT through hl_dyn_call.
//
// Shape (the hxDatachannel pattern, specialised to object arguments):
//
//   1. Haxe hands the native library two closures and one class instance.
//        cb_int : Cell -> Int    (mutates the instance in place)
//        cb_obj : Cell -> Cell   (allocates and returns a NEW instance)
//        obj    : Cell           (a plain Haxe class instance)
//   2. The library stores all three raw pointers in a malloc'd struct and
//      returns. No GC root is registered by default -- exactly like
//      hxDatachannel, which just relies on the pointers staying valid.
//   3. Haxe churns the heap.
//   4. The library calls hl_dyn_call(cb_int, { (vdynamic*)obj }, 1). The
//      class instance travels through the Dynamic argument slot and the
//      callee MUTATES it.
//   5. Haxe fetches the same instance back out of the native struct and
//      checks that the mutation is visible.
//   6. The library calls hl_dyn_call(cb_obj, { (vdynamic*)obj }, 1), which
//      returns a pointer-typed value. hl_dyn_call boxes a non-dynamic ptr
//      return into a vdynamic whose v.ptr is the object, so the library
//      unwraps it and hands the raw vobj* back to Haxe.
//
// Diagnostic env switches (all default off; `env -u VAR` to clear -- an empty
// value still counts as set to getenv):
//   CB5_HLP=1        call hlp_dyn_call instead of hl_dyn_call
//   CB5_ROOT=object  hl_add_root(closure) / hl_add_root(obj)   -- ash spelling
//   CB5_ROOT=slot    hl_add_root(&stash->cb) / (&stash->obj)   -- upstream
//                    HashLink spelling: the address of the SLOT
//   CB5_DUMP=1       hexdump the first 32 bytes of the stored object

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HL_NAME(n) cb5test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

typedef struct {
	int       magic;
	vclosure *cb_int;   // Cell -> Int  (mutates in place)
	vclosure *cb_obj;   // Cell -> Cell (returns a fresh instance)
	vdynamic *obj;      // the Cell instance itself (HOBJ => vobj* is the Dynamic)
	vdynamic *obj2;     // whatever cb_obj returned
	void     *obj_at_store;
	void     *t_at_store;
} cb5_stash;

#define CB5_MAGIC 0x0B5EC0DE

static cb5_stash *g5 = NULL;

static void cb5_dump( const char *tag, vdynamic *o ) {
	unsigned char *p;
	int i;
	if( !getenv("CB5_DUMP") ) return;
	if( o == NULL ) { printf("[cb5] %-8s: obj=NULL\n", tag); fflush(stdout); return; }
	p = (unsigned char*)o;
	printf("[cb5] %-8s: obj=%p bytes:", tag, (void*)o);
	for( i = 0; i < 32; i++ ) printf(" %02x", p[i]);
	printf("\n");
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// store: keep the two closures and the class instance, no GC root by default.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *ci, vclosure *co, vdynamic *o ) {
	const char *mode;
	if( g5 == NULL ) g5 = (cb5_stash*)malloc(sizeof(cb5_stash));
	memset(g5, 0, sizeof(cb5_stash));
	g5->magic = CB5_MAGIC;
	g5->cb_int = ci;
	g5->cb_obj = co;
	g5->obj = o;
	g5->obj_at_store = (void*)o;
	g5->t_at_store = o ? (void*)o->t : NULL;

	mode = getenv("CB5_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb5] root    : hl_add_root(&stash->cb_int)=%p (&obj)=%p  [upstream slot spelling]\n",
			(void*)&g5->cb_int, (void*)&g5->obj);
		fflush(stdout);
		hl_add_root(&g5->cb_int);
		hl_add_root(&g5->cb_obj);
		hl_add_root(&g5->obj);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb5] root    : hl_add_root(cb_int)=%p hl_add_root(obj)=%p  [ash object spelling]\n",
			(void*)ci, (void*)o);
		fflush(stdout);
		hl_add_root(ci);
		hl_add_root(co);
		hl_add_root(o);
	}

	printf("[cb5] store   : cb_int=%p (fun=%p hasValue=%d) cb_obj=%p (fun=%p) obj=%p obj->t=%p\n",
		(void*)ci, ci ? (void*)ci->fun : NULL, ci ? (int)ci->hasValue : -1,
		(void*)co, co ? (void*)co->fun : NULL,
		(void*)o, o ? (void*)o->t : NULL);
	fflush(stdout);
	cb5_dump("store", o);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_DYN) _FUN(_DYN,_DYN) _DYN);

// ---------------------------------------------------------------------------
// invoke: hl_dyn_call(cb_int, { obj }, 1) -- object travels as the Dynamic arg
// and the callee mutates it in place.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(invoke)( void ) {
	vdynamic *args[1];
	vdynamic *ret;

	if( g5 == NULL || g5->magic != CB5_MAGIC ) { printf("[cb5] invoke  : nothing stored\n"); fflush(stdout); return -1; }
	printf("[cb5] invoke  : cb_int=%p obj=%p (obj at store=%p) obj->t=%p (t at store=%p)\n",
		(void*)g5->cb_int, (void*)g5->obj, g5->obj_at_store,
		g5->obj ? (void*)g5->obj->t : NULL, g5->t_at_store);
	fflush(stdout);
	cb5_dump("pre", g5->obj);
	if( g5->cb_int == NULL || g5->obj == NULL ) return -2;

	args[0] = g5->obj;
	if( getenv("CB5_HLP") ) {
		printf("[cb5] path    : hlp_dyn_call\n"); fflush(stdout);
		ret = hlp_dyn_call(g5->cb_int, args, 1);
	} else {
		printf("[cb5] path    : hl_dyn_call\n"); fflush(stdout);
		ret = hl_dyn_call(g5->cb_int, args, 1);
	}
	cb5_dump("post", g5->obj);

	if( ret == NULL ) { printf("[cb5] result  : hl_dyn_call returned NULL\n"); fflush(stdout); return -3; }
	printf("[cb5] result  : vdynamic=%p t=%p kind=%d v.i=%d\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.i);
	fflush(stdout);
	return ret->v.i;
}
DEFINE_PRIM(_I32, invoke, _NO_ARG);

// ---------------------------------------------------------------------------
// invoke_obj: hl_dyn_call(cb_obj, { obj }, 1). The callee returns a freshly
// allocated instance. A ptr-typed, non-dynamic return is boxed by hl_dyn_call
// (upstream does the same), so unwrap v.ptr before handing it back to Haxe.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(invoke_obj)( void ) {
	vdynamic *args[1];
	vdynamic *ret;

	if( g5 == NULL || g5->magic != CB5_MAGIC ) { printf("[cb5] invokeO : nothing stored\n"); fflush(stdout); return; }
	if( g5->cb_obj == NULL || g5->obj == NULL ) { printf("[cb5] invokeO : null slot\n"); fflush(stdout); return; }
	printf("[cb5] invokeO : cb_obj=%p obj=%p\n", (void*)g5->cb_obj, (void*)g5->obj);
	fflush(stdout);

	args[0] = g5->obj;
	if( getenv("CB5_HLP") )
		ret = hlp_dyn_call(g5->cb_obj, args, 1);
	else
		ret = hl_dyn_call(g5->cb_obj, args, 1);

	if( ret == NULL ) { printf("[cb5] resultO : NULL\n"); fflush(stdout); g5->obj2 = NULL; return; }
	printf("[cb5] resultO : box=%p box->t=%p kind=%d v.ptr=%p\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.ptr);
	fflush(stdout);
	// hl_call_method only boxes a ptr-typed return when the type is NOT
	// dynamic (HBYTES, HTYPE, HREF, HABSTRACT...). HOBJ *is* dynamic in
	// HashLink's TIsDynamic table, so an object return comes back as the raw
	// vobj* -- ret IS the value. CB5_BOX=1 applies the wrong (unwrap v.ptr)
	// convention on purpose, which reads the object's FIRST FIELD.
	if( getenv("CB5_BOX") ) {
		printf("[cb5] unwrap  : v.ptr (deliberately wrong for a dynamic return)\n"); fflush(stdout);
		g5->obj2 = (vdynamic*)ret->v.ptr;
	} else {
		printf("[cb5] unwrap  : raw (HOBJ is dynamic -> ret is the object)\n"); fflush(stdout);
		g5->obj2 = ret;
	}
	cb5_dump("derived", g5->obj2);
}
DEFINE_PRIM(_VOID, invoke_obj, _NO_ARG);

// ---------------------------------------------------------------------------
// fetch/fetch2: hand the stored instances back to Haxe.
// ---------------------------------------------------------------------------
HL_PRIM vdynamic *HL_NAME(fetch)( void ) {
	if( g5 == NULL || g5->magic != CB5_MAGIC ) return NULL;
	printf("[cb5] fetch   : obj=%p obj->t=%p (t at store=%p)\n",
		(void*)g5->obj, g5->obj ? (void*)g5->obj->t : NULL, g5->t_at_store);
	fflush(stdout);
	cb5_dump("fetch", g5->obj);
	return g5->obj;
}
DEFINE_PRIM(_DYN, fetch, _NO_ARG);

HL_PRIM vdynamic *HL_NAME(fetch2)( void ) {
	if( g5 == NULL || g5->magic != CB5_MAGIC ) return NULL;
	printf("[cb5] fetch2  : obj2=%p obj2->t=%p\n",
		(void*)g5->obj2, g5->obj2 ? (void*)g5->obj2->t : NULL);
	fflush(stdout);
	return g5->obj2;
}
DEFINE_PRIM(_DYN, fetch2, _NO_ARG);
