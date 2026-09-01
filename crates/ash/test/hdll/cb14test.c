// CASE #14 -- closure invoked from a FOREIGN (non-Haxe) native thread.
//
// This is the exact hxDatachannel shape: libdatachannel's own worker threads
// are not Haxe threads and were never created by the runtime. They hold a
// vclosure* that Haxe handed over earlier, stored in a malloc'd struct with no
// GC root, and they call hl_dyn_call(*res->closure, args, n) on it.
//
// Two phases:
//   SERIAL     : the Haxe thread is parked inside a native call while the
//                foreign thread does every hl_dyn_call and is then joined.
//   CONCURRENT : the foreign thread runs hl_dyn_call while the Haxe thread is
//                simultaneously executing Haxe code and churning the heap.
//
// Env knobs (all default off; getenv("")=="" still counts as set, use `env -u`)
//   CB14_ROOT=slot     hl_add_root(&stash->cb)   -- upstream HashLink spelling
//   CB14_ROOT=object   hl_add_root(stash->cb)    -- what ash's hl_add_root wants
//   CB14_REGISTER=1    foreign thread calls hl_register_thread(&stack_top)
//                      and hl_unregister_thread() before exiting
//   CB14_HLP=1         call hlp_dyn_call instead of hl_dyn_call
//   CB14_VERBOSE=1     print every individual call

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>

#define HL_NAME(n) cb14test_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

#define CB14_MAGIC 0x14C0DE14

typedef struct {
	int       magic;
	vclosure *cb;            // Int -> Int
	void     *fun_at_store;
	void     *t_at_store;
} cb14_stash;

static cb14_stash *g_st = NULL;

static pthread_t g_thread;
static int       g_thread_live   = 0;
static long      g_n             = 0;
static int       g_sum           = 0;      // accumulated 3i+1
static int       g_calls_ok      = 0;
static int       g_calls_null    = 0;
static int       g_calls_wrong   = 0;
static int       g_first_wrong_i = 0;
static int       g_first_wrong_v = 0;
static int       g_fun_changed   = 0;
static int       g_registered    = 0;
static int       g_slow          = 0;      // interleave with the Haxe thread

static int env_on( const char *k ) { return getenv(k) != NULL; }

// ---------------------------------------------------------------------------
// store: keep the raw vclosure* in a malloc'd struct, exactly like the real
// library. Optionally register a GC root in either spelling.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	const char *mode;
	if( g_st == NULL ) g_st = (cb14_stash*)malloc(sizeof(cb14_stash));
	g_st->magic        = CB14_MAGIC;
	g_st->cb           = c;
	g_st->fun_at_store = c ? c->fun : NULL;
	g_st->t_at_store   = c ? (void*)c->t : NULL;

	mode = getenv("CB14_ROOT");
	if( mode && strcmp(mode,"slot") == 0 ) {
		printf("[cb14] root    : hl_add_root(&stash->cb) slot=%p (holds %p)\n",
			(void*)&g_st->cb, (void*)c);
		hl_add_root(&g_st->cb);
	} else if( mode && strcmp(mode,"object") == 0 ) {
		printf("[cb14] root    : hl_add_root(closure) = %p\n", (void*)c);
		hl_add_root(c);
	} else {
		printf("[cb14] root    : none\n");
	}
	printf("[cb14] store   : vclosure=%p t=%p fun=%p hasValue=%d\n",
		(void*)c, c ? (void*)c->t : NULL, c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store, _FUN(_I32,_I32));

// ---------------------------------------------------------------------------
// the foreign thread body: hl_dyn_call in a loop, from a thread the runtime
// did not create.
// ---------------------------------------------------------------------------
static void *cb14_worker( void *p ) {
	long   i;
	long   n = (long)p;
	char   stack_top;
	int    use_hlp = env_on("CB14_HLP");
	int    verbose = env_on("CB14_VERBOSE");

	g_sum = 0; g_calls_ok = 0; g_calls_null = 0; g_calls_wrong = 0;
	g_first_wrong_i = 0; g_first_wrong_v = 0; g_fun_changed = 0;

	if( env_on("CB14_REGISTER") ) {
		g_registered = 1;
		printf("[cb14] thread  : hl_register_thread(%p)\n", (void*)&stack_top);
		fflush(stdout);
		hl_register_thread(&stack_top);
	} else {
		g_registered = 0;
		printf("[cb14] thread  : NOT registered with the GC\n");
		fflush(stdout);
	}

	printf("[cb14] thread  : foreign tid=%p starting %ld calls via %s\n",
		(void*)pthread_self(), n, use_hlp ? "hlp_dyn_call" : "hl_dyn_call");
	fflush(stdout);

	for( i = 1; i <= n; i++ ) {
		vclosure *c;
		vdynamic  a;
		vdynamic *args[1];
		vdynamic *ret;
		int       expect = (int)i * 3 + 1;

		if( g_st == NULL || g_st->magic != CB14_MAGIC ) {
			printf("[cb14] thread  : stash gone at i=%ld\n", i);
			fflush(stdout);
			break;
		}
		c = g_st->cb;
		if( c == NULL ) { g_calls_null++; continue; }
		if( (void*)c->fun != g_st->fun_at_store ) {
			if( !g_fun_changed )
				printf("[cb14] MUTATED : i=%ld fun %p -> %p (t %p -> %p)\n",
					i, g_st->fun_at_store, (void*)c->fun,
					g_st->t_at_store, (void*)c->t);
			g_fun_changed++;
			fflush(stdout);
		}

		a.t = &hlt_i32;
		a.v.i = (int)i;
		args[0] = &a;

		ret = use_hlp ? hlp_dyn_call(c, args, 1) : hl_dyn_call(c, args, 1);

		if( ret == NULL ) {
			g_calls_null++;
			if( g_calls_null == 1 ) {
				printf("[cb14] NULLRET : i=%ld returned NULL\n", i);
				fflush(stdout);
			}
		} else if( ret->v.i != expect ) {
			g_calls_wrong++;
			if( g_calls_wrong == 1 ) {
				g_first_wrong_i = (int)i;
				g_first_wrong_v = ret->v.i;
				printf("[cb14] WRONG   : i=%ld got %d expected %d (ret=%p t=%p kind=%d)\n",
					i, ret->v.i, expect, (void*)ret, (void*)ret->t,
					ret->t ? (int)ret->t->kind : -1);
				fflush(stdout);
			}
			g_sum += ret->v.i;
		} else {
			g_calls_ok++;
			g_sum += ret->v.i;
			if( verbose ) {
				printf("[cb14] call    : i=%ld -> %d\n", i, ret->v.i);
				fflush(stdout);
			}
		}
		if( g_slow ) usleep(1000);
	}

	printf("[cb14] thread  : done ok=%d null=%d wrong=%d sum=%d\n",
		g_calls_ok, g_calls_null, g_calls_wrong, g_sum);
	fflush(stdout);

	if( g_registered ) hl_unregister_thread();
	return NULL;
}

// ---------------------------------------------------------------------------
// SERIAL: spawn + join inside one native call. The Haxe thread is parked in
// native code for the whole thing.
// ---------------------------------------------------------------------------
HL_PRIM int HL_NAME(run_serial)( int n ) {
	g_slow = 0;
	g_n = n;
	if( pthread_create(&g_thread, NULL, cb14_worker, (void*)(long)n) != 0 )
		return -1;
	pthread_join(g_thread, NULL);
	return g_sum;
}
DEFINE_PRIM(_I32, run_serial, _I32);

// ---------------------------------------------------------------------------
// CONCURRENT: spawn and return; Haxe keeps running (and allocating) while the
// foreign thread calls back into it.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(spawn)( int n ) {
	g_slow = 1;
	g_n = n;
	if( pthread_create(&g_thread, NULL, cb14_worker, (void*)(long)n) != 0 ) {
		printf("[cb14] spawn   : pthread_create failed\n");
		fflush(stdout);
		g_thread_live = 0;
		return;
	}
	g_thread_live = 1;
}
DEFINE_PRIM(_VOID, spawn, _I32);

HL_PRIM int HL_NAME(join)() {
	if( !g_thread_live ) return -1;
	pthread_join(g_thread, NULL);
	g_thread_live = 0;
	return g_sum;
}
DEFINE_PRIM(_I32, join, _NO_ARG);

HL_PRIM int HL_NAME(stat)( int which ) {
	switch( which ) {
	case 0: return g_calls_ok;
	case 1: return g_calls_null;
	case 2: return g_calls_wrong;
	case 3: return g_fun_changed;
	case 4: return g_first_wrong_i;
	case 5: return g_first_wrong_v;
	}
	return -999;
}
DEFINE_PRIM(_I32, stat, _I32);
