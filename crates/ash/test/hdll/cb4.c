// cb4.hdll -- CASE #4: hl_dyn_call with STRING and BYTES (pointer-typed) args.
//
// Shape (mirrors hxDatachannel's onMessage(String)/onBinary(Bytes) pumps):
//
//   1. Haxe hands the native library a String and an hl.Bytes; the library
//      keeps the RAW GC pointers in a malloc'd struct. No GC root.
//   2. Haxe hands the library a closure (String, hl.Bytes, Int) -> String.
//      Again the raw vclosure* goes in the malloc'd struct.
//   3. Haxe churns the heap hard.
//   4. Haxe calls invoke(); the library boxes the stored String / Bytes into
//      vdynamic args and calls hl_dyn_call(c, args, 3), then hands the
//      resulting String back to Haxe.
//
// Every one of those three stored pointers must survive marking. If the
// collector cannot see them, the string bytes / closure are reclaimed and the
// result is wrong (or the process dies).
//
// Env switches (all default OFF; `env -u VAR` to clear -- getenv("")!=NULL):
//   CB4_HLP=1        route through hlp_dyn_call instead of hl_dyn_call
//   CB4_ROOT=object  hl_add_root(closure), hl_add_root(str), hl_add_root(bytes)
//   CB4_ROOT=slot    hl_add_root(&g->cb), hl_add_root(&g->str), ... (upstream)
//   CB4_MAKESTR=1    build a FRESH String natively (hl_gc_alloc_gen) inside
//                    invoke() instead of using the stored one
//   CB4_NARGS=n      override the argument count handed to hl_dyn_call

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define HL_NAME(n) cb4_##n
#include "hl.h"

HL_API vdynamic *hlp_dyn_call( vclosure *c, vdynamic **args, int nargs );

typedef struct {
	int       magic;
	vclosure *cb;        // (String, hl.Bytes, Int) -> String
	vstring  *str;       // String kept from Haxe
	vbyte    *bytes;     // hl.Bytes kept from Haxe
	int       blen;
	hl_type  *strtype;   // captured from str->t, used for the return/box type
	void     *cb_fun_at_store;
	uchar    *str_bytes_at_store;
	int       str_len_at_store;
} stash4;

#define STASH4_MAGIC 0x0CB40004

static stash4 *g4 = NULL;

static stash4 *g4_get( void ) {
	if( g4 == NULL ) {
		g4 = (stash4*)malloc(sizeof(stash4));
		memset(g4, 0, sizeof(stash4));
		g4->magic = STASH4_MAGIC;
	}
	return g4;
}

static const char *root_mode( void ) {
	const char *m = getenv("CB4_ROOT");
	return m ? m : "";
}

// ASCII-only dumper: deliberately does NOT allocate (no hl_to_utf8), so the
// diagnostics cannot themselves perturb the collector.
static void dump_ustr( const char *tag, uchar *u, int len ) {
	char buf[257];
	int i, n;
	if( u == NULL ) { printf("[cb4] %-12s ptr=%p <NULL bytes>\n", tag, (void*)u); fflush(stdout); return; }
	n = len; if( n < 0 ) n = 0; if( n > 256 ) n = 256;
	for( i = 0; i < n; i++ ) {
		int c = (int)u[i];
		buf[i] = (c >= 32 && c < 127) ? (char)c : '?';
	}
	buf[n] = 0;
	printf("[cb4] %-12s ptr=%p len=%d \"%s\"\n", tag, (void*)u, len, buf);
	fflush(stdout);
}

static void dump_bytes( const char *tag, vbyte *b, int len ) {
	char buf[257];
	int i, n;
	if( b == NULL ) { printf("[cb4] %-12s ptr=%p <NULL>\n", tag, (void*)b); fflush(stdout); return; }
	n = len; if( n < 0 ) n = 0; if( n > 256 ) n = 256;
	for( i = 0; i < n; i++ ) {
		int c = (int)b[i];
		buf[i] = (c >= 32 && c < 127) ? (char)c : '?';
	}
	buf[n] = 0;
	printf("[cb4] %-12s ptr=%p len=%d \"%s\" [", tag, (void*)b, len, buf);
	for( i = 0; i < n && i < 16; i++ ) printf("%s%d", i ? "," : "", (int)b[i]);
	printf("]\n");
	fflush(stdout);
}

// ---------------------------------------------------------------------------
// keep_str: stash a String (HOBJ) pointer received from Haxe.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(keep_str)( vstring *s ) {
	stash4 *g = g4_get();
	g->str = s;
	g->strtype = s ? s->t : NULL;
	g->str_bytes_at_store = s ? s->bytes : NULL;
	g->str_len_at_store = s ? s->length : -1;
	printf("[cb4] keep_str    vstring=%p t=%p\n", (void*)s, s ? (void*)s->t : NULL);
	if( s ) dump_ustr("  str@store", s->bytes, s->length);
	if( strcmp(root_mode(),"object") == 0 ) {
		printf("[cb4] root       hl_add_root(str=%p)\n", (void*)s); fflush(stdout);
		hl_add_root(s);
	} else if( strcmp(root_mode(),"slot") == 0 ) {
		printf("[cb4] root       hl_add_root(&g->str=%p)\n", (void*)&g->str); fflush(stdout);
		hl_add_root(&g->str);
	}
	fflush(stdout);
}
DEFINE_PRIM(_VOID, keep_str, _STRING);

// ---------------------------------------------------------------------------
// keep_bytes: stash an hl.Bytes (HBYTES) pointer received from Haxe.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(keep_bytes)( vbyte *b, int len ) {
	stash4 *g = g4_get();
	g->bytes = b;
	g->blen = len;
	printf("[cb4] keep_bytes  vbyte=%p len=%d\n", (void*)b, len);
	dump_bytes("  bytes@store", b, len);
	if( strcmp(root_mode(),"object") == 0 ) {
		printf("[cb4] root       hl_add_root(bytes=%p)\n", (void*)b); fflush(stdout);
		hl_add_root(b);
	} else if( strcmp(root_mode(),"slot") == 0 ) {
		printf("[cb4] root       hl_add_root(&g->bytes=%p)\n", (void*)&g->bytes); fflush(stdout);
		hl_add_root(&g->bytes);
	}
	fflush(stdout);
}
DEFINE_PRIM(_VOID, keep_bytes, _BYTES _I32);

// ---------------------------------------------------------------------------
// store: stash the callback closure.
// ---------------------------------------------------------------------------
HL_PRIM void HL_NAME(store)( vclosure *c ) {
	stash4 *g = g4_get();
	g->cb = c;
	g->cb_fun_at_store = c ? c->fun : NULL;
	printf("[cb4] store       vclosure=%p t=%p fun=%p hasValue=%d\n",
		(void*)c, c ? (void*)c->t : NULL, c ? (void*)c->fun : NULL,
		c ? (int)c->hasValue : -1);
	if( strcmp(root_mode(),"object") == 0 ) {
		printf("[cb4] root       hl_add_root(closure=%p)\n", (void*)c); fflush(stdout);
		hl_add_root(c);
	} else if( strcmp(root_mode(),"slot") == 0 ) {
		printf("[cb4] root       hl_add_root(&g->cb=%p)\n", (void*)&g->cb); fflush(stdout);
		hl_add_root(&g->cb);
	}
	fflush(stdout);
}
DEFINE_PRIM(_VOID, store, _FUN(_STRING, _STRING _BYTES _I32));

// ---------------------------------------------------------------------------
// invoke: box the stored pointer-typed values and call the closure.
// ---------------------------------------------------------------------------
HL_PRIM vstring *HL_NAME(invoke)( void ) {
	stash4 *g = g4;
	vdynamic  a0, a1, a2;
	vdynamic *args[3];
	vdynamic *ret;
	vstring  *sarg;
	int nargs = 3;
	const char *nv;

	if( g == NULL || g->magic != STASH4_MAGIC ) {
		printf("[cb4] invoke     nothing stored\n"); fflush(stdout);
		return NULL;
	}

	printf("[cb4] invoke     vclosure=%p fun=%p (fun@store=%p) hasValue=%d\n",
		(void*)g->cb, g->cb ? (void*)g->cb->fun : NULL, g->cb_fun_at_store,
		g->cb ? (int)g->cb->hasValue : -1);
	fflush(stdout);
	if( g->cb == NULL ) return NULL;

	sarg = g->str;
	if( getenv("CB4_MAKESTR") ) {
		// Build a fresh String natively. Only the C stack holds it while
		// hl_dyn_call does its own allocating.
		static const char *lit = "native-made";
		int n = (int)strlen(lit), i;
		uchar *u = (uchar*)hl_gc_alloc_gen(&hlt_bytes, (n + 1) * (int)sizeof(uchar), 0);
		vstring *s = (vstring*)hl_gc_alloc_gen(g->strtype, (int)sizeof(vstring), 0);
		for( i = 0; i < n; i++ ) u[i] = (uchar)lit[i];
		u[n] = 0;
		s->t = g->strtype;
		s->bytes = u;
		s->length = n;
		sarg = s;
		printf("[cb4] makestr    fresh vstring=%p t=%p\n", (void*)s, (void*)s->t);
		fflush(stdout);
	}

	printf("[cb4] pre-call    str=%p (bytes@store=%p len@store=%d) bytes=%p blen=%d strtype=%p\n",
		(void*)sarg, (void*)g->str_bytes_at_store, g->str_len_at_store,
		(void*)g->bytes, g->blen, (void*)g->strtype);
	fflush(stdout);
	if( sarg ) {
		printf("[cb4] pre-call    str->t=%p str->length=%d\n", (void*)sarg->t, sarg->length);
		fflush(stdout);
		dump_ustr("  str@call", sarg->bytes, sarg->length);
	}
	dump_bytes("  bytes@call", g->bytes, g->blen);

	// HOBJ is one of HashLink's "dynamic" kinds: the object already starts
	// with an hl_type*, so an obj pointer IS a vdynamic* and must be passed
	// straight through -- hl_dyn_castp(HDYN -> HOBJ) returns the pointer
	// itself, it does not unwrap a box. CB4_STRBOX=1 selects the other
	// (wrong, but very natural) spelling so the two can be compared.
	// HBYTES is NOT dynamic, so it must be boxed.
	if( getenv("CB4_STRBOX") ) {
		a0.t = g->strtype;
		a0.v.ptr = sarg;
		args[0] = &a0;
		printf("[cb4] argmode    string BOXED (a0.t=strtype, a0.v.ptr=str)\n");
	} else {
		args[0] = (vdynamic*)sarg;
		printf("[cb4] argmode    string DIRECT (args[0] = vstring*)\n");
	}
	fflush(stdout);
	a1.t = &hlt_bytes;
	a1.v.bytes = g->bytes;
	a2.t = &hlt_i32;
	a2.v.i = g->blen;
	args[1] = &a1;
	args[2] = &a2;

	nv = getenv("CB4_NARGS");
	if( nv ) nargs = atoi(nv);

	if( getenv("CB4_HLP") ) {
		printf("[cb4] path       hlp_dyn_call nargs=%d\n", nargs); fflush(stdout);
		ret = hlp_dyn_call(g->cb, args, nargs);
	} else {
		printf("[cb4] path       hl_dyn_call nargs=%d\n", nargs); fflush(stdout);
		ret = hl_dyn_call(g->cb, args, nargs);
	}

	if( ret == NULL ) {
		printf("[cb4] result     dyn_call returned NULL\n"); fflush(stdout);
		return NULL;
	}
	printf("[cb4] result     vdynamic=%p t=%p kind=%d v.ptr=%p\n",
		(void*)ret, (void*)ret->t, ret->t ? (int)ret->t->kind : -1, ret->v.ptr);
	fflush(stdout);
	{
		// Two shapes are possible depending on which engine ran the call:
		//   boxed   : ret->t == String type, ret->v.ptr == vstring*
		//   unboxed : ret IS the vstring (HOBJ is dynamic)
		// Pick whichever actually has the String type in its header.
		vstring *boxed = (vstring*)ret->v.ptr;
		vstring *rs = (vstring*)ret;
		if( boxed != NULL && ((uintptr_t)boxed & 7) == 0 && g->strtype != NULL
		    && boxed->t == g->strtype ) {
			rs = boxed;
			printf("[cb4] result     shape=BOXED\n");
		} else {
			printf("[cb4] result     shape=UNBOXED (ret is the vstring)\n");
		}
		fflush(stdout);
		if( rs == NULL ) { printf("[cb4] result     <null string>\n"); fflush(stdout); return NULL; }
		printf("[cb4] result     vstring=%p t=%p length=%d\n", (void*)rs, (void*)rs->t, rs->length);
		dump_ustr("  ret@c", rs->bytes, rs->length);
		return rs;
	}
}
DEFINE_PRIM(_STRING, invoke, _NO_ARG);
