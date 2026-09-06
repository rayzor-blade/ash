#!/usr/bin/env python3
"""Generate the wasm sdl shim from the signatures the real library reports.

The primitives, and the exact type of every argument, come from `sdl.hdll`
itself -- see `crates/ash/tests/native_signatures.rs` for why that is the
authority and the Haxe externs are not. Sixty-seven functions written by hand
would be sixty-seven chances to get an argument wrong; written from the
library's own answer they are wrong only if the extractor is.

    ASH_SIGNATURES_LIB=examples/heaps_base2d/bin/sdl.hdll \\
    ASH_SIGNATURES_NAMES=<names.txt> \\
      cargo test -p ash_core --test native_signatures -- --nocapture > sigs.txt
    scripts/generate_sdl_shim.py sigs.txt > crates/tinysdl/src/generated.rs
    scripts/generate_sdl_shim.py --host sigs.txt \
      > crates/ash_wasm_runtime/src/native/sdl_generated.rs

Both results are committed, because generating them needs the .hdll and a
machine that can load it, and neither is true of everyone who builds ash.

`--host` emits the other side of the same sixty-seven: the bindings that let a
host answer them. They funnel into one method, so the arithmetic of arities
and argument types is generated and the BEHAVIOUR -- what a window is, what a
frame is -- is written once, by hand, in `native/sdl.rs`.
"""

import re
import sys

# HL type to (what the shim's exported function takes, how it reaches the
# host). The split is the point: HL's boxing is the shim's business, and the
# host sees plain values.
#
#   null<i32>  a BOXED int -- a `vdynamic*`, not an int. Unwrapped on the way
#              out and boxed again on the way back.
#   obj(...)   `_STRING`: a `vstring*`, whose bytes are UTF-16.
#   ref<i32>   an out-parameter the callee writes through.
#   abstract   an opaque pointer this library minted, so its meaning is ours.
ARG = {
    "i32":            ("i32",             ["{n}"]),
    "f64":            ("f64",             ["{n}"]),
    "f32":            ("f32",             ["{n} as f64"]),
    "bool":           ("bool",            ["{n} as i32"]),
    "bytes":          ("*mut vbyte",      ["{n} as i32"]),
    "dyn":            ("*mut vdynamic",   ["{n} as i32"]),
    "array":          ("*mut varray",     ["{n} as i32"]),
    "null<i32>":      ("*mut vdynamic",   ["unbox_i32({n})"]),
    "ref<i32>":       ("*mut i32",        ["{n} as i32"]),
    "obj(bytes, i32)": ("*mut vstring",   ["string_bytes({n})", "string_length({n})"]),
}
# A callback: `vclosure*`, forwarded as the pointer it is. A host that wants
# to call one back has to go through the runtime, which is why nothing here
# tries.
CLOSURE = ("*mut c_void", ["{n} as i32"])
ABSTRACT = ("*mut c_void", ["{n} as i32"])

RET = {
    "void":      ("()",             None),
    "i32":       ("i32",            "{c}"),
    "f64":       ("f64",            "{c}"),
    "bool":      ("bool",           "{c} != 0"),
    "bytes":     ("*mut vbyte",     "{c} as *mut vbyte"),
    # A query answering `Dynamic` answers a VALUE, so it is boxed like any
    # other. Casting the host's integer to a pointer would hand the VM an
    # address to dereference.
    "dyn":       ("*mut vdynamic",  "box_i32({c})"),
    "null<i32>": ("*mut vdynamic",  "box_i32({c})"),
    # A host with no screen has no displays and no devices to list. The call
    # still goes out, so a trace shows the program asked.
    "array":     ("*mut varray",    "{{ let _ = {c}; empty_array() }}"),
}
RET_ABSTRACT = ("*mut c_void", "{c} as *mut c_void")


def split_args(text):
    """Arguments of `(a, b) -> r`, respecting the nesting in `obj(...)`."""
    out, depth, cur = [], 0, ""
    for ch in text:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == "," and depth == 0:
            out.append(cur.strip())
            cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


def host_type(hl):
    """Every pointer is an i32 to the host: that is what a wasm address is."""
    return {"f64": "f64", "f32": "f64"}.get(hl, "i32")


def emit_host(rows, lib):
    """The host side: one `func_wrap` per primitive, all funnelling into
    `Sdl::call`."""
    print(HOST_HEADER)
    for name, raw, args, ret in rows:
        if name in MANUAL:
            continue
        params, pass_args = [], []
        for i, a in enumerate(args):
            n = f"a{i}"
            if a in ("f64", "f32"):
                # f64 either way: the library widens an f32 before it
                # forwards, so both sides agree without the host having to
                # know which primitives use the narrower type.
                params.append(f"{n}: f64")
                pass_args.append(f"Arg::D({n})")
            elif "->" in a:
                params.append(f"{n}: i32")
                pass_args.append(f"Arg::I({n})")
            elif a == "bool":
                # The guest hands a bool across as an i32, since wasm has no
                # narrower type to put it in.
                params.append(f"{n}: i32")
                pass_args.append(f"Arg::I({n})")
            elif a == "obj(bytes, i32)":
                params += [f"{n}: i32", f"{n}_len: i32"]
                pass_args += [f"Arg::I({n})", f"Arg::I({n}_len)"]
            else:
                params.append(f"{n}: i32")
                pass_args.append(f"Arg::I({n})")
        rty = "" if ret == "void" else (" -> f64" if ret == "f64" else " -> i32")
        call = f'''caller.data_mut().sdl.call("{lib}@{name}", &[{", ".join(pass_args)}])'''
        if ret == "void":
            body = f"{{ {call}; }}"
        elif ret == "f64":
            body = f"{{ f64::from_bits({call} as u64) }}"
        else:
            body = f"{{ {call} as i32 }}"
        print(HOST_BINDING.format(
            lib=lib, name=name,
            params="".join(f", {p}" for p in params),
            ret=rty,
            body=body,
        ))
    print(HOST_FOOTER)


# Primitives whose shape the generator cannot express: they allocate in the
# guest and are filled by the host, so both sides are written by hand.
MANUAL = {"gl_get_string", "win_swap_window"}


def library():
    for i, a in enumerate(sys.argv):
        if a == "--lib":
            return sys.argv[i + 1]
    return "sdl"


def main():
    lib = library()
    rows = []
    argv = sys.argv[1:]
    paths = [a for i, a in enumerate(argv)
             if not a.startswith("--") and not (i > 0 and argv[i - 1] == "--lib")]
    if not paths:
        sys.exit("give it the signature table")
    for line in open(paths[0]):
        parts = line.rstrip("\n").split("\t")
        if len(parts) != 3 or parts[2].startswith("("):
            pass
        if len(parts) != 3:
            continue
        name, raw, decoded = parts
        m = re.match(r"^\((.*)\) -> (.+)$", decoded)
        if not m:
            print(f"// unparsed: {name} {decoded}", file=sys.stderr)
            continue
        rows.append((name, raw, split_args(m.group(1)) if m.group(1) else [], m.group(2)))
    rows.sort()

    if "--host" in sys.argv:
        emit_host(rows, lib)
        return

    print(HEADER)
    for name, raw, args, ret in rows:
        if name in MANUAL:
            continue
        rust_args, forward, host_args = [], [], []
        for i, a in enumerate(args):
            n = f"a{i}"
            if a.startswith("abstract<"):
                ty, conv = ABSTRACT
            elif "->" in a:
                ty, conv = CLOSURE
            else:
                ty, conv = ARG.get(a, (None, None))
            if ty is None:
                print(f"// unmapped argument {a} in {name}", file=sys.stderr)
                break
            rust_args.append(f"{n}: {ty}")
            for c in conv:
                forward.append(c.format(n=n))
                host_args.append(host_type(a) if len(conv) == 1 else "i32")
        else:
            rty, wrap = RET_ABSTRACT if ret.startswith("abstract<") else RET.get(ret, (None, None))
            if rty is None:
                print(f"// unmapped return {ret} in {name}", file=sys.stderr)
                continue
            host_ret = "" if ret == "void" else (
                " -> f64" if ret == "f64" else " -> i32")
            print(EXTERN.format(
                lib=lib, name=name,
                host_params=", ".join(f"{c}: {t}" for c, t in
                                      zip([f"a{i}" for i in range(len(host_args))], host_args)),
                host_ret=host_ret,
            ))
            body = f"ash_host_{lib}_{name}({', '.join(forward)})"
            if ret == "void":
                body = f"{body};"
            elif wrap:
                body = wrap.format(c=body)
            print(FUNCTION.format(
                name=name, raw=raw,
                params=", ".join(rust_args),
                ret="" if ret == "void" else f" -> {rty}",
                body=body,
            ))


HEADER = '''//! The `sdl` library for wasm: every primitive the Heaps base3d example
//! reaches, forwarding to the host.
//!
//! GENERATED by `scripts/generate_sdl_shim.py` from the signatures `sdl.hdll`
//! reports for itself. Edit the generator, not this file.
//!
//! Nothing here decides anything. A window, a GL context and a frame belong
//! to whoever is embedding the program -- a canvas in a page, or a recorder
//! under a headless host -- so every function is the same shape: unwrap what
//! HashLink boxed, hand the plain values to the host, box whatever comes
//! back.

use std::ffi::c_void;

// Not every library needs all of it: `ui` boxes nothing.
#[allow(unused_imports)]
use crate::abi::*;
'''

EXTERN = '''
#[link(wasm_import_module = "env")]
extern "C" {{
    fn ash_host_{lib}_{name}({host_params}){host_ret};
}}'''

FUNCTION = '''
/// `{raw}`
///
/// # Safety
/// Called by the VM through the resolver below, with the arguments the
/// signature above declares.
#[no_mangle]
pub unsafe extern "C" fn sdl_{name}({params}){ret} {{
    {body}
}}
define_prim!(hlp_{name}, sdl_{name}, "{raw}");'''


HOST_HEADER = '''//! Answering the sixty-seven imports `sdl.wasm` declares.
//!
//! GENERATED by `scripts/generate_sdl_shim.py --host` from the signatures
//! `sdl.hdll` reports for itself, so this side and the library's side cannot
//! disagree about an argument. Edit the generator, not this file.
//!
//! Every binding funnels into [`super::sdl::Sdl::call`], which is where a
//! window and a frame are actually decided. Nothing is decided here.

use wasmtime::{Caller, Linker};

use super::sdl::Arg;
use super::{Host, FIBER_YIELD_MODULE};

/// Install all sixty-seven.
pub(crate) fn install(linker: &mut Linker<Host>) -> anyhow::Result<()> {'''

HOST_BINDING = '''    linker.func_wrap(
        FIBER_YIELD_MODULE,
        "ash_host_{lib}_{name}",
        |mut caller: Caller<\'_, Host>{params}|{ret} {body},
    )?;'''

HOST_FOOTER = '''    Ok(())
}'''


if __name__ == "__main__":
    main()
