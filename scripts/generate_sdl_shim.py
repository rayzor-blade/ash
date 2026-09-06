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

The result is committed, because generating it needs the .hdll and a machine
that can load it, and neither is true of everyone who builds ash.
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
    "bool":           ("bool",            ["{n} as i32"]),
    "bytes":          ("*mut vbyte",      ["{n} as i32"]),
    "dyn":            ("*mut vdynamic",   ["{n} as i32"]),
    "array":          ("*mut varray",     ["{n} as i32"]),
    "null<i32>":      ("*mut vdynamic",   ["unbox_i32({n})"]),
    "ref<i32>":       ("*mut i32",        ["{n} as i32"]),
    "obj(bytes, i32)": ("*mut vstring",   ["string_bytes({n})", "string_length({n})"]),
}
ABSTRACT = ("*mut c_void", ["{n} as i32"])

RET = {
    "void":      ("()",             None),
    "i32":       ("i32",            "{c}"),
    "f64":       ("f64",            "{c}"),
    "bool":      ("bool",           "{c} != 0"),
    "bytes":     ("*mut vbyte",     "{c} as *mut vbyte"),
    "dyn":       ("*mut vdynamic",  "{c} as *mut vdynamic"),
    "null<i32>": ("*mut vdynamic",  "box_i32({c})"),
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
    return "f64" if hl == "f64" else "i32"


def main():
    rows = []
    for line in open(sys.argv[1]):
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

    print(HEADER)
    for name, raw, args, ret in rows:
        rust_args, forward, host_args = [], [], []
        for i, a in enumerate(args):
            n = f"a{i}"
            ty, conv = ABSTRACT if a.startswith("abstract<") else ARG.get(a, (None, None))
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
                name=name,
                host_params=", ".join(f"{c}: {t}" for c, t in
                                      zip([f"a{i}" for i in range(len(host_args))], host_args)),
                host_ret=host_ret,
            ))
            body = f"ash_host_sdl_{name}({', '.join(forward)})"
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

use crate::abi::*;
'''

EXTERN = '''
#[link(wasm_import_module = "env")]
extern "C" {{
    fn ash_host_sdl_{name}({host_params}){host_ret};
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


if __name__ == "__main__":
    main()
