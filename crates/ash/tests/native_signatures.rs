//! The signature every primitive of a library declares.
//!
//! Porting a library means writing functions with the same signatures, and
//! guessing them from the Haxe externs is guessing. The `DEFINE_PRIM`
//! protocol already carries the answer: the exported `hlp_<name>` is a
//! RESOLVER that reports the signature through an out-parameter and returns
//! the real function. So the library will say, if asked.
//!
//! Env-gated: it needs a library to ask, and the answer is a starting point
//! for writing code rather than something to assert.
//!
//!     ASH_SIGNATURES_LIB=examples/heaps_base2d/bin/sdl.hdll \
//!     ASH_SIGNATURES_NAMES=/tmp/used.txt \
//!       cargo test -p ash_core --test native_signatures -- --nocapture
//!
//! `ASH_SIGNATURES_NAMES` is a file of primitive names, one per line -- what
//! `ASH_TRACE_NATIVES` reported, with the library prefix stripped. Without
//! it, every `hlp_` symbol the library exports is asked.

use std::ffi::{c_char, c_void, CStr};

/// One type from HashLink's signature grammar, and what is left after it.
///
/// A grammar rather than a list of letters: `_FUN`, `_OBJ`, `_ABSTRACT`,
/// `_REF` and `_NULL` all carry a nested type, and the first three are
/// terminated by `_` -- so an argument list cannot be read one character at a
/// time. `hl.h` is the reference; the letters are its `_I32`, `_BYTES` and
/// the rest.
fn one_type(sig: &str) -> (String, &str) {
    let mut chars = sig.chars();
    let Some(c) = chars.next() else {
        return ("?".to_string(), "");
    };
    let rest = chars.as_str();
    match c {
        'v' => ("void".to_string(), rest),
        'c' => ("i8".to_string(), rest),
        's' => ("i16".to_string(), rest),
        'i' => ("i32".to_string(), rest),
        'l' => ("i64".to_string(), rest),
        'f' => ("f32".to_string(), rest),
        'd' => ("f64".to_string(), rest),
        'b' => ("bool".to_string(), rest),
        'B' => ("bytes".to_string(), rest),
        'D' => ("dyn".to_string(), rest),
        'A' => ("array".to_string(), rest),
        'T' => ("type".to_string(), rest),
        'S' => ("struct".to_string(), rest),
        // A boxed value rather than a raw one: `Null<Int>` is a pointer.
        'N' => {
            let (inner, rest) = one_type(rest);
            (format!("null<{inner}>"), rest)
        }
        'R' => {
            let (inner, rest) = one_type(rest);
            (format!("ref<{inner}>"), rest)
        }
        // An abstract's name may itself contain the `_` that terminates it
        // -- `Xsdl_window_` -- and nothing disambiguates that, because
        // HashLink never parses these strings back. So every candidate
        // terminator is tried and the one whose remainder parses is taken.
        'X' => {
            let mut at = 0;
            while let Some(next) = rest[at..].find('_') {
                let end = at + next;
                let (name, after) = (&rest[..end], &rest[end + 1..]);
                if parses(after) {
                    return (format!("abstract<{name}>"), after);
                }
                at = end + 1;
            }
            ("abstract<?>".to_string(), "")
        }
        'O' => {
            let (fields, rest) = until_terminator(rest);
            (format!("obj({})", fields.join(", ")), rest)
        }
        'P' => {
            let (args, rest) = until_terminator(rest);
            let (ret, rest) = one_type(rest);
            (format!("({}) -> {ret}", args.join(", ")), rest)
        }
        other => (format!("?{other}"), rest),
    }
}

/// Whether the rest of a signature reads as types to its end. The test that
/// settles an abstract's name above.
fn parses(sig: &str) -> bool {
    let mut rest = sig;
    let mut steps = 0;
    while !rest.is_empty() {
        // The group this abstract sits in ends here, which is a good parse
        // and not a stuck one.
        if rest.starts_with('_') {
            return true;
        }
        steps += 1;
        if steps > 64 {
            return false;
        }
        let (one, next) = one_type(rest);
        if one.starts_with('?') || next.len() == rest.len() {
            return false;
        }
        rest = next;
    }
    true
}

/// Types until the `_` that closes the group.
fn until_terminator(mut sig: &str) -> (Vec<String>, &str) {
    let mut out = Vec::new();
    loop {
        match sig.strip_prefix('_') {
            Some(rest) => return (out, rest),
            None if sig.is_empty() => return (out, sig),
            None => {
                let (one, rest) = one_type(sig);
                out.push(one);
                sig = rest;
            }
        }
    }
}

/// A primitive's whole signature, which is always a function.
fn describe(sig: &str) -> String {
    one_type(sig).0
}

#[test]
fn report_the_signatures() {
    let (Ok(lib), names) = (
        std::env::var("ASH_SIGNATURES_LIB"),
        std::env::var("ASH_SIGNATURES_NAMES"),
    ) else {
        eprintln!("set ASH_SIGNATURES_LIB to a .hdll to run this");
        return;
    };
    let Ok(names) = names else {
        eprintln!("set ASH_SIGNATURES_NAMES to a file of primitive names");
        return;
    };
    let wanted = std::fs::read_to_string(&names).expect("reading the name list");

    let library = unsafe { libloading::Library::new(&lib) }.expect("opening the library");
    type Resolver = unsafe extern "C" fn(*mut *const c_char) -> *mut c_void;

    for name in wanted.lines().map(str::trim).filter(|l| !l.is_empty()) {
        let symbol = format!("hlp_{name}");
        let resolver: Result<libloading::Symbol<Resolver>, _> =
            unsafe { library.get(symbol.as_bytes()) };
        let Ok(resolver) = resolver else {
            println!("{name}: NOT EXPORTED");
            continue;
        };
        let mut sig: *const c_char = std::ptr::null();
        let addr = unsafe { resolver(&mut sig) };
        if addr.is_null() || sig.is_null() {
            println!("{name}: resolver answered nothing");
            continue;
        }
        let sig = unsafe { CStr::from_ptr(sig) }.to_string_lossy().into_owned();
        println!("{name}\t{sig}\t{}", describe(&sig));
    }
}
