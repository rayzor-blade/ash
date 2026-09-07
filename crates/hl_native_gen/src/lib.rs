//! Emit every side of a HashLink native library from one declaration.
//!
//! One primitive otherwise gets written out five times -- the Rust that
//! implements it, the Rust that forwards it on wasm, the JavaScript that
//! answers it in a page, the Haxe extern, and the table another host reads.
//! Keeping those in step by hand is how a wrong argument type gets into one
//! of them and nowhere else.
//!
//! Call [`generate`] from a `build.rs`:
//!
//! ```no_run
//! hl_native_gen::generate("wgpu.api").unwrap();
//! ```
//!
//! Rust goes to `OUT_DIR` to be `include!`d, as `crates/ash/build.rs` already
//! does for `std_symbols.rs`. The JavaScript, Haxe and contract are written
//! into the crate and committed, since whoever needs them is not building it.
//!
//! # The declaration
//!
//! ```text
//! library wgpu
//! prefix hlwgpu_
//! kinds instance adapter device
//!
//! # Comments attach to the line below them.
//! prim adapter_request(inst: i32, power: i32) -> i32
//!   js H.pending(H.get("instance", inst).requestAdapter())
//! ```
//!
//! The `js` line is that primitive's body in a page: an expression with the
//! arguments in scope, plus `H` from the prelude. Rust bodies are not
//! declared -- they are `imp::<name>`, and a mismatch is a link error.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

/// One type as it appears in each language.
struct Ty {
    /// The letter HashLink's signature grammar uses.
    letter: char,
    /// What a primitive takes or returns in Rust.
    rust: &'static str,
    haxe: &'static str,
    /// What crosses a wasm boundary, where a pointer is an address.
    wasm: &'static str,
}

fn ty(name: &str) -> Option<Ty> {
    Some(match name {
        "i32" => Ty { letter: 'i', rust: "i32", haxe: "Int", wasm: "i32" },
        "bool" => Ty { letter: 'b', rust: "bool", haxe: "Bool", wasm: "i32" },
        "f64" => Ty { letter: 'd', rust: "f64", haxe: "Float", wasm: "f64" },
        "bytes" => Ty { letter: 'B', rust: "*mut vbyte", haxe: "hl.Bytes", wasm: "i32" },
        "void" => Ty { letter: 'v', rust: "()", haxe: "Void", wasm: "" },
        _ => return None,
    })
}

struct Prim {
    name: String,
    args: Vec<(String, String)>,
    ret: String,
    js: String,
    doc: Vec<String>,
}

impl Prim {
    /// `P`, the arguments, `_`, the return: the primitive as one function
    /// type. `native_signatures.rs` parses `P` as `_FUN`, which is why every
    /// signature starts with one and not because of a leading argument.
    fn signature(&self) -> String {
        let mut s = String::from("P");
        for (_, t) in &self.args {
            s.push(ty(t).unwrap().letter);
        }
        s.push('_');
        s.push(ty(&self.ret).unwrap().letter);
        s
    }
}

struct Decl {
    library: String,
    prefix: String,
    kinds: Vec<String>,
    prims: Vec<Prim>,
}

fn parse(text: &str) -> Result<Decl, String> {
    let (mut library, mut prefix) = (String::new(), String::new());
    let (mut kinds, mut prims) = (Vec::new(), Vec::<Prim>::new());
    let mut doc: Vec<String> = Vec::new();

    for raw in text.lines() {
        let line = raw.trim();
        if let Some(rest) = line.strip_prefix('#') {
            doc.push(rest.trim().to_string());
            continue;
        }
        if line.is_empty() {
            // A blank line ends a comment block, so a section note is not
            // read as documentation of whatever follows.
            doc.clear();
            continue;
        }
        if let Some(rest) = line.strip_prefix("library ") {
            library = rest.trim().to_string();
            doc.clear();
        } else if let Some(rest) = line.strip_prefix("prefix ") {
            prefix = rest.trim().to_string();
            doc.clear();
        } else if let Some(rest) = line.strip_prefix("kinds ") {
            kinds = rest.split_whitespace().map(str::to_string).collect();
            if kinds.len() > 15 {
                return Err("a handle carries four bits of kind, so at most 15".into());
            }
            doc.clear();
        } else if let Some(rest) = line.strip_prefix("js ") {
            match prims.last_mut() {
                Some(p) => p.js = rest.trim().to_string(),
                None => return Err("a js line before any prim".into()),
            }
        } else if let Some(rest) = line.strip_prefix("prim ") {
            prims.push(parse_prim(rest, std::mem::take(&mut doc))?);
        } else {
            return Err(format!("cannot read: {line}"));
        }
    }

    if library.is_empty() || prefix.is_empty() {
        return Err("the declaration needs a `library` and a `prefix` line".into());
    }
    if let Some(p) = prims.iter().find(|p| p.js.is_empty()) {
        return Err(format!("no js body for {}", p.name));
    }
    Ok(Decl { library, prefix, kinds, prims })
}

fn parse_prim(rest: &str, doc: Vec<String>) -> Result<Prim, String> {
    let open = rest.find('(').ok_or_else(|| format!("no arguments in: {rest}"))?;
    let close = rest.rfind(')').ok_or_else(|| format!("no arguments in: {rest}"))?;
    let name = rest[..open].trim().to_string();
    let ret = rest[close + 1..]
        .trim()
        .strip_prefix("->")
        .ok_or_else(|| format!("{name}: no return type"))?
        .trim()
        .to_string();
    if ty(&ret).is_none() {
        return Err(format!("{name}: no such return type {ret}"));
    }

    let mut args = Vec::new();
    let inside = rest[open + 1..close].trim();
    if !inside.is_empty() {
        for part in inside.split(',') {
            let (n, t) = part
                .split_once(':')
                .ok_or_else(|| format!("{name}: cannot read argument {part}"))?;
            let t = t.trim().to_string();
            match ty(&t) {
                None | Some(Ty { letter: 'v', .. }) => {
                    return Err(format!("{name}: no such argument type {t}"))
                }
                Some(_) => {}
            }
            args.push((n.trim().to_string(), t));
        }
    }
    Ok(Prim { name, args, ret, js: String::new(), doc })
}

fn banner(src: &str) -> String {
    format!("// GENERATED from `{src}`. Edit the declaration, not this file.\n")
}

/// The doc comment, signature and header a primitive gets in both Rust files.
fn rust_head(out: &mut String, d: &Decl, p: &Prim) {
    for line in &p.doc {
        let _ = writeln!(out, "/// {line}");
    }
    if !p.doc.is_empty() {
        out.push_str("///\n");
    }
    let _ = writeln!(out, "/// `{}`", p.signature());
    out.push_str("///\n/// # Safety\n");
    out.push_str("/// Called by the VM through the resolver below, with the arguments\n");
    out.push_str("/// the signature declares.\n");
    let params = p
        .args
        .iter()
        .map(|(n, t)| format!("{n}: {}", ty(t).unwrap().rust))
        .collect::<Vec<_>>()
        .join(", ");
    let ret = if p.ret == "void" { String::new() } else { format!(" -> {}", ty(&p.ret).unwrap().rust) };
    out.push_str("#[no_mangle]\n");
    let _ = writeln!(out, "pub unsafe extern \"C\" fn {}_{}({params}){ret} {{", d.library, p.name);
}

fn rust_tail(out: &mut String, d: &Decl, p: &Prim) {
    out.push_str("}\n");
    let _ = writeln!(
        out,
        "define_prim!(hlp_{}, {}_{}, \"{}\");\n",
        p.name,
        d.library,
        p.name,
        p.signature()
    );
}

fn emit_kinds(d: &Decl, src: &str) -> String {
    let mut out = banner(src);
    out.push_str(
        "//\n// What a handle names, carried in its top four bits. Numbered from 1\n\
         // so that zero is never a valid handle. `js/prelude.js` is given the\n\
         // same numbering from the same line.\n\n",
    );
    out.push_str("#[derive(Clone, Copy, PartialEq, Eq, Debug)]\n");
    out.push_str("#[allow(dead_code)] // the whole numbering, not only what is used yet\n");
    out.push_str("#[repr(i32)]\npub enum Kind {\n");
    for (i, k) in d.kinds.iter().enumerate() {
        let mut c = k.chars();
        let upper: String = c.next().map(|f| f.to_uppercase().to_string()).unwrap_or_default();
        let _ = writeln!(out, "    {upper}{} = {},", c.as_str(), i + 1);
    }
    out.push_str("}\n");
    out
}

fn emit_native(d: &Decl, src: &str) -> String {
    let mut out = banner(src);
    out.push_str(
        "//\n// Every primitive calls straight into `crate::imp`. Nothing here\n\
         // decides anything.\n\n",
    );
    out.push_str("#[allow(unused_imports)]\nuse hl_abi::{define_prim, vbyte};\n\n");
    for p in &d.prims {
        rust_head(&mut out, d, p);
        let call = p.args.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ");
        let _ = writeln!(out, "    crate::imp::{}({call})", p.name);
        rust_tail(&mut out, d, p);
    }
    out
}

fn emit_wasm(d: &Decl, src: &str) -> String {
    let mut out = banner(src);
    let _ = write!(
        out,
        "//\n// Every primitive forwards to the host, which answers `env.{}*`.\n\
         // A wasip1 module has no GPU and no JavaScript, so there is no\n\
         // implementation to hold here; `IMPORTS.md` is what a host supplies.\n\n",
        d.prefix
    );
    out.push_str("#[allow(unused_imports)]\nuse hl_abi::{define_prim, vbyte};\n\n");
    for p in &d.prims {
        let imports = p
            .args
            .iter()
            .map(|(n, t)| format!("{n}: {}", ty(t).unwrap().wasm))
            .collect::<Vec<_>>()
            .join(", ");
        let iret = if p.ret == "void" {
            String::new()
        } else {
            format!(" -> {}", ty(&p.ret).unwrap().wasm)
        };
        out.push_str("#[link(wasm_import_module = \"env\")]\nextern \"C\" {\n");
        let _ = writeln!(out, "    fn {}{}({imports}){iret};", d.prefix, p.name);
        out.push_str("}\n\n");

        rust_head(&mut out, d, p);
        // A pointer is an address across this boundary and a bool is a word;
        // everything else is already the width the import declares.
        let call = p
            .args
            .iter()
            .map(|(n, t)| match t.as_str() {
                "bytes" | "bool" => format!("{n} as i32"),
                _ => n.clone(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        let mut expr = format!("{}{}({call})", d.prefix, p.name);
        match p.ret.as_str() {
            "bytes" => expr.push_str(" as *mut vbyte"),
            "bool" => expr.push_str(" != 0"),
            _ => {}
        }
        let _ = writeln!(out, "    {expr}");
        rust_tail(&mut out, d, p);
    }
    out
}

fn emit_js(d: &Decl, src: &str, prelude: &str) -> String {
    let numbering = d
        .kinds
        .iter()
        .enumerate()
        .map(|(i, k)| format!("{k}: {}", i + 1))
        .collect::<Vec<_>>()
        .join(", ");
    let mut out = format!(
        "// GENERATED from `{src}`, with the prelude below taken verbatim from\n\
         // `js/prelude.js`. Edit one of those, not this file.\n\n\
         // The handle kind numbering, from the same line of the declaration that\n\
         // `kinds.rs` comes from.\nconst KINDS = {{ {numbering} }};\n\n"
    );
    out.push_str(prelude.trim_end());
    out.push_str(
        "\n\n\n// The import object a page merges into `env`: one entry per primitive,\n\
         // each body taken from the declaration.\nexport function hlwgpuImports(rt) {\n\
         \x20 const H = makeHandles(rt);\n  return {\n",
    );
    for p in &d.prims {
        let args = p.args.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ");
        for line in &p.doc {
            let _ = writeln!(out, "    // {line}");
        }
        let body = match p.ret.as_str() {
            "void" => format!("{{ {}; }}", p.js),
            "bool" => format!("({}) ? 1 : 0", p.js),
            _ => p.js.clone(),
        };
        let _ = writeln!(out, "    {}{}: ({args}) => {body},", d.prefix, p.name);
    }
    out.push_str("  };\n}\n");
    out
}

fn emit_haxe(d: &Decl, src: &str) -> String {
    let mut out = format!(
        "// GENERATED from `{src}`. Edit the declaration, not this file.\n\n\
         package {};\n\n\
         /**\n\tThe primitives, one to one. A program is not meant to call these:\n\
         \tthe classes beside them are the API. `{}.hdll` answers them natively,\n\
         \tand a host answers them on wasm.\n**/\n@:keep\nclass _Native {{\n",
        d.library, d.library
    );
    for p in &d.prims {
        let args = p
            .args
            .iter()
            .map(|(n, t)| format!("{n} : {}", ty(t).unwrap().haxe))
            .collect::<Vec<_>>()
            .join(", ");
        for line in &p.doc {
            let _ = writeln!(out, "\t// {line}");
        }
        let _ = writeln!(out, "\t@:hlNative(\"{}\", \"{}\")", d.library, p.name);
        let _ = writeln!(
            out,
            "\tpublic static function {}({args}) : {} {{",
            p.name,
            ty(&p.ret).unwrap().haxe
        );
        // A body the VM replaces. Reaching one means the library never
        // loaded, so it must not look like a real answer.
        out.push_str(match p.ret.as_str() {
            "void" => "\t\treturn;\n",
            "bool" => "\t\treturn false;\n",
            "bytes" => "\t\treturn null;\n",
            _ => "\t\treturn 0;\n",
        });
        out.push_str("\t}\n\n");
    }
    out.push_str("}\n");
    out
}

fn emit_contract(d: &Decl, src: &str) -> String {
    let mut out = format!(
        "# What a host must supply for `{}`\n\n\
         GENERATED from `{src}`.\n\n\
         Natively none of this applies: `{}.hdll` holds the implementation and\n\
         answers the VM directly.\n\n\
         On wasm, `{}.wasm` holds no implementation. It imports the following\n\
         from `env`, and whatever instantiates the module has to provide them.\n\
         A page gets them from `hlwgpu.js`; any other host implements this table.\n\n\
         Every handle is an `i32`, and every string is a pointer to\n\
         NUL-terminated UTF-16 allocated through the program's `hlp_alloc_bytes`.\n\n\
         | import | parameters | returns |\n|---|---|---|\n",
        d.library, d.library, d.library
    );
    for p in &d.prims {
        let args = if p.args.is_empty() {
            "--".to_string()
        } else {
            p.args
                .iter()
                .map(|(n, t)| format!("`{n}: {}`", ty(t).unwrap().wasm))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let ret = match ty(&p.ret).unwrap().wasm {
            "" => "void",
            other => other,
        };
        let _ = writeln!(out, "| `{}{}` | {args} | `{ret}` |", d.prefix, p.name);
    }
    out
}

/// Writes only on a change, so a build does not keep touching files.
fn write_if_changed(path: &Path, text: &str) -> io::Result<()> {
    if fs::read_to_string(path).ok().as_deref() == Some(text) {
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, text)
}

/// Reads `api`, relative to the crate root, and writes every side of it.
///
/// `kinds.rs`, `native.rs` and `wasm.rs` go to `OUT_DIR`; the JavaScript,
/// Haxe externs and `IMPORTS.md` into the crate, since a page and a Haxe
/// program need those without building anything.
pub fn generate(api: impl AsRef<Path>) -> io::Result<()> {
    let root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").map_err(io::Error::other)?);
    let api = root.join(api);
    let prelude_path = root.join("js/prelude.js");

    println!("cargo:rerun-if-changed={}", api.display());
    println!("cargo:rerun-if-changed={}", prelude_path.display());

    let text = fs::read_to_string(&api)?;
    let src = api.file_name().unwrap_or_default().to_string_lossy().to_string();
    let decl = parse(&text).map_err(io::Error::other)?;

    let out = PathBuf::from(env::var("OUT_DIR").map_err(io::Error::other)?);
    fs::write(out.join("kinds.rs"), emit_kinds(&decl, &src))?;
    fs::write(out.join("native.rs"), emit_native(&decl, &src))?;
    fs::write(out.join("wasm.rs"), emit_wasm(&decl, &src))?;

    // Best effort: a read-only checkout still builds, and the committed
    // copies are what a consumer reads.
    let prelude = fs::read_to_string(&prelude_path).unwrap_or_default();
    let _ = write_if_changed(&root.join("js/hlwgpu.js"), &emit_js(&decl, &src, &prelude));
    let _ = write_if_changed(
        &root.join(format!("haxe/{}/_Native.hx", decl.library)),
        &emit_haxe(&decl, &src),
    );
    let _ = write_if_changed(&root.join("IMPORTS.md"), &emit_contract(&decl, &src));
    Ok(())
}
