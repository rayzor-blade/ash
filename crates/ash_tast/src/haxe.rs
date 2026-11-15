//! Generates the Haxe side of the format.
//!
//! The output is a Haxe module of constants and one writer function per record,
//! built from the same [`RecordDesc`]s the Rust structs are, so a field added
//! to a record appears in the writer with no second edit.

use std::fmt::Write;

use crate::format::*;

const KEYWORDS: &[&str] = &[
    "abstract",
    "break",
    "case",
    "cast",
    "catch",
    "class",
    "continue",
    "default",
    "do",
    "dynamic",
    "else",
    "enum",
    "extends",
    "extern",
    "false",
    "final",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "inline",
    "interface",
    "is",
    "macro",
    "new",
    "null",
    "override",
    "package",
    "private",
    "public",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "untyped",
    "using",
    "var",
    "while",
];

/// `first_kid` as `firstKid`, avoiding Haxe keywords and the writer's own parameter names.
fn camel(snake: &str) -> String {
    let mut out = String::new();
    let mut upper = false;
    for ch in snake.chars() {
        if ch == '_' {
            upper = true;
        } else if upper {
            out.extend(ch.to_uppercase());
            upper = false;
        } else {
            out.push(ch);
        }
    }
    if KEYWORDS.contains(&out.as_str()) || out == "out" || out == "at" {
        out.push('_');
    }
    out
}

/// `NodeRec` as `NODE_REC`.
fn screaming(camel_case: &str) -> String {
    let mut out = String::new();
    for (i, ch) in camel_case.chars().enumerate() {
        if ch.is_uppercase() && i > 0 {
            out.push('_');
        }
        out.extend(ch.to_uppercase());
    }
    out
}

fn group<T: std::fmt::Display + Copy>(
    out: &mut String,
    class: &str,
    doc: &str,
    entries: &[(&str, T)],
) {
    writeln!(out, "/** {doc} */\nclass {class} {{").unwrap();
    for (name, value) in entries {
        writeln!(out, "\tpublic static inline final {name} = {value};").unwrap();
    }
    writeln!(out, "}}\n").unwrap();
}

fn names(out: &mut String, class: &str, doc: &str, list: &[&str]) {
    writeln!(out, "/** {doc} */\nclass {class} {{").unwrap();
    let quoted: Vec<String> = list.iter().map(|n| format!("\"{n}\"")).collect();
    writeln!(
        out,
        "\tpublic static final NAMES = [{}];",
        quoted.join(", ")
    )
    .unwrap();
    writeln!(out, "}}\n").unwrap();
}

fn width(ty: &str) -> usize {
    match ty {
        "u8" => 1,
        "u16" => 2,
        "u32" => 4,
        other => panic!("no Haxe writer for wire type {other}"),
    }
}

/// The writes for one record. A run of sub-word fields that exactly fills a 4-byte
/// word is packed into one `setInt32`, since each call is a cost the interpreter
/// running the writer pays per node; every other field is its own write.
fn write_fields(out: &mut String, fields: &[FieldDesc]) {
    let mut i = 0;
    while i < fields.len() {
        let field = &fields[i];
        if field.offset.is_multiple_of(4) && field.ty != "u32" {
            let (mut j, mut bytes) = (i, 0);
            while j < fields.len()
                && fields[j].ty != "u32"
                && fields[j].offset == field.offset + bytes
            {
                bytes += width(fields[j].ty);
                j += 1;
                if bytes == 4 {
                    break;
                }
            }
            if bytes == 4 && j - i > 1 {
                let mut terms = Vec::new();
                let mut shift = 0;
                for part in &fields[i..j] {
                    let w = width(part.ty);
                    if !part.name.starts_with('_') {
                        let mask = (1u32 << (8 * w)) - 1;
                        let value = camel(part.name);
                        terms.push(if shift == 0 {
                            format!("({value} & {mask:#x})")
                        } else {
                            format!("(({value} & {mask:#x}) << {shift})")
                        });
                    }
                    shift += 8 * w;
                }
                let expr = if terms.is_empty() {
                    "0".to_owned()
                } else {
                    terms.join(" | ")
                };
                writeln!(out, "\t\tout.setInt32(at + {}, {expr});", field.offset).unwrap();
                i = j;
                continue;
            }
        }
        let value = if field.name.starts_with('_') {
            "0".to_owned()
        } else {
            camel(field.name)
        };
        let call = match field.ty {
            "u8" => format!("out.set(at + {}, {value});", field.offset),
            "u16" => format!("out.setUInt16(at + {}, {value});", field.offset),
            _ => format!("out.setInt32(at + {}, {value});", field.offset),
        };
        writeln!(out, "\t\t{call}").unwrap();
        i += 1;
    }
}

/// The text of `ash/tast/Format.hx`.
pub fn generate() -> String {
    let mut out = String::new();
    let hash = schema_hash();
    writeln!(
        out,
        "// Generated by `cargo run -p ash_tast --bin gen_haxe`. Do not edit.\n\
         package ash.tast;\n\n\
         import haxe.io.Bytes;\n\n\
         /**\n\
         \tThe TAST layout: constants, and one writer per record. Words are\n\
         \tlittle-endian, which is what `Bytes.setInt32` and `setUInt16` write.\n\
         **/\n\
         class Format {{"
    )
    .unwrap();
    writeln!(out, "\tpublic static inline final MAGIC = {:#010x};", MAGIC).unwrap();
    writeln!(out, "\tpublic static inline final VERSION = {VERSION};").unwrap();
    writeln!(
        out,
        "\t/** A reference to nothing (`u32::MAX`, which is -1 as an Int). */"
    )
    .unwrap();
    writeln!(out, "\tpublic static inline final NONE = -1;").unwrap();
    writeln!(
        out,
        "\tpublic static inline final SCHEMA_HASH_LO = {:#010x};",
        hash as u32
    )
    .unwrap();
    writeln!(
        out,
        "\tpublic static inline final SCHEMA_HASH_HI = {:#010x};",
        (hash >> 32) as u32
    )
    .unwrap();
    writeln!(
        out,
        "\tpublic static inline final SECTION_COUNT = {};",
        SECTIONS.len()
    )
    .unwrap();
    for (i, section) in SECTIONS.iter().enumerate() {
        let name = screaming(section.name);
        writeln!(out, "\tpublic static inline final SECTION_{name} = {i};").unwrap();
    }
    for section in SECTIONS {
        writeln!(
            out,
            "\tpublic static inline final {}_ELEM = {};",
            screaming(section.name),
            section.elem
        )
        .unwrap();
    }
    for record in RECORDS {
        writeln!(
            out,
            "\tpublic static inline final {}_SIZE = {};",
            screaming(record.name),
            record.size
        )
        .unwrap();
        for field in record.fields.iter().filter(|f| !f.name.starts_with('_')) {
            writeln!(
                out,
                "\tpublic static inline final {}_{}_AT = {};",
                screaming(record.name),
                field.name.to_uppercase(),
                field.offset
            )
            .unwrap();
        }
    }
    writeln!(out).unwrap();

    for record in RECORDS {
        let params: Vec<String> = record
            .fields
            .iter()
            .filter(|f| !f.name.starts_with('_'))
            .map(|f| format!("{}:Int", camel(f.name)))
            .collect();
        writeln!(out, "\t/** Write one `{}` at byte `at`. */", record.name).unwrap();
        writeln!(
            out,
            "\tpublic static inline function write{}(out:Bytes, at:Int, {}):Void {{",
            record.name,
            params.join(", ")
        )
        .unwrap();
        write_fields(&mut out, record.fields);
        writeln!(out, "\t}}\n").unwrap();
    }

    writeln!(
        out,
        "\t#if macro\n\
         \t/** Fails when the running compiler's enums are not the ones the format was built against. */\n\
         \tpublic static function checkCompiler():Void {{\n\
         \t\tcheckNames(\"TypedExprDef\", NodeKind.TYPED_EXPR_DEF, Type.getEnumConstructs(haxe.macro.Type.TypedExprDef));\n\
         \t\tcheckNames(\"Binop\", Binops.NAMES, Type.getEnumConstructs(haxe.macro.Expr.Binop));\n\
         \t\tcheckNames(\"Unop\", Unops.NAMES, Type.getEnumConstructs(haxe.macro.Expr.Unop));\n\
         \t}}\n\n\
         \tstatic function checkNames(what:String, expected:Array<String>, actual:Array<String>):Void {{\n\
         \t\tvar n = expected.length < actual.length ? expected.length : actual.length;\n\
         \t\tfor (i in 0...n)\n\
         \t\t\tif (expected[i] != actual[i])\n\
         \t\t\t\tthrow 'TAST was built for $what constructor $i = ${{expected[i]}}, this compiler has ${{actual[i]}}';\n\
         \t\tif (expected.length != actual.length)\n\
         \t\t\tthrow 'TAST was built for ${{expected.length}} $what constructors, this compiler has ${{actual.length}}';\n\
         \t}}\n\
         \t#end\n\
         }}\n"
    )
    .unwrap();

    // NodeKind carries the TypedExprDef names too, for checkCompiler.
    writeln!(out, "/** `NodeRec.kind`. `0..=28` are the constructors of `TypedExprDef` in order. */\nclass NodeKind {{").unwrap();
    for (name, value) in NODE_KINDS {
        writeln!(out, "\tpublic static inline final {name} = {value};").unwrap();
    }
    let quoted: Vec<String> = TYPED_EXPR_DEF.iter().map(|n| format!("\"{n}\"")).collect();
    writeln!(
        out,
        "\tpublic static final TYPED_EXPR_DEF = [{}];",
        quoted.join(", ")
    )
    .unwrap();
    writeln!(out, "}}\n").unwrap();

    group(
        &mut out,
        "NodeFlag",
        "`NodeRec.flags`, per kind.",
        NODE_FLAGS,
    );
    group(&mut out, "ConstKind", "`CONST` nodes' flags.", CONST_KINDS);
    group(
        &mut out,
        "FieldAccess",
        "`FIELD` nodes' `a`.",
        FIELD_ACCESSES,
    );
    group(
        &mut out,
        "ModuleKind",
        "Which table a module-type index names.",
        MODULE_KINDS,
    );
    group(
        &mut out,
        "AnonStatus",
        "`TypeRec.flags` of an `ANON` type: what the structure is the statics of.",
        ANON_STATUSES,
    );
    group(&mut out, "TypeKind", "`TypeRec.kind`.", TYPE_KINDS);
    group(&mut out, "ClassFlag", "`ClassRec.flags`.", CLASS_FLAGS);
    group(&mut out, "FieldFlag", "`FieldRec.flags`.", FIELD_FLAGS);
    group(&mut out, "LocalFlag", "`LocalRec.flags`.", LOCAL_FLAGS);
    group(&mut out, "KeyKind", "`KeyRec.kind`.", KEY_KINDS);
    group(&mut out, "KeptKind", "`KeptRec.kind`.", KEPT_KINDS);
    group(
        &mut out,
        "MetaOwner",
        "`MetaRec.owner_kind`: which table `owner` indexes.",
        META_OWNERS,
    );
    names(
        &mut out,
        "Binops",
        "`haxe.macro.Expr.Binop` constructors, by the index `BINOP` nodes carry.",
        &BINOPS,
    );
    names(
        &mut out,
        "Unops",
        "`haxe.macro.Expr.Unop` constructors, by the index `UNOP` nodes carry.",
        &UNOPS,
    );
    while out.ends_with("\n\n") {
        out.pop();
    }
    out
}
