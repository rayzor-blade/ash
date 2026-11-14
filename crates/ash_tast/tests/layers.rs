//! A program built on a library layer: ids that cross layers, and the checks on them.

use ash_tast::builder::{Builder, Tree};
use ash_tast::dump::Dumper;
use ash_tast::format::*;
use ash_tast::{Stack, Tast, TastError};
use zerocopy::little_endian::U32;

const LIBRARY: u64 = 0x1111_2222_3333_4444;
const PROGRAM: u64 = 0x5555_6666_7777_8888;

/// A library with one class, `pkg.Base`, and one method, `greet`, that returns 7.
fn library() -> Vec<u8> {
    let mut b = Builder::new();
    b.layer_id = LIBRARY;
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    let name = b.string("pkg.Base");
    b.classes.push(ClassRec {
        name: U32::new(name),
        super_type: U32::new(NONE),
        flags: U32::new(0),
        fields_first: U32::new(0),
        fields_count: U32::new(1),
        interfaces_first: U32::new(0),
        interfaces_count: U32::new(0),
    });
    let class_type = b.add_type(type_kind::CLASS, None, 0, &[]);
    let body = Tree::new(node_kind::RETURN)
        .flags(node_flag::HAS_VALUE)
        .kids(vec![
            Tree::new(node_kind::CONST)
                .flags(const_kind::INT)
                .a(7)
                .ty(dynamic),
        ]);
    let greet = b.add_func(dynamic, &[], Some(body));
    let greet_name = b.string("greet");
    b.fields.push(FieldRec {
        name: U32::new(greet_name),
        ty: U32::new(dynamic),
        owner: U32::new(0),
        flags: U32::new(field_flag::METHOD),
        func: U32::new(greet),
        pos: U32::new(NONE),
    });
    b.add_key(key_kind::CLASS, "pkg.Base", 0);
    b.add_key(key_kind::FIELD, "pkg.Base#i:greet", 0);
    b.add_key(key_kind::INST_TYPE, "pkg.Base", class_type);
    b.finish()
}

/// A program whose entry calls `new Base().greet()`, naming the library's entries by tagged id.
fn program(field: u32, class: u32, ty: u32) -> Vec<u8> {
    let mut b = Builder::new();
    b.layer_id = PROGRAM;
    b.parents = vec![LIBRARY];
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    let body = Tree::new(node_kind::CALL).ty(dynamic).kids(vec![
        Tree::new(node_kind::FIELD)
            .a(field_access::INSTANCE)
            .b(field)
            .kids(vec![Tree::new(node_kind::NEW).a(class).ty(ty)]),
    ]);
    let main = b.add_func(dynamic, &[], Some(body));
    b.entry = Some(main);
    b.add_kept(kept_kind::CLASS, tag::tagged(0, 0));
    b.add_kept(kept_kind::FIELD, tag::tagged(0, 0));
    b.finish()
}

fn good_program() -> Vec<u8> {
    program(tag::tagged(0, 0), tag::tagged(0, 0), tag::tagged(0, 1))
}

#[test]
fn a_program_names_the_library_by_tagged_ids() {
    let (lib, prog) = (library(), good_program());
    let stack = Stack::parse(&[&lib, &prog]).expect("the layers stack");
    assert_eq!(stack.layers().len(), 2);
    assert_eq!(stack.layers()[0].layer_id(), LIBRARY);
    assert_eq!(stack.top().layer_id(), PROGRAM);

    // The program's field node resolves to the library's `greet` field, in layer 0.
    let top = stack.top();
    let entry = &top.funcs()[top.entry().unwrap() as usize];
    let call = top.body(entry).unwrap();
    let field = &top.kids(call)[0];
    let (layer, rec) = stack.field_rec(1, field.b);
    assert_eq!(layer, 0);
    assert_eq!(stack.layers()[0].opt_string(rec.name), Some("greet"));

    // What was kept from below is listed, with tags.
    assert_eq!(top.kept().len(), 2);
    assert_eq!(top.kept()[1].id.get(), tag::tagged(0, 0));
}

#[test]
fn the_dump_prints_names_not_ids_across_layers() {
    let (lib, prog) = (library(), good_program());
    let stack = Stack::parse(&[&lib, &prog]).unwrap();
    let dumper = Dumper::new(&stack);
    let entry = stack.top().entry().unwrap() as usize;
    let text = dumper.function_text(1, entry);
    assert!(text.contains("FIELD INSTANCE pkg.Base#greet"), "{text}");
    assert!(text.contains("NEW pkg.Base : pkg.Base"), "{text}");
    // The library's own function prints the same way from its own layer.
    assert!(dumper.function_text(0, 0).contains("CONST 7"));
}

#[test]
fn a_tag_that_names_nothing_is_refused() {
    let lib = library();
    // The library has one field and one class; a second does not exist.
    let prog = program(tag::tagged(0, 1), tag::tagged(0, 0), tag::tagged(0, 1));
    assert!(matches!(
        Stack::parse(&[&lib, &prog]).unwrap_err(),
        TastError::BadReference {
            table: "Nodes",
            field: "b",
            ..
        }
    ));
    // A layer that was never stacked.
    let prog = program(tag::tagged(1, 0), tag::tagged(0, 0), tag::tagged(0, 1));
    assert!(matches!(
        Stack::parse(&[&lib, &prog]).unwrap_err(),
        TastError::BadReference {
            table: "Nodes",
            field: "b",
            ..
        }
    ));
}

#[test]
fn a_file_with_tags_does_not_stand_alone() {
    let prog = good_program();
    assert!(matches!(
        Tast::parse(&prog).unwrap_err(),
        TastError::BadReference { .. }
    ));
}

#[test]
fn layers_must_stack_on_the_layers_they_name() {
    let (lib, prog) = (library(), good_program());
    // Missing the library.
    assert!(Stack::parse(&[&prog]).is_err());
    // Stacked on something that is not the library it names.
    let mut other = Builder::new();
    other.layer_id = 0x9999;
    let other = other.finish();
    assert_eq!(
        Stack::parse(&[&other, &prog]).unwrap_err(),
        TastError::ParentMismatch { layer: 1 }
    );
    // The library alone is fine, and its parent list is empty.
    let stack = Stack::parse(&[&lib]).unwrap();
    assert_eq!(stack.top().parent_ids().count(), 0);
}

#[test]
fn keys_and_kept_entries_are_checked_against_the_tables_they_name() {
    let mut b = Builder::new();
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    b.add_key(key_kind::INST_TYPE, "Thing", dynamic);
    assert!(Tast::parse(&b.finish()).is_ok());

    let mut b = Builder::new();
    b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    b.add_key(key_kind::CLASS, "pkg.Missing", 3);
    assert!(matches!(
        Tast::parse(&b.finish()).unwrap_err(),
        TastError::BadReference {
            table: "Keys",
            field: "value",
            ..
        }
    ));

    let mut b = Builder::new();
    b.add_kept(kept_kind::CLASS, 0);
    assert!(matches!(
        Tast::parse(&b.finish()).unwrap_err(),
        TastError::BadReference { table: "Kept", .. }
    ));
}
