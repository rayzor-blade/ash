use ash_tast::builder::{Builder, Tree};
use ash_tast::format::*;
use ash_tast::{Tast, TastError};

/// `{ var x = 5; return x; }` as the body of the entry function.
fn small_program() -> Vec<u8> {
    let mut b = Builder::new();
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    let x = b.add_local("x", dynamic, 0);
    let body = Tree::new(node_kind::BLOCK).kids(vec![
        Tree::new(node_kind::VAR)
            .flags(node_flag::HAS_INIT)
            .a(x)
            .kids(vec![
                Tree::new(node_kind::CONST)
                    .flags(const_kind::INT)
                    .a(5)
                    .ty(dynamic),
            ]),
        Tree::new(node_kind::RETURN)
            .flags(node_flag::HAS_VALUE)
            .kids(vec![Tree::new(node_kind::LOCAL).a(x).ty(dynamic)]),
    ]);
    let main = b.add_func(dynamic, &[], Some(body));
    b.entry = Some(main);
    b.finish()
}

/// The byte offset of a section's first record.
fn section_offset(bytes: &[u8], section: usize) -> usize {
    let at = Header::SIZE + section * SectionRec::SIZE;
    u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap()) as usize
}

/// The byte offset of `field` of node `node`.
fn node_field(bytes: &[u8], node: usize, field: &str) -> usize {
    let offset = NodeRec::DESC
        .fields
        .iter()
        .find(|f| f.name == field)
        .unwrap()
        .offset;
    section_offset(bytes, section::NODES) + node * NodeRec::SIZE + offset
}

fn set_u32(bytes: &mut [u8], at: usize, value: u32) {
    bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
}

#[test]
fn a_small_program_reads_back_in_place() {
    let bytes = small_program();
    let tast = Tast::parse(&bytes).unwrap();
    assert_eq!(tast.haxe_version(), 40306);
    assert_eq!(tast.funcs().len(), 1);
    assert_eq!(tast.entry(), Some(0));

    let func = &tast.funcs()[tast.entry().unwrap() as usize];
    let root = tast.body(func).unwrap();
    assert_eq!(root.kind, node_kind::BLOCK);
    let statements = tast.kids(root);
    assert_eq!(statements.len(), 2);
    assert_eq!(statements[0].kind, node_kind::VAR);
    assert_eq!(statements[1].kind, node_kind::RETURN);

    let init = tast.kids(&statements[0]);
    assert_eq!(
        (init[0].kind, init[0].flags, init[0].a.get()),
        (node_kind::CONST, const_kind::INT, 5)
    );
    let local = &tast.locals()[statements[0].a.get() as usize];
    assert_eq!(tast.opt_string(local.name), Some("x"));
}

#[test]
fn children_are_consecutive_and_follow_their_parent() {
    let mut b = Builder::new();
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    let leaf = |n: u32| Tree::new(node_kind::CONST).flags(const_kind::INT).a(n);
    let body = Tree::new(node_kind::BLOCK).kids(vec![
        Tree::new(node_kind::ARRAY_DECL).kids(vec![leaf(1), leaf(2), leaf(3)]),
        Tree::new(node_kind::PARENTHESIS)
            .kids(vec![Tree::new(node_kind::UNOP).a(2).kids(vec![leaf(4)])]),
    ]);
    let other = Tree::new(node_kind::ARRAY_DECL).kids(vec![leaf(9)]);
    b.add_func(dynamic, &[], Some(body));
    b.add_func(dynamic, &[], Some(other));
    let bytes = b.finish();
    let tast = Tast::parse(&bytes).unwrap();

    // Both roots come first, then each level in order.
    assert_eq!(tast.funcs()[0].root.get(), 0);
    assert_eq!(tast.funcs()[1].root.get(), 1);
    for (i, node) in tast.nodes().iter().enumerate() {
        if node.kid_count.get() > 0 {
            assert!(node.first_kid.get() as usize > i);
        }
    }
    let root = tast.node(0);
    let kinds: Vec<u8> = tast.kids(root).iter().map(|n| n.kind).collect();
    assert_eq!(kinds, [node_kind::ARRAY_DECL, node_kind::PARENTHESIS]);
    let array = &tast.kids(root)[0];
    let values: Vec<u32> = tast.kids(array).iter().map(|n| n.a.get()).collect();
    assert_eq!(values, [1, 2, 3]);
}

#[test]
fn a_damaged_header_is_refused() {
    let good = small_program();

    let mut bytes = good.clone();
    set_u32(&mut bytes, 0, 0x1234_5678);
    assert_eq!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadMagic(0x1234_5678)
    );

    let mut bytes = good.clone();
    set_u32(&mut bytes, 4, 99);
    assert_eq!(Tast::parse(&bytes).unwrap_err(), TastError::BadVersion(99));

    let mut bytes = good.clone();
    bytes[8] ^= 1;
    assert!(matches!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::SchemaMismatch { .. }
    ));

    assert_eq!(Tast::parse(&good[..20]).unwrap_err(), TastError::TooShort);
    let cut = section_offset(&good, section::NODES) + 5;
    assert_eq!(
        Tast::parse(&good[..cut]).unwrap_err(),
        TastError::SectionOutOfBounds { section: "Nodes" }
    );
}

#[test]
fn a_string_that_is_not_utf8_is_refused() {
    let mut bytes = small_program();
    let blob = section_offset(&bytes, section::STRINGS);
    bytes[blob] = 0xff;
    assert!(matches!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadString { .. }
    ));
}

#[test]
fn bad_node_references_are_refused() {
    let good = small_program();

    // The BLOCK's children point back at the root.
    let mut bytes = good.clone();
    let at = node_field(&bytes, 0, "first_kid");
    set_u32(&mut bytes, at, 0);
    assert_eq!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::KidsNotAfterParent { node: 0 }
    );

    // A child range that runs past the node array.
    let mut bytes = good.clone();
    let at = node_field(&bytes, 0, "kid_count");
    set_u32(&mut bytes, at, 1000);
    assert!(matches!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadReference { table: "Nodes", .. }
    ));

    // A node of a kind the format does not define.
    let mut bytes = good.clone();
    let at = node_field(&bytes, 0, "kind");
    bytes[at] = 200;
    assert_eq!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::UnknownNodeKind { node: 0, kind: 200 }
    );

    // A local id outside the table.
    let mut bytes = good.clone();
    let local_node = tast_node_of_kind(&good, node_kind::LOCAL);
    let at = node_field(&bytes, local_node, "a");
    set_u32(&mut bytes, at, 7);
    assert!(matches!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadReference { field: "a", .. }
    ));
}

fn tast_node_of_kind(bytes: &[u8], kind: u8) -> usize {
    let tast = Tast::parse(bytes).unwrap();
    tast.nodes().iter().position(|n| n.kind == kind).unwrap()
}

#[test]
fn a_node_with_the_wrong_number_of_children_is_refused() {
    let mut b = Builder::new();
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    // An IF that says it has an else branch but has only two children.
    let body = Tree::new(node_kind::IF)
        .flags(node_flag::HAS_ELSE)
        .kids(vec![
            Tree::new(node_kind::CONST).flags(const_kind::BOOL).a(1),
            Tree::new(node_kind::BREAK),
        ]);
    b.add_func(dynamic, &[], Some(body));
    let bytes = b.finish();
    assert_eq!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadArity {
            node: 0,
            kind: "IF",
            kids: 2
        }
    );
}

#[test]
fn a_string_id_past_the_table_is_refused() {
    let mut b = Builder::new();
    let dynamic = b.add_type(type_kind::DYNAMIC, None, NONE, &[]);
    b.add_func(dynamic, &[], Some(Tree::new(node_kind::IDENT).a(3)));
    let bytes = b.finish();
    assert!(matches!(
        Tast::parse(&bytes).unwrap_err(),
        TastError::BadReference {
            table: "Nodes",
            field: "a",
            value: 3,
            ..
        }
    ));
}

#[test]
fn the_schema_hash_changes_only_on_purpose() {
    // Changing a record, a section or a constant table changes this. Bump
    // `VERSION`, regenerate Format.hx with `gen_haxe`, and update the hash.
    assert_eq!(schema_hash(), 0xdf64_499b_956a_98aa);
}

#[test]
fn records_have_no_padding() {
    for record in RECORDS {
        let packed: usize = record
            .fields
            .iter()
            .map(|f| match f.ty {
                "u8" => 1,
                "u16" => 2,
                "u32" => 4,
                other => panic!("unknown wire type {other}"),
            })
            .sum();
        assert_eq!(packed, record.size, "{} has padding", record.name);
    }
    assert_eq!(NodeRec::SIZE, 28);
    assert_eq!(Header::SIZE, 40);
}

#[test]
fn the_assign_op_index_names_op_assign_op() {
    assert_eq!(BINOPS[BINOP_ASSIGN_OP as usize], "OpAssignOp");
}

#[test]
fn node_kinds_follow_typed_expr_def() {
    // `TConst` is `CONST`, `TObjectDecl` is `OBJECT_DECL`.
    for (i, constructor) in TYPED_EXPR_DEF.iter().enumerate() {
        let mut expected = String::new();
        for (j, ch) in constructor.strip_prefix('T').unwrap().chars().enumerate() {
            if ch.is_uppercase() && j > 0 {
                expected.push('_');
            }
            expected.extend(ch.to_uppercase());
        }
        assert_eq!(NODE_KINDS[i], (expected.as_str(), i as u8), "kind {i}");
    }
}

#[test]
fn the_committed_haxe_writer_is_current() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/haxe/ash/tast/Format.hx");
    let committed = std::fs::read_to_string(path).expect("haxe/ash/tast/Format.hx");
    assert!(
        committed == ash_tast::haxe::generate(),
        "Format.hx is stale; run `cargo run -p ash_tast --bin gen_haxe -- crates/ash_tast/haxe/ash/tast/Format.hx`"
    );
}

#[test]
fn the_committed_ocaml_writer_is_current() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/ocaml/ash_tast/ml/tast_format.ml"
    );
    let committed = std::fs::read_to_string(path).expect("ocaml/ash_tast/ml/tast_format.ml");
    assert!(
        committed == ash_tast::ocaml::generate(),
        "tast_format.ml is stale; run `cargo run -p ash_tast --bin gen_ocaml -- crates/ash_tast/ocaml/ash_tast/ml/tast_format.ml`"
    );
}
