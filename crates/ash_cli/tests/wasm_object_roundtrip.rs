//! The object writer against the real linker: two objects built by
//! `ash_wasm_emit::object`, one calling a function the other defines, linked
//! by `ash_wasm_link` and run in wasmtime.
use ash_wasm_emit::object::{Body, ObjectBuilder};
use ash_wasm_emit::wasm_encoder::{Instruction as I, ValType};

fn double_object() -> Vec<u8> {
    let mut o = ObjectBuilder::new();
    let f = o.declare_function("rt_double", vec![ValType::I32], vec![ValType::I32], false);
    let mut b = Body::new(vec![]);
    b.instruction(&I::LocalGet(0));
    b.instruction(&I::I32Const(2));
    b.instruction(&I::I32Mul);
    b.instruction(&I::End);
    o.set_body(f, b);
    // The linker builds its constructor stub from a `() -> ()` type, which
    // every real object has.
    let noop = o.declare_function("noop", vec![], vec![], true);
    let mut b = Body::new(vec![]);
    b.instruction(&I::End);
    o.set_body(noop, b);
    o.finish()
}

fn caller_object() -> Vec<u8> {
    let mut o = ObjectBuilder::new();
    // Declared before it is called, and another object defines the callee.
    let callee = o.import_function("rt_double", vec![ValType::I32], vec![ValType::I32]);
    let f = o.declare_function("double_plus_one", vec![ValType::I32], vec![ValType::I32], false);
    let mut b = Body::new(vec![]);
    b.instruction(&I::LocalGet(0));
    b.call(callee);
    b.instruction(&I::I32Const(1));
    b.instruction(&I::I32Add);
    b.instruction(&I::End);
    o.set_body(f, b);
    o.finish()
}

#[test]
fn objects_made_by_the_writer_link_and_run() {
    let a = ash_wasm_link::read("caller", &caller_object()).expect("the caller object reads");
    let b = ash_wasm_link::read("double", &double_object()).expect("the callee object reads");
    // Every call site must hold the index its symbol resolves to.
    assert_eq!(ash_wasm_link::check_function_sites(&a).unwrap(), vec![]);
    let opts = ash_wasm_link::LinkOptions {
        export_all_functions: true,
        ..Default::default()
    };
    let module = ash_wasm_link::link(vec![a, b], &opts).expect("the objects link");

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &module).expect("the linked module validates");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let f = instance
        .get_typed_func::<i32, i32>(&mut store, "double_plus_one")
        .unwrap();
    assert_eq!(f.call(&mut store, 20).unwrap(), 41);
}
