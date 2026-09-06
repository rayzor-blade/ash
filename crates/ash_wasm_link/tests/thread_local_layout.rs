//! Where a thread-local ends up, checked on a module small enough to read.
//!
//! The dangerous property this crate warns about applies here twice over. A
//! `MEMORY_ADDR_TLS_SLEB` relocation writes an offset into a five-byte slot,
//! and every wrong offset that lands inside the block produces a module that
//! validates and runs -- reading the neighbouring variable instead of the one
//! asked for. Nothing about that is visible in a backtrace.
//!
//! So the object here is built by hand rather than read from disk: two data
//! segments, one of them `.tdata`, one relocation that has to come out
//! holding zero because the thread-local it names is the first thing in the
//! block, and a second variable after it that has to come out holding four.

mod common;

use ash_wasm_link::{link, LinkOptions};
use common::{object, read, sleb5, FIRST_SLOT, SECOND_SLOT};
use wasmparser::{RelocationType, SymbolFlags};

#[test]
fn a_thread_local_is_addressed_from_the_block_and_not_from_memory() {
    let module = link(vec![object(".tdata", 0)], &LinkOptions::default()).expect("link");

    let mut features = wasmparser::WasmFeatures::default();
    features.insert(wasmparser::WasmFeatures::THREADS);
    wasmparser::Validator::new_with_features(features)
        .validate_all(&module)
        .expect("the output validates");

    let read = read(&module);

    // __stack_pointer, __memory_base, __table_base, then the three a threads
    // build needs.
    assert_eq!(read.globals.len(), 6, "globals: {:?}", read.globals);
    let (base_mutable, tls_base) = read.globals[3];
    let (_, tls_size) = read.globals[4];
    let (_, tls_align) = read.globals[5];
    assert!(base_mutable, "__tls_base has to move as each thread arrives");
    assert_eq!(tls_size, 8, "two i32s");
    assert_eq!(tls_align, 8, "the .tdata segment asked for eight");

    // The relocations: an offset within the block, not an address. The first
    // variable is at the start of it and the second four bytes in, and both
    // would be six-figure addresses if they were measured from memory.
    let main = read.functions.first().expect("a body for main");
    assert_eq!(sleb5(main, FIRST_SLOT as usize), 0);
    assert_eq!(sleb5(main, SECOND_SLOT as usize), 4);

    // Two copies of the thread-local data are placed: the template a new
    // thread copies from, and the main thread's own block, which
    // `__tls_base` starts at. They have to hold the same bytes, or a thread
    // starting later gets different initial values from the one that started
    // first.
    let (Some(start), image) = read.data.first().expect("a data segment") else {
        panic!("the data is passive in a build that did not ask for it");
    };
    let at = |address: i32| {
        let offset = (address as u32 - start) as usize;
        image[offset..offset + 8].to_vec()
    };
    let expected = vec![1, 0, 0, 0, 2, 0, 0, 0];
    assert_eq!(at(tls_base), expected, "the main thread's block");
    let template = tls_base - tls_size;
    assert_eq!(at(template), expected, "the template");

    // And `__wasm_init_tls`, which is what gives a thread a block of its own:
    // it stores its argument into `__tls_base` and copies the template over
    // it.
    let init_tls = &read.functions[read.functions.len() - 1];
    assert!(
        init_tls.contains(&0xfc) && init_tls.windows(2).any(|w| w == [0xfc, 0x0a]),
        "__wasm_init_tls does not copy the template: {init_tls:02x?}"
    );
}

/// The same layout, whichever of the two things says the segment is
/// thread-local: the name LLVM gives it -- with the per-symbol suffix it
/// carries under `-fdata-sections` -- or the flag that `lld -r` drops.
#[test]
fn either_the_name_or_the_flag_makes_a_segment_thread_local() {
    for (name, flags) in [(".tdata.first", 0u32), (".tbss", 0), ("anything", 0x2)] {
        let module =
            link(vec![object(name, flags)], &LinkOptions::default()).expect("link");
        let read = read(&module);
        assert_eq!(read.globals.len(), 6, "{name} flags={flags:#x}");
        assert_eq!(read.globals[4].1, 8, "__tls_size for {name}");
        let main = read.functions.first().expect("a body for main");
        assert_eq!(sleb5(main, FIRST_SLOT as usize), 0, "{name}");
        assert_eq!(sleb5(main, SECOND_SLOT as usize), 4, "{name}");
    }
}

/// A segment nothing says is thread-local holds symbols nothing says are
/// either, and a link where the two disagree is refused rather than laid out
/// from one of them.
#[test]
fn a_symbol_and_its_segment_have_to_agree() {
    // The symbols still claim to be thread-local; the segment no longer is.
    let error = link(vec![object("ordinary", 0)], &LinkOptions::default())
        .expect_err("a TLS symbol in a segment that is not")
        .to_string();
    assert!(error.contains("first"), "{error}");
    assert!(error.contains("thread-local"), "{error}");

    // And the other way: an ordinary symbol in a thread-local segment.
    let mut obj = object(".tdata", 0);
    obj.symbols[1].flags = SymbolFlags::empty();
    let error = link(vec![obj], &LinkOptions::default())
        .expect_err("a plain symbol in a TLS segment")
        .to_string();
    assert!(error.contains("first"), "{error}");
}

/// A thread-local reached as if it were at an address would be one thread's
/// copy, shared by every thread that ran the code. Nothing this linker links
/// does that, and one arriving is refused rather than placed.
#[test]
fn a_thread_local_cannot_be_reached_by_address() {
    let mut obj = object(".tdata", 0);
    obj.code_relocs[0].ty = RelocationType::MemoryAddrSleb;
    let error = link(vec![obj], &LinkOptions::default())
        .expect_err("an absolute address for a thread-local");
    // The whole chain: `patch` wraps its failures in which section they were
    // in, and the detail is under that.
    let error = format!("{error:#}");
    assert!(error.contains("first"), "{error}");
    assert!(error.contains("thread-local"), "{error}");
}
