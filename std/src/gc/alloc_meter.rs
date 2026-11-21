//! The totals behind `hl.Gc.stats()`: bytes handed out to the program and how
//! many objects that was.
//!
//! `GC_STATS.bytes_allocated` charges a whole region when one is carved, so it
//! moves in steps of a block and says nothing about a small window. These
//! totals are read off the bump cursors and the object table, so the
//! allocation fast path, including the inline path in compiled code, does no
//! work for them.
//!
//! Each total has three parts:
//!
//! * `heap.locked_bytes` and `heap.locked_objects`: what the locked allocator
//!   served, and large objects at their requested size.
//! * [`RETIRED_BYTES`] and [`RETIRED_OBJECTS`]: what was bumped out of regions
//!   their thread has given up, either to take a new region or on exit.
//! * Each registered thread's open regions, between `start` and `cur`.
//!
//! The object table holds one entry per object start in a region, set by the
//! bump and cleared only when the region's memory is reused, so a region's
//! exact bytes and count are the sum and the number of non-zero entries
//! between `start` and `cur`. That skips the unused end of a line when an
//! object did not fit in it, which the cursor alone cannot tell from an object.
//! Reading it costs a pass over the table, so it starts at the first read of
//! the totals: until then a retiring region adds `cur - start` bytes (line
//! tails included) and no objects, and a program that never reads them pays
//! one subtraction per refill.
//!
//! Only registered mutators are counted, on both paths. A thread the runtime
//! starts for itself, such as a compiler thread, allocates without registering
//! and is not the program, so a window measuring the program does not see it.
//!
//! Another thread's cursor is read while that thread may be bumping it, so the
//! totals can trail that thread by the allocations in flight, and never run
//! ahead of what it has bumped.

use super::*;

/// Bytes and objects bumped out of regions that have been retired.
static RETIRED_BYTES: AtomicU64 = AtomicU64::new(0);
static RETIRED_OBJECTS: AtomicU64 = AtomicU64::new(0);

/// Whether the totals have been read, which is when regions start being
/// counted from the object table.
static METERED: AtomicBool = AtomicBool::new(false);

/// What one open region of this thread has handed out: bytes and objects.
/// `exact` reads the object table; otherwise it is the cursor distance and no
/// objects.
fn region_totals(t: &Tlab, noptr: bool, exact: bool) -> (u64, u64) {
    let (cur, start) = if noptr {
        (&t.noptr_cur, &t.noptr_start)
    } else {
        (&t.cur, &t.start)
    };
    // `start` first: it only changes under the GC lock, and the cursor never
    // falls below it, so a cursor read after it cannot come out smaller.
    let start = start.load(Ordering::Relaxed);
    let cur = cur.load(Ordering::Relaxed);
    if start == 0 || cur < start {
        return (0, 0);
    }
    let objects = t.objects.get();
    if !exact || objects.is_null() {
        return ((cur - start) as u64, 0);
    }
    let base = t.heap_base.get();
    let (mut bytes, mut count) = (0u64, 0u64);
    for q in (start - base) / ALLOC_QUANTUM..(cur - base) / ALLOC_QUANTUM {
        // The marker sets the mark bit on a live object's entry.
        let entry = unsafe { &*objects.add(q) }.load(Ordering::Relaxed) & !OBJECT_MARK;
        bytes += entry as u64 * ALLOC_QUANTUM as u64;
        count += (entry != 0) as u64;
    }
    (bytes, count)
}

/// Move what this thread's current region of this kind has handed out into
/// the retired totals and mark the region as having none.
///
/// Called with the GC lock held, or with the mutator world locked, so
/// [`total`], which holds both, sees them in exactly one place.
pub(super) fn retire(t: &Tlab, noptr: bool) {
    let (bytes, objects) = region_totals(t, noptr, METERED.load(Ordering::Relaxed));
    if bytes != 0 {
        RETIRED_BYTES.fetch_add(bytes, Ordering::Relaxed);
        RETIRED_OBJECTS.fetch_add(objects, Ordering::Relaxed);
    }
    let (cur, start) = if noptr {
        (&t.noptr_cur, &t.noptr_start)
    } else {
        (&t.cur, &t.start)
    };
    start.store(cur.load(Ordering::Relaxed), Ordering::Relaxed);
}

/// Bytes and objects handed out to the program: counted from the first call,
/// not from the start of the process, so only differences between calls mean
/// anything. Needs the GC lock, which the caller holds through `gc`.
pub(super) fn total(gc: &ImmixAllocator) -> (u64, u64) {
    METERED.store(true, Ordering::Relaxed);
    let mut bytes = gc.heap.locked_bytes + RETIRED_BYTES.load(Ordering::Relaxed);
    let mut objects = gc.heap.locked_objects + RETIRED_OBJECTS.load(Ordering::Relaxed);
    let world = MUTATOR_WORLD.state.lock().unwrap();
    for m in &world.mutators {
        if m.tlab == 0 {
            continue;
        }
        // The record is removed before its thread's storage is freed.
        let t = unsafe { &*(m.tlab as *const Tlab) };
        for noptr in [false, true] {
            let (b, n) = region_totals(t, noptr, true);
            bytes += b;
            objects += n;
        }
    }
    (bytes, objects)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// What the current thread's open regions have handed out, from the
    /// object table.
    fn own() -> (u64, u64) {
        METERED.store(true, Ordering::Relaxed);
        TLAB.with(|t| {
            let (b, n) = region_totals(t, false, true);
            let (nb, nn) = region_totals(t, true, true);
            (b + nb, n + nn)
        })
    }

    /// Runs `body` on a thread whose bump region lies in a heap of its own, so
    /// no other test's allocation can land in it.
    fn on_isolated_region(body: impl FnOnce() + Send + 'static) {
        if !tlab_enabled() {
            return; // Stress mode disables the bump path.
        }
        std::thread::spawn(move || {
            let mut gc = ImmixAllocator::with_heap_size(BLOCK_SIZE * 4);
            let block = gc.acquire_free_block(false).unwrap();
            let base = gc.heap.memory.as_ptr() as usize;
            adopt_tlab_region(
                &mut gc,
                block,
                base + block,
                base + block,
                base + block + BLOCK_SIZE,
                false,
            );
            TLAB.with(|t| t.registered.set(true));
            body();
            release_tlab_region(&mut gc);
            TLAB.with(|t| t.registered.set(false));
        })
        .join()
        .unwrap();
    }

    #[test]
    fn a_region_counts_the_bytes_and_objects_bumped_out_of_it() {
        on_isolated_region(|| {
            assert_eq!(own(), (0, 0), "a region nothing was taken from");
            for _ in 0..32 {
                gc_alloc(16).unwrap();
            }
            assert_eq!(own(), (512, 32));
        });
    }

    #[test]
    fn a_window_with_no_allocation_reads_zero() {
        on_isolated_region(|| {
            gc_alloc(48).unwrap();
            let before = own();
            std::hint::black_box(vec![1u8; 64]);
            assert_eq!(own(), before);
        });
    }

    #[test]
    fn the_end_of_a_line_an_object_did_not_fit_in_is_not_counted() {
        on_isolated_region(|| {
            // 80 bytes leave 48 in the line; 64 does not fit, so it starts the
            // next line. The cursor has moved by a line and 64, the program
            // was handed 144.
            gc_alloc(80).unwrap();
            gc_alloc(64).unwrap();
            assert_eq!(own(), (144, 2));
            TLAB.with(|t| {
                let cursor = t.cur.load(Ordering::Relaxed) - t.start.load(Ordering::Relaxed);
                assert_eq!(cursor, LINE_SIZE + 64);
            });
        });
    }

    #[test]
    fn a_marked_object_counts_as_much_as_an_unmarked_one() {
        on_isolated_region(|| {
            let p = gc_alloc(32).unwrap();
            TLAB.with(|t| {
                let q = (p.as_ptr() as usize - t.heap_base.get()) / ALLOC_QUANTUM;
                unsafe { &*t.objects.get().add(q) }.fetch_or(OBJECT_MARK, Ordering::Relaxed);
            });
            assert_eq!(own(), (32, 1));
        });
    }

    #[test]
    fn retiring_a_region_moves_its_totals_to_the_retired_ones() {
        on_isolated_region(|| {
            for _ in 0..32 {
                gc_alloc(16).unwrap();
            }
            let before = (
                RETIRED_BYTES.load(Ordering::Relaxed),
                RETIRED_OBJECTS.load(Ordering::Relaxed),
            );
            TLAB.with(|t| retire(t, false));
            assert_eq!(own(), (0, 0), "nothing is counted twice");
            // Other threads may retire at the same time, so only a floor.
            assert!(RETIRED_BYTES.load(Ordering::Relaxed) >= before.0 + 512);
            assert!(RETIRED_OBJECTS.load(Ordering::Relaxed) >= before.1 + 32);
            gc_alloc(16).unwrap();
            assert_eq!(own(), (16, 1), "counting resumes from the cursor");
        });
    }

    #[test]
    fn the_totals_count_another_threads_open_region_and_keep_it_when_the_thread_ends() {
        if !tlab_enabled() {
            return;
        }
        // Unregistering a thread releases its region in the process heap.
        drop(gc_locked_init());
        let gc = ImmixAllocator::with_heap_size(BLOCK_SIZE);
        let (ready_tx, ready_rx) = std::sync::mpsc::channel::<()>();
        let (go_tx, go_rx) = std::sync::mpsc::channel::<()>();
        let (taken_tx, taken_rx) = std::sync::mpsc::channel::<()>();
        let (done_tx, done_rx) = std::sync::mpsc::channel::<()>();
        let worker = std::thread::spawn(move || {
            let mut heap = ImmixAllocator::with_heap_size(BLOCK_SIZE * 4);
            let block = heap.acquire_free_block(false).unwrap();
            let base = heap.heap.memory.as_ptr() as usize;
            adopt_tlab_region(
                &mut heap,
                block,
                base + block,
                base + block,
                base + block + BLOCK_SIZE,
                false,
            );
            gc_register_current_os_thread();
            ready_tx.send(()).unwrap();
            go_rx.recv().unwrap();
            for _ in 0..64 {
                gc_alloc(16).unwrap();
            }
            taken_tx.send(()).unwrap();
            done_rx.recv().unwrap();
            gc_unregister_current_os_thread();
        });
        ready_rx.recv().unwrap();
        let before = total(&gc);
        go_tx.send(()).unwrap();
        taken_rx.recv().unwrap();
        let during = total(&gc);
        // Other threads in the process can only add to what is counted.
        assert!(
            during.0 >= before.0 + 64 * 16 && during.1 >= before.1 + 64,
            "the open region of a running thread is counted: {before:?} -> {during:?}"
        );
        done_tx.send(()).unwrap();
        worker.join().unwrap();
        let after = total(&gc);
        assert!(
            after.0 >= during.0 && after.1 >= during.1,
            "a thread that ends keeps what it was handed: {during:?} -> {after:?}"
        );
    }

    #[test]
    fn the_locked_allocator_counts_its_own_bytes_and_objects() {
        std::thread::spawn(|| {
            let mut gc = ImmixAllocator::with_heap_size(BLOCK_SIZE * 4);
            gc.allocate_kind(24, false).unwrap();
            assert_eq!(
                (gc.heap.locked_bytes, gc.heap.locked_objects),
                (0, 0),
                "a thread that is not a registered mutator is not the program"
            );
            TLAB.with(|t| t.registered.set(true));
            gc.allocate_kind(40, false).unwrap();
            gc.allocate_kind(200, false).unwrap();
            TLAB.with(|t| t.registered.set(false));
            assert_eq!((gc.heap.locked_bytes, gc.heap.locked_objects), (48 + 208, 2));
        })
        .join()
        .unwrap();
    }
}
