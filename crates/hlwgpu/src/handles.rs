//! Integer handles for objects that live in Rust.
//!
//! Layout: kind in bits 27..30, generation in 20..26, slot index in 0..19.
//! Zero is never a handle. The generation makes a destroyed handle raise
//! rather than reach whatever took its slot; the kind stops a buffer being
//! read as a texture. `js/prelude.js` uses the same layout.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::Arc;

use crate::bindings::kinds::Kind;

const INDEX_BITS: i32 = 20;
const GEN_BITS: i32 = 7;
const INDEX_MASK: i32 = (1 << INDEX_BITS) - 1;
const GEN_MASK: i32 = (1 << GEN_BITS) - 1;

/// Live objects of one kind.
pub struct Slab<T> {
    kind: Kind,
    obj: Vec<Option<Arc<T>>>,
    generation: Vec<i32>,
    free: VecDeque<usize>,
}

impl<T> Slab<T> {
    pub const fn new(kind: Kind) -> Self {
        Slab { kind, obj: Vec::new(), generation: Vec::new(), free: VecDeque::new() }
    }

    /// Stores `value`; 0 if this kind is full.
    pub fn put(&mut self, value: T) -> i32 {
        let index = match self.free.pop_front() {
            // Oldest free slot first: taking the newest would cycle one slot's
            // generation back to a value a stale handle still holds.
            Some(i) => {
                self.generation[i] = (self.generation[i] + 1) & GEN_MASK;
                i
            }
            None => {
                let i = self.obj.len();
                if i as i32 > INDEX_MASK {
                    return 0;
                }
                self.obj.push(None);
                self.generation.push(0);
                i
            }
        };
        self.obj[index] = Some(Arc::new(value));
        ((self.kind as i32) << (INDEX_BITS + GEN_BITS))
            | (self.generation[index] << INDEX_BITS)
            | index as i32
    }

    /// The object, if the handle is live and of this kind.
    pub fn get(&self, handle: i32) -> Option<Arc<T>> {
        let index = self.slot(handle)?;
        self.obj[index].clone()
    }

    /// Releases a handle. Doing it twice does nothing: ash runs no finalizer,
    /// so `destroy()` is the only path and has to tolerate being repeated.
    pub fn remove(&mut self, handle: i32) {
        let Some(index) = self.slot(handle) else { return };
        if self.obj[index].take().is_some() {
            self.free.push_back(index);
        }
    }

    /// Live handles of this kind. What a leak check reads.
    pub fn live(&self) -> usize {
        self.obj.iter().filter(|slot| slot.is_some()).count()
    }

    /// The slot a handle names, if kind and generation both match.
    fn slot(&self, handle: i32) -> Option<usize> {
        if handle <= 0 || handle >> (INDEX_BITS + GEN_BITS) != self.kind as i32 {
            return None;
        }
        let index = (handle & INDEX_MASK) as usize;
        if self.generation.get(index).copied()? != (handle >> INDEX_BITS) & GEN_MASK {
            return None;
        }
        Some(index)
    }
}

/// An operation the caller started and polls for.
///
/// `done` and `result` are shared with whatever finishes the work -- a wgpu
/// callback, or nothing at all when the answer was ready immediately.
struct PendingRequest {
    done: Arc<AtomicBool>,
    result: Arc<AtomicI32>,
    /// Polled while waiting. Natively a callback runs only when the device is
    /// asked; in a page the event loop does it and this is 0.
    device: i32,
}

/// In-flight requests, by id.
pub struct PendingRequests {
    next: i32,
    slots: HashMap<i32, PendingRequest>,
}

impl Default for PendingRequests {
    fn default() -> Self {
        PendingRequests { next: 1, slots: HashMap::new() }
    }
}

impl PendingRequests {
    /// An id for work that has already finished.
    pub fn settled(&mut self, result: i32) -> i32 {
        self.add(PendingRequest {
            done: Arc::new(AtomicBool::new(true)),
            result: Arc::new(AtomicI32::new(result)),
            device: 0,
        })
    }

    /// An id for work in flight. `done` is what the callback sets.
    pub fn waiting(&mut self, done: Arc<AtomicBool>, result: Arc<AtomicI32>, device: i32) -> i32 {
        self.add(PendingRequest { done, result, device })
    }

    fn add(&mut self, request: PendingRequest) -> i32 {
        let id = self.next;
        self.next += 1;
        self.slots.insert(id, request);
        id
    }

    pub fn ready(&self, id: i32) -> bool {
        self.slots.get(&id).is_some_and(|r| r.done.load(Ordering::Acquire))
    }

    /// The device to poll before asking again, or 0.
    pub fn device_of(&self, id: i32) -> i32 {
        self.slots.get(&id).map_or(0, |r| r.device)
    }

    /// Collects a finished request and forgets it; 0 if unknown or unfinished.
    pub fn take(&mut self, id: i32) -> i32 {
        match self.slots.get(&id) {
            Some(r) if r.done.load(Ordering::Acquire) => {
                let result = r.result.load(Ordering::Acquire);
                self.slots.remove(&id);
                result
            }
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slab() -> Slab<i32> {
        Slab::new(Kind::Buffer)
    }

    #[test]
    fn a_handle_finds_what_was_put_in_it() {
        let mut s = slab();
        let h = s.put(7);
        assert_ne!(h, 0, "a handle is never zero");
        assert_eq!(s.get(h).as_deref(), Some(&7));
        assert_eq!(s.live(), 1);
    }

    #[test]
    fn a_destroyed_handle_stops_resolving() {
        let mut s = slab();
        let h = s.put(7);
        s.remove(h);
        assert!(s.get(h).is_none());
        assert_eq!(s.live(), 0);
        // Twice is not an error: ash runs no finalizer, so destroy() is the
        // only path and callers are expected to be defensive.
        s.remove(h);
    }

    #[test]
    fn a_reused_slot_does_not_answer_the_old_handle() {
        let mut s = slab();
        let old = s.put(7);
        s.remove(old);
        let new = s.put(9);
        assert_ne!(old, new, "the generation makes the handle different");
        assert!(s.get(old).is_none());
        assert_eq!(s.get(new).as_deref(), Some(&9));
    }

    #[test]
    fn a_handle_of_another_kind_is_refused() {
        let mut buffers = slab();
        let h = buffers.put(7);
        let textures: Slab<i32> = Slab::new(Kind::Texture);
        assert!(textures.get(h).is_none(), "kind is checked, not just the slot");
    }

    #[test]
    fn slots_are_reused_oldest_first() {
        // Taking the newest free slot would cycle one slot's generation back
        // to a value a stale handle still holds.
        let mut s = slab();
        let (a, b) = (s.put(1), s.put(2));
        s.remove(a);
        s.remove(b);
        assert_eq!(s.put(3) & INDEX_MASK, a & INDEX_MASK);
        assert_eq!(s.put(4) & INDEX_MASK, b & INDEX_MASK);
    }

    #[test]
    fn a_request_is_collected_once() {
        let mut r = PendingRequests::default();
        let id = r.settled(42);
        assert!(r.ready(id));
        assert_eq!(r.take(id), 42);
        assert!(!r.ready(id), "collecting forgets it");
        assert_eq!(r.take(id), 0);
    }

    #[test]
    fn a_waiting_request_is_not_ready_until_its_flag_is_set() {
        let mut r = PendingRequests::default();
        let done = Arc::new(AtomicBool::new(false));
        let result = Arc::new(AtomicI32::new(0));
        let id = r.waiting(done.clone(), result.clone(), 42);
        assert!(!r.ready(id));
        assert_eq!(r.take(id), 0, "an unfinished request has no result");
        assert_eq!(r.device_of(id), 42, "and says what to poll");

        result.store(1, Ordering::Release);
        done.store(true, Ordering::Release);
        assert!(r.ready(id));
        assert_eq!(r.take(id), 1);
    }
}
