//! Byte-budgeted AIR reuse. Weak entries preserve the canonical body while a
//! walker/compiler owns it, without pinning every body compiled by the process.
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::{Arc, Weak};

struct Entry<V> {
    weak: Weak<V>,
    resident: Option<Arc<V>>,
    bytes: usize,
    touched: u64,
}

pub(crate) struct BodyCache<K, V> {
    entries: HashMap<K, Entry<V>>,
    budget: usize,
    resident_bytes: usize,
    clock: u64,
    pub(crate) hits: usize,
    pub(crate) misses: usize,
    pub(crate) evictions: usize,
}

impl<K: Copy + Eq + Hash, V> BodyCache<K, V> {
    pub(crate) fn new(budget: usize) -> Self {
        Self {
            entries: HashMap::new(),
            budget,
            resident_bytes: 0,
            clock: 0,
            hits: 0,
            misses: 0,
            evictions: 0,
        }
    }

    pub(crate) fn get(&mut self, key: &K) -> Option<Arc<V>> {
        self.clock += 1;
        let hit = self.entries.get_mut(key).and_then(|e| {
            let body = e.weak.upgrade()?;
            e.touched = self.clock;
            Some(body)
        });
        if hit.is_some() {
            self.hits += 1;
        } else {
            self.misses += 1;
        }
        hit
    }

    /// Called after lowering outside the lock. A racing caller adopts the
    /// winner, so all active users of this key share identical value/pc IDs.
    pub(crate) fn insert(&mut self, key: K, body: Arc<V>, bytes: usize) -> Arc<V> {
        if let Some(existing) = self.entries.get(&key).and_then(|e| e.weak.upgrade()) {
            return existing;
        }
        self.clock += 1;
        if self.clock & 63 == 0 {
            self.entries.retain(|_, e| e.weak.strong_count() != 0);
        }
        let resident = if bytes <= self.budget {
            while self.resident_bytes > self.budget - bytes {
                let Some(oldest) = self
                    .entries
                    .iter()
                    .filter(|(_, e)| e.resident.is_some())
                    .min_by_key(|(_, e)| e.touched)
                    .map(|(key, _)| *key)
                else {
                    break;
                };
                let e = self.entries.get_mut(&oldest).unwrap();
                self.resident_bytes -= e.bytes;
                e.resident = None;
                self.evictions += 1;
            }
            self.resident_bytes += bytes;
            Some(Arc::clone(&body))
        } else {
            None
        };
        self.entries.insert(
            key,
            Entry {
                weak: Arc::downgrade(&body),
                resident,
                bytes,
                touched: self.clock,
            },
        );
        body
    }

    pub(crate) fn clear(&mut self) {
        self.entries.clear();
        self.resident_bytes = 0;
    }

    pub(crate) fn for_each_live(&self, mut visit: impl FnMut(K, &V)) {
        for (key, entry) in &self.entries {
            if let Some(body) = entry.weak.upgrade() {
                visit(*key, &body);
            }
        }
    }

    /// Includes bodies kept alive by interpreters/compilers, separately from
    /// the bounded strong reuse cache. Conservative heap-storage estimates.
    pub(crate) fn storage(&self) -> (usize, usize, usize) {
        let live = self.entries.values().filter(|e| e.weak.strong_count() != 0);
        live.fold((0, 0, self.resident_bytes), |(n, bytes, resident), e| {
            (n + 1, bytes + e.bytes, resident)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eviction_keeps_active_body_canonical_then_releases_it() {
        let mut cache = BodyCache::new(200);
        let active = cache.insert(1, Arc::new(11), 100);
        drop(cache.insert(2, Arc::new(22), 100));
        drop(cache.insert(3, Arc::new(33), 100));
        assert_eq!(cache.storage(), (3, 300, 200));
        assert!(Arc::ptr_eq(&active, &cache.get(&1).unwrap()));
        let weak = Arc::downgrade(&active);
        drop(active);
        assert!(weak.upgrade().is_none());
        assert!(cache.get(&1).is_none());
        assert_eq!(cache.storage(), (2, 200, 200));
    }

    #[test]
    fn oversized_bodies_are_never_pinned_by_cache() {
        let mut cache = BodyCache::new(100);
        let active = cache.insert(1, Arc::new(11), 101);
        assert_eq!(cache.storage(), (1, 101, 0));
        assert!(Arc::ptr_eq(&active, &cache.get(&1).unwrap()));
        drop(active);
        assert_eq!(cache.storage(), (0, 0, 0));
    }

    #[test]
    fn racing_insert_adopts_winner_and_invalidation_preserves_active_calls() {
        let mut cache = BodyCache::new(100);
        let active = cache.insert(1, Arc::new(11), 80);
        let winner = cache.insert(1, Arc::new(22), 80);
        assert!(Arc::ptr_eq(&active, &winner));
        cache.clear();
        assert_eq!(cache.storage(), (0, 0, 0));
        assert_eq!(*active, 11);
        let replacement = cache.insert(1, Arc::new(33), 80);
        assert!(!Arc::ptr_eq(&active, &replacement));
    }
}
