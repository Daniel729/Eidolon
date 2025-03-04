use std::{
    mem::MaybeUninit,
    num::{NonZeroU16, NonZeroUsize},
    simd::{Simd, cmp::SimdPartialEq, num::SimdUint},
};

pub struct TranspositionHashMap<T: Copy> {
    entries: Box<[MaybeUninit<(u64, T)>]>,
    relevance: Box<[u16]>,
    ctrl: Box<[u8]>,
    indexing_capacity: NonZeroUsize,
    len: usize,
}

const FP_MASK: u8 = 0b0111_1111;
const EMPTY_FP: u8 = 0b1000_0000;
const EMPTY_REL: u16 = 0;
const PROBES: usize = 16;

impl<T: Copy> TranspositionHashMap<T> {
    pub fn new(size_mib: NonZeroUsize) -> Self {
        const MIB: usize = 1024 * 1024;

        let capacity =
            size_mib.get() * MIB / (size_of::<(u64, T)>() + size_of::<u16>() + size_of::<u8>());

        const { assert!(MIB / size_of::<Option<(u64, T)>>() > PROBES) };

        Self {
            entries: Box::new_uninit_slice(capacity),
            relevance: vec![EMPTY_REL; capacity].into_boxed_slice(),
            ctrl: vec![EMPTY_FP; capacity].into_boxed_slice(),
            indexing_capacity: NonZeroUsize::new(capacity - PROBES + 1).unwrap(),
            len: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.indexing_capacity() + PROBES - 1
    }

    fn indexing_capacity(&self) -> usize {
        self.indexing_capacity.get()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn clear(&mut self) {
        self.len = 0;

        for ctrl in self.ctrl.iter_mut() {
            *ctrl = EMPTY_FP;
        }

        for rel in self.relevance.iter_mut() {
            *rel = 0;
        }
    }

    fn get_bucket_fp(&self, hash: u64) -> (usize, u8) {
        let fp = (hash >> 57) as u8 & FP_MASK;

        let bucket = (hash & ((1 << 57) - 1)) as usize % self.indexing_capacity();

        (bucket, fp)
    }

    pub fn get(&self, hash: u64) -> Option<&T> {
        // SAFETY: unsafe is used to avoid array bound checking,
        // and to use manually-initialized memory.

        let (bucket, fp) = self.get_bucket_fp(hash);

        let ctrl_simd = Simd::<u8, PROBES>::from_slice(unsafe {
            self.ctrl.get_unchecked(bucket..bucket + PROBES)
        });

        let mut fp_matches = ctrl_simd.simd_eq(Simd::splat(fp)).to_bitmask();

        while fp_matches != 0 {
            let idx = bucket + fp_matches.trailing_zeros() as usize;

            let entry = unsafe { self.entries.get_unchecked(idx).assume_init_ref() };

            if entry.0 == hash {
                return Some(&entry.1);
            }

            fp_matches &= fp_matches - 1;
        }

        None
    }

    // The new entry will always be stored even if it is not the best in it's buckets (relevance-wise)
    pub fn insert(&mut self, new_entry: T, hash: u64, relevance: NonZeroU16) {
        // SAFETY: unsafe is used to avoid array bound checking,
        // and to use manually-initialized memory.

        let relevance = relevance.get();

        let (bucket, fp) = self.get_bucket_fp(hash);

        let ctrl_simd = Simd::<u8, PROBES>::from_slice(unsafe {
            self.ctrl.get_unchecked(bucket..bucket + PROBES)
        });

        let mut fp_matches = ctrl_simd.simd_eq(Simd::splat(fp)).to_bitmask();

        while fp_matches > 0 {
            let idx = bucket + fp_matches.trailing_zeros() as usize;

            let entry = unsafe { self.entries.get_unchecked_mut(idx).assume_init_mut() };

            if entry.0 == hash {
                let entry_relevance = unsafe { self.relevance.get_unchecked_mut(idx) };

                if relevance >= *entry_relevance {
                    entry.1 = new_entry;
                    *entry_relevance = relevance;
                }

                return;
            }

            fp_matches &= fp_matches - 1;
        }

        let rel_simd = Simd::<u16, PROBES>::from_slice(unsafe {
            self.relevance.get_unchecked(bucket..bucket + PROBES)
        });

        let worst_rel = rel_simd.reduce_min();

        let idx = bucket
            + rel_simd
                .simd_eq(Simd::splat(worst_rel))
                .to_bitmask()
                .trailing_zeros() as usize;

        unsafe {
            self.entries.get_unchecked_mut(idx).write((hash, new_entry));
            *self.ctrl.get_unchecked_mut(idx) = fp;
            *self.relevance.get_unchecked_mut(idx) = relevance;
        };

        if worst_rel == EMPTY_REL {
            self.len += 1;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_insert_and_get() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        map.insert(10, 42, NonZeroU16::new(10).unwrap());
        assert_eq!(map.get(42), Some(&10));
    }

    #[test]
    fn test_insert_replaces_if_better() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        map.insert(10, 42, NonZeroU16::new(10).unwrap());
        map.insert(20, 42, NonZeroU16::new(20).unwrap());
        assert_eq!(map.get(42), Some(&20));
    }

    #[test]
    fn test_insert_does_not_replace_if_worse() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        map.insert(20, 42, NonZeroU16::new(20).unwrap());
        map.insert(10, 42, NonZeroU16::new(10).unwrap());
        assert_eq!(map.get(42), Some(&20));
    }

    #[test]
    fn test_insert_finds_best_replacement() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        let hash1 = 42;
        let hash2 = 43;

        map.insert(10, hash1, NonZeroU16::new(10).unwrap());
        assert_eq!(map.get(hash1), Some(&10));

        map.insert(5, hash2, NonZeroU16::new(5).unwrap());
        assert_eq!(map.get(hash2), Some(&5));

        map.insert(15, hash2, NonZeroU16::new(15).unwrap());

        assert_eq!(map.get(hash1), Some(&10));
        assert_eq!(map.get(hash2), Some(&15));
    }

    #[test]
    fn test_insert_worse_then_better() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        let hash = 42;

        map.insert(5, hash, NonZeroU16::new(5).unwrap());
        assert_eq!(map.get(hash), Some(&5));
        map.insert(3, hash, NonZeroU16::new(3).unwrap());
        assert_eq!(map.get(hash), Some(&5));
        map.insert(8, hash, NonZeroU16::new(8).unwrap());
        assert_eq!(map.get(hash), Some(&8));
    }

    #[test]
    fn test_clear() {
        let mut map = TranspositionHashMap::new(NonZeroUsize::new(1).unwrap());

        map.insert(10, 42, NonZeroU16::new(10).unwrap());
        assert_eq!(map.len(), 1);

        map.clear();
        assert_eq!(map.len(), 0);
        assert_eq!(map.get(42), None);
    }

    #[test]
    fn test_capacity() {
        let map = TranspositionHashMap::<i32>::new(NonZeroUsize::new(1).unwrap());
        assert!(map.capacity() > 0);
    }
}
