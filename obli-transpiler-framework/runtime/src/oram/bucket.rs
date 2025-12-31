// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! ORAM bucket implementation
//!
//! A bucket is a fixed-size container of blocks in the ORAM tree.

use super::OramBlock;
use crate::constant_time::{ct_lookup, ct_store};
use subtle::{Choice, ConditionallySelectable};

/// Number of blocks per bucket (Z parameter in Path ORAM)
pub const BUCKET_SIZE: usize = 4;

/// A single entry in a bucket
#[derive(Clone)]
pub struct BucketEntry<T: OramBlock> {
    /// The logical address (u64::MAX means empty/dummy)
    pub addr: u64,
    /// The data block
    pub data: T,
}

impl<T: OramBlock> Default for BucketEntry<T> {
    fn default() -> Self {
        BucketEntry {
            addr: u64::MAX, // Empty marker
            data: T::default(),
        }
    }
}

impl<T: OramBlock> ConditionallySelectable for BucketEntry<T>
where
    T: ConditionallySelectable,
{
    fn conditional_select(a: &Self, b: &Self, choice: Choice) -> Self {
        BucketEntry {
            addr: u64::conditional_select(&a.addr, &b.addr, choice),
            data: T::conditional_select(&a.data, &b.data, choice),
        }
    }
}

/// A bucket containing multiple entries
#[derive(Clone)]
pub struct Bucket<T: OramBlock> {
    entries: [BucketEntry<T>; BUCKET_SIZE],
}

impl<T: OramBlock> Default for Bucket<T> {
    fn default() -> Self {
        Bucket {
            entries: std::array::from_fn(|_| BucketEntry::default()),
        }
    }
}

impl<T: OramBlock> Bucket<T> {
    /// Create a new empty bucket
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if the bucket is full
    pub fn is_full(&self) -> bool {
        self.entries.iter().all(|e| e.addr != u64::MAX)
    }

    /// Count non-empty entries
    pub fn count(&self) -> usize {
        self.entries.iter().filter(|e| e.addr != u64::MAX).count()
    }

    /// Try to add an entry (returns false if full)
    pub fn try_add(&mut self, addr: u64, data: T) -> bool {
        for entry in &mut self.entries {
            if entry.addr == u64::MAX {
                entry.addr = addr;
                entry.data = data;
                return true;
            }
        }
        false
    }

    /// Read and remove entry with given address (constant-time)
    ///
    /// Returns the data if found, None otherwise.
    /// The entry is marked as empty.
    pub fn read_and_remove(&mut self, addr: u64) -> Option<T>
    where
        T: ConditionallySelectable + Clone,
    {
        let mut found = false;
        let mut result = T::default();

        for entry in &mut self.entries {
            let matches = entry.addr == addr;
            if matches {
                found = true;
                result = entry.data.clone();
                entry.addr = u64::MAX;
                entry.data = T::default();
            }
        }

        if found {
            Some(result)
        } else {
            None
        }
    }

    /// Read entry with given address without removing (constant-time)
    pub fn read(&self, addr: u64) -> Option<T>
    where
        T: ConditionallySelectable + Clone,
    {
        for entry in &self.entries {
            if entry.addr == addr {
                return Some(entry.data.clone());
            }
        }
        None
    }

    /// Get entries as slice
    pub fn entries(&self) -> &[BucketEntry<T>; BUCKET_SIZE] {
        &self.entries
    }

    /// Get mutable entries
    pub fn entries_mut(&mut self) -> &mut [BucketEntry<T>; BUCKET_SIZE] {
        &mut self.entries
    }

    /// Drain all real (non-dummy) entries
    pub fn drain_real(&mut self) -> Vec<(u64, T)> {
        let mut result = Vec::new();
        for entry in &mut self.entries {
            if entry.addr != u64::MAX {
                result.push((entry.addr, entry.data.clone()));
                entry.addr = u64::MAX;
                entry.data = T::default();
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bucket_add_read() {
        let mut bucket: Bucket<u64> = Bucket::new();
        assert!(bucket.try_add(10, 100));
        assert!(bucket.try_add(20, 200));
        assert_eq!(bucket.read(10), Some(100));
        assert_eq!(bucket.read(20), Some(200));
        assert_eq!(bucket.read(30), None);
    }

    #[test]
    fn test_bucket_full() {
        let mut bucket: Bucket<u64> = Bucket::new();
        for i in 0..BUCKET_SIZE {
            assert!(bucket.try_add(i as u64, i as u64 * 10));
        }
        assert!(!bucket.try_add(100, 1000));
    }

    #[test]
    fn test_bucket_read_and_remove() {
        let mut bucket: Bucket<u64> = Bucket::new();
        bucket.try_add(10, 100);
        assert_eq!(bucket.read_and_remove(10), Some(100));
        assert_eq!(bucket.read_and_remove(10), None);
    }
}
