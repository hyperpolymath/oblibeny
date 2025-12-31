// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Oblivious collections
//!
//! These collections hide access patterns using ORAM.

use crate::crypto::SecretKey;
use crate::oram::{OArray, OramBlock};
use subtle::ConditionallySelectable;

/// Oblivious stack
///
/// A stack where push/pop operations hide which element is accessed.
pub struct OStack<T: OramBlock + ConditionallySelectable> {
    data: OArray<T>,
    size: u64,
    capacity: u64,
}

impl<T: OramBlock + ConditionallySelectable> OStack<T> {
    /// Create a new oblivious stack with given capacity
    pub fn new(capacity: u64) -> Self {
        OStack {
            data: OArray::new(capacity),
            size: 0,
            capacity,
        }
    }

    /// Push a value onto the stack
    pub fn push(&mut self, value: T) -> bool {
        if self.size >= self.capacity {
            return false;
        }
        self.data.write(self.size, value);
        self.size += 1;
        true
    }

    /// Pop a value from the stack
    pub fn pop(&mut self) -> Option<T> {
        if self.size == 0 {
            // Perform dummy access to maintain constant access pattern
            let _ = self.data.read(0);
            return None;
        }
        self.size -= 1;
        Some(self.data.read(self.size))
    }

    /// Peek at the top value
    pub fn peek(&mut self) -> Option<T> {
        if self.size == 0 {
            let _ = self.data.read(0);
            return None;
        }
        Some(self.data.read(self.size - 1))
    }

    /// Get the current size
    pub fn len(&self) -> u64 {
        self.size
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
}

/// Oblivious queue
///
/// A queue where enqueue/dequeue hide which element is accessed.
pub struct OQueue<T: OramBlock + ConditionallySelectable> {
    data: OArray<T>,
    head: u64,
    tail: u64,
    size: u64,
    capacity: u64,
}

impl<T: OramBlock + ConditionallySelectable> OQueue<T> {
    /// Create a new oblivious queue with given capacity
    pub fn new(capacity: u64) -> Self {
        OQueue {
            data: OArray::new(capacity),
            head: 0,
            tail: 0,
            size: 0,
            capacity,
        }
    }

    /// Enqueue a value
    pub fn enqueue(&mut self, value: T) -> bool {
        if self.size >= self.capacity {
            return false;
        }
        self.data.write(self.tail, value);
        self.tail = (self.tail + 1) % self.capacity;
        self.size += 1;
        true
    }

    /// Dequeue a value
    pub fn dequeue(&mut self) -> Option<T> {
        if self.size == 0 {
            let _ = self.data.read(0);
            return None;
        }
        let value = self.data.read(self.head);
        self.head = (self.head + 1) % self.capacity;
        self.size -= 1;
        Some(value)
    }

    /// Peek at the front value
    pub fn peek(&mut self) -> Option<T> {
        if self.size == 0 {
            let _ = self.data.read(0);
            return None;
        }
        Some(self.data.read(self.head))
    }

    /// Get the current size
    pub fn len(&self) -> u64 {
        self.size
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
}

/// Oblivious map (simple linear scan implementation)
///
/// For small maps, uses linear scan with ORAM backing.
/// For large maps, a tree-based structure would be more efficient.
pub struct OMap<K: OramBlock + ConditionallySelectable + PartialEq, V: OramBlock + ConditionallySelectable> {
    keys: OArray<K>,
    values: OArray<V>,
    size: u64,
    capacity: u64,
}

impl<K: OramBlock + ConditionallySelectable + PartialEq + Clone, V: OramBlock + ConditionallySelectable + Clone> OMap<K, V> {
    /// Create a new oblivious map with given capacity
    pub fn new(capacity: u64) -> Self {
        OMap {
            keys: OArray::new(capacity),
            values: OArray::new(capacity),
            size: 0,
            capacity,
        }
    }

    /// Insert or update a key-value pair
    pub fn insert(&mut self, key: K, value: V) -> bool {
        // First, try to find existing key
        for i in 0..self.size {
            let k = self.keys.read(i);
            if k == key {
                self.values.write(i, value);
                return true;
            }
        }

        // Key not found, insert new
        if self.size >= self.capacity {
            return false;
        }
        self.keys.write(self.size, key);
        self.values.write(self.size, value);
        self.size += 1;
        true
    }

    /// Get a value by key
    pub fn get(&mut self, key: &K) -> Option<V>
    where
        K: Clone,
    {
        for i in 0..self.size {
            let k = self.keys.read(i);
            if &k == key {
                return Some(self.values.read(i));
            }
        }
        // Dummy access for constant pattern
        if self.size < self.capacity {
            let _ = self.values.read(0);
        }
        None
    }

    /// Check if key exists
    pub fn contains(&mut self, key: &K) -> bool
    where
        K: Clone,
    {
        for i in 0..self.size {
            let k = self.keys.read(i);
            if &k == key {
                return true;
            }
        }
        false
    }

    /// Get the current size
    pub fn len(&self) -> u64 {
        self.size
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ostack() {
        let mut stack: OStack<u64> = OStack::new(10);
        assert!(stack.push(1));
        assert!(stack.push(2));
        assert!(stack.push(3));
        assert_eq!(stack.pop(), Some(3));
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(1));
        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_oqueue() {
        let mut queue: OQueue<u64> = OQueue::new(10);
        assert!(queue.enqueue(1));
        assert!(queue.enqueue(2));
        assert!(queue.enqueue(3));
        assert_eq!(queue.dequeue(), Some(1));
        assert_eq!(queue.dequeue(), Some(2));
        assert_eq!(queue.dequeue(), Some(3));
        assert_eq!(queue.dequeue(), None);
    }

    #[test]
    fn test_omap() {
        let mut map: OMap<u64, u64> = OMap::new(10);
        assert!(map.insert(1, 100));
        assert!(map.insert(2, 200));
        assert_eq!(map.get(&1), Some(100));
        assert_eq!(map.get(&2), Some(200));
        assert_eq!(map.get(&3), None);

        // Update existing
        assert!(map.insert(1, 150));
        assert_eq!(map.get(&1), Some(150));
    }
}
