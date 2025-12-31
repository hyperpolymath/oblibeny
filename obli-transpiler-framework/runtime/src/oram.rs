// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! ORAM (Oblivious RAM) implementations
//!
//! This module provides oblivious memory access through Path ORAM,
//! hiding access patterns from observers.

mod path;
mod position;
mod stash;
mod bucket;

pub use path::PathOram;
pub use position::PositionMap;
pub use stash::Stash;
pub use bucket::Bucket;

use crate::crypto::SecretKey;

/// Trait for types that can be stored in ORAM
pub trait OramBlock: Clone + Default + Sized {
    /// Size of the block in bytes
    const SIZE: usize;

    /// Serialize to bytes
    fn to_bytes(&self) -> Vec<u8>;

    /// Deserialize from bytes
    fn from_bytes(bytes: &[u8]) -> Self;
}

/// Implement OramBlock for primitive types
macro_rules! impl_oram_block_primitive {
    ($($t:ty),*) => {
        $(
            impl OramBlock for $t {
                const SIZE: usize = std::mem::size_of::<$t>();

                fn to_bytes(&self) -> Vec<u8> {
                    self.to_le_bytes().to_vec()
                }

                fn from_bytes(bytes: &[u8]) -> Self {
                    let arr: [u8; std::mem::size_of::<$t>()] =
                        bytes.try_into().unwrap_or([0; std::mem::size_of::<$t>()]);
                    <$t>::from_le_bytes(arr)
                }
            }
        )*
    };
}

impl_oram_block_primitive!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128);

/// ORAM access trait
pub trait OramAccess<T: OramBlock> {
    /// Read a value at the given logical address
    fn oram_read(&mut self, addr: u64) -> T;

    /// Write a value at the given logical address
    fn oram_write(&mut self, addr: u64, value: T);

    /// Get the capacity (number of blocks)
    fn capacity(&self) -> u64;
}

/// Oblivious array type - the main interface for ORAM access
pub struct OArray<T: OramBlock> {
    oram: PathOram<T>,
}

impl<T: OramBlock> OArray<T> {
    /// Create a new oblivious array with the given capacity
    pub fn new(capacity: u64) -> Self {
        OArray {
            oram: PathOram::new(capacity, SecretKey::generate()),
        }
    }

    /// Create with a specific key (for testing/deterministic behavior)
    pub fn with_key(capacity: u64, key: SecretKey) -> Self {
        OArray {
            oram: PathOram::new(capacity, key),
        }
    }

    /// Read a value at the given index
    #[inline]
    pub fn read(&mut self, index: u64) -> T {
        self.oram.oram_read(index)
    }

    /// Write a value at the given index
    #[inline]
    pub fn write(&mut self, index: u64, value: T) {
        self.oram.oram_write(index, value);
    }

    /// Get the capacity
    pub fn len(&self) -> u64 {
        self.oram.capacity()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Convenience methods matching the OIR codegen expectations
impl<T: OramBlock> OArray<T> {
    /// ORAM read (matches codegen output)
    #[inline]
    pub fn oram_read(&mut self, index: u64) -> T {
        self.read(index)
    }

    /// ORAM write (matches codegen output)
    #[inline]
    pub fn oram_write(&mut self, index: u64, value: T) {
        self.write(index, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_oarray_basic() {
        let mut arr: OArray<u64> = OArray::new(100);
        arr.write(42, 12345);
        assert_eq!(arr.read(42), 12345);
    }

    #[test]
    fn test_oarray_multiple_writes() {
        let mut arr: OArray<u64> = OArray::new(100);
        for i in 0..10 {
            arr.write(i, i * 100);
        }
        for i in 0..10 {
            assert_eq!(arr.read(i), i * 100);
        }
    }
}
