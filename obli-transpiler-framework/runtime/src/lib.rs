// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Oblibeny Runtime Library
//!
//! This crate provides the runtime support for Oblibeny compiled programs,
//! including:
//!
//! - **Constant-time primitives**: `cmov`, `cswap`, and other operations
//!   that don't leak information through timing
//! - **ORAM implementations**: Path ORAM for oblivious memory access
//! - **Oblivious collections**: Maps, vectors, stacks with hidden access patterns
//! - **Cryptographic utilities**: Encryption, hashing, key derivation

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod constant_time;
pub mod oram;
pub mod crypto;
pub mod collections;

/// Prelude module for common imports
pub mod prelude {
    pub use crate::constant_time::*;
    pub use crate::oram::{OArray, PathOram, OramAccess};
    pub use crate::collections::*;
}
