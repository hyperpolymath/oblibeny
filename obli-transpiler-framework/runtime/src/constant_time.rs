// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Constant-time primitives for side-channel resistance
//!
//! These primitives ensure that execution time does not depend on
//! secret values, preventing timing attacks.

use subtle::{Choice, ConditionallySelectable, ConstantTimeEq, ConstantTimeLess};

/// Constant-time conditional move
///
/// Returns `a` if `cond` is true, `b` otherwise.
/// The selection is done in constant time.
#[inline]
pub fn cmov<T: ConditionallySelectable>(cond: bool, a: T, b: T) -> T {
    T::conditional_select(&b, &a, Choice::from(cond as u8))
}

/// Constant-time conditional swap
///
/// Swaps `a` and `b` if `cond` is true, otherwise leaves them unchanged.
/// The swap is done in constant time.
#[inline]
pub fn cswap<T: ConditionallySelectable>(cond: bool, a: &mut T, b: &mut T) {
    T::conditional_swap(a, b, Choice::from(cond as u8));
}

/// Constant-time equality comparison
///
/// Returns true if `a == b` in constant time.
#[inline]
pub fn ct_eq<T: ConstantTimeEq>(a: &T, b: &T) -> bool {
    a.ct_eq(b).into()
}

/// Constant-time less-than comparison
///
/// Returns true if `a < b` in constant time.
#[inline]
pub fn ct_lt<T: ConstantTimeLess>(a: &T, b: &T) -> bool {
    a.ct_lt(b).into()
}

/// Constant-time array lookup
///
/// Returns the element at `index` from `array` in constant time.
/// All elements are accessed regardless of the index value.
#[inline]
pub fn ct_lookup<T: ConditionallySelectable + Default + Clone>(array: &[T], index: usize) -> T {
    let mut result = T::default();
    for (i, elem) in array.iter().enumerate() {
        let select = Choice::from((i == index) as u8);
        result.conditional_assign(elem, select);
    }
    result
}

/// Constant-time array store
///
/// Stores `value` at `index` in `array` in constant time.
/// All elements are potentially modified regardless of the index value.
#[inline]
pub fn ct_store<T: ConditionallySelectable + Clone>(array: &mut [T], index: usize, value: &T) {
    for (i, elem) in array.iter_mut().enumerate() {
        let select = Choice::from((i == index) as u8);
        elem.conditional_assign(value, select);
    }
}

/// Constant-time minimum
#[inline]
pub fn ct_min<T: ConditionallySelectable + ConstantTimeLess + Clone>(a: T, b: T) -> T {
    cmov(a.ct_lt(&b).into(), a, b)
}

/// Constant-time maximum
#[inline]
pub fn ct_max<T: ConditionallySelectable + ConstantTimeLess + Clone>(a: T, b: T) -> T {
    cmov(b.ct_lt(&a).into(), a, b)
}

/// Constant-time absolute value for signed integers
#[inline]
pub fn ct_abs_i64(x: i64) -> i64 {
    let mask = x >> 63;
    (x ^ mask) - mask
}

/// Constant-time sign extraction
#[inline]
pub fn ct_sign_i64(x: i64) -> i64 {
    let positive = !(x >> 63) & 1;
    let negative = (x >> 63) & 1;
    positive - negative
}

/// Convert a bool to Choice in constant time
#[inline]
pub fn bool_to_choice(b: bool) -> Choice {
    Choice::from(b as u8)
}

/// Convert a Choice to bool
#[inline]
pub fn choice_to_bool(c: Choice) -> bool {
    c.into()
}

/// Constant-time byte array equality
pub fn ct_bytes_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut result = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        result |= x ^ y;
    }
    result == 0
}

/// Constant-time byte array copy based on condition
pub fn ct_copy_if(cond: bool, dst: &mut [u8], src: &[u8]) {
    assert_eq!(dst.len(), src.len());
    let mask = if cond { 0xFF } else { 0x00 };
    for (d, s) in dst.iter_mut().zip(src.iter()) {
        *d = (*d & !mask) | (*s & mask);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cmov() {
        assert_eq!(cmov(true, 42u64, 0u64), 42u64);
        assert_eq!(cmov(false, 42u64, 0u64), 0u64);
    }

    #[test]
    fn test_cswap() {
        let mut a = 1u64;
        let mut b = 2u64;
        cswap(true, &mut a, &mut b);
        assert_eq!(a, 2);
        assert_eq!(b, 1);

        cswap(false, &mut a, &mut b);
        assert_eq!(a, 2);
        assert_eq!(b, 1);
    }

    #[test]
    fn test_ct_lookup() {
        let array = [10u64, 20, 30, 40, 50];
        assert_eq!(ct_lookup(&array, 0), 10);
        assert_eq!(ct_lookup(&array, 2), 30);
        assert_eq!(ct_lookup(&array, 4), 50);
    }

    #[test]
    fn test_ct_store() {
        let mut array = [10u64, 20, 30, 40, 50];
        ct_store(&mut array, 2, &99);
        assert_eq!(array, [10, 20, 99, 40, 50]);
    }

    #[test]
    fn test_ct_bytes_eq() {
        assert!(ct_bytes_eq(b"hello", b"hello"));
        assert!(!ct_bytes_eq(b"hello", b"world"));
        assert!(!ct_bytes_eq(b"hello", b"hell"));
    }
}
