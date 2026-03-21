# aLib Implementation in Oblíbený

This directory contains Oblíbený implementations of the aggregate-library (aLib) Common Library operations.

## Purpose

These implementations demonstrate:
- How aLib specifications map to Oblíbený's constrained form
- Preservation of behavioral semantics under Turing-incompleteness
- Reversibility guarantees for all operations
- Accountability tracing for operation execution

## Structure

Each aLib category has a corresponding `.obl` file:
- `arithmetic.obl` - add, subtract, multiply, divide, modulo
- `collection.obl` - map, filter, fold, contains
- `comparison.obl` - equal, not_equal, less_than, etc.
- `conditional.obl` - if_then_else
- `logical.obl` - and, or, not
- `string.obl` - concat, length, substring

## Conformance

All implementations pass the aLib conformance test suite (see `../../test/alib_conformance.ml`).

## Notes

- **Termination**: All operations terminate (guaranteed by Oblíbený's constrained form)
- **Reversibility**: Arithmetic operations use reversible primitives where applicable
- **Traces**: All operations produce accountability traces
- **Types**: Uses Oblíbený's type system (i64 for Number, custom types for Collection/String)
