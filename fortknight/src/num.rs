use num_bigint::BigUint;

/// An abstraction that uses a 32-bit integer for "small" integers, but uses a BigUint when it's
/// even bigger. This is to avoid additional heap allocations
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Uint<'a> {
    Small(u32),
    Big(&'a BigUint),
}
