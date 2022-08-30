//! Exercises for: Lecture Notes on Static Analysis, schwartzbach
//! https://people.cs.vt.edu/~gback/cs6304.spring07/papers/static.pdf
//!
//! General problem: when ECSifying IRs, rust's pattern matching and debugging
//! facilities start being fairly useless. Oh well.

#![allow(unused, unstable_name_collisions)]

pub mod ecs;

pub mod prettier;

pub mod tip;

pub mod collections;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
