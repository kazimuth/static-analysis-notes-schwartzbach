//! a little datalog-like thing for storing little databases, indexed by everything
//!
//! basic principles:
//! - all structures named, no position-based things except vectors
//! - keys are struct types, which allows both compile-time lookup and run-time lookup with TypeID
//! - avoid macros where possible
