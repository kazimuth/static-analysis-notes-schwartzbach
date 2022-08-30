use std::{hash::Hash, ops::Index};

use ahash::HashMap;

/// A bidirectional hash table, aka, something isomorphic to a bipartial isomorphism (A ⊇ . ≅ . ⊆ B)
#[derive(Clone)]
pub struct Bidirectional<A: Eq + Hash + Clone, B: Eq + Hash + Clone> {
    a_to_b: HashMap<A, B>,
    b_to_a: HashMap<B, A>,
}

impl<A: Eq + Hash + Clone, B: Eq + Hash + Clone> Default for Bidirectional<A, B> {
    fn default() -> Self {
        Bidirectional {
            a_to_b: HashMap::default(),
            b_to_a: HashMap::default(),
        }
    }
}

impl<A: Eq + Hash + Clone, B: Eq + Hash + Clone> Bidirectional<A, B> {
    pub fn insert(&mut self, a: A, b: B) {
        assert!(!self.a_to_b.contains_key(&a));
        assert!(!self.b_to_a.contains_key(&b));
        self.a_to_b.insert(a.clone(), b.clone());
        self.b_to_a.insert(b, a);
    }

    pub fn get_left(&self, a: &A) -> Option<&B> {
        self.a_to_b.get(a)
    }

    pub fn get_right(&self, b: &B) -> Option<&A> {
        self.b_to_a.get(b)
    }

    // sadly we can't override index because there's no way to require A ≠ B
    pub fn left(&self, a: &A) -> &B {
        &self.a_to_b[a]
    }

    pub fn right(&self, b: &B) -> &A {
        &self.b_to_a[b]
    }

    pub fn iter(&self) -> impl Iterator<Item = (&A, &B)> {
        self.a_to_b.iter()
    }
}
