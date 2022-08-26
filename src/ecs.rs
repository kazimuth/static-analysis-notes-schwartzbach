use std::ops::Index;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};

pub struct Storage<T> {
    nodes: Vec<T>,
}
impl<T> Storage<T> {
    pub fn node(&self, id: usize) -> Node<T> {
        assert!(id < self.nodes.len());
        Node {
            id: id as u32,
            _phantom: PhantomData,
        }
    }
    pub fn iter(&self) -> StorageIter<T> {
        StorageIter {
            current: 0,
            storage: self,
        }
    }
    pub fn insert(&mut self, item: T) -> Node<T> {
        self.nodes.push(item);
        self.node(self.nodes.len() - 1)
    }
}
impl<T> Index<Node<T>> for Storage<T> {
    type Output = T;

    fn index(&self, index: Node<T>) -> &T {
        &self.nodes[index.id() as usize]
    }
}
pub struct StorageIter<'a, T> {
    current: usize,
    storage: &'a Storage<T>,
}
impl<'a, T> Iterator for StorageIter<'a, T> {
    type Item = Node<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.storage.nodes.len() {
            None
        } else {
            let result = Some(self.storage.node(self.current));
            self.current += 1;
            result
        }
    }
}
impl<'a, T> IntoIterator for &'a Storage<T> {
    type Item = Node<T>;

    type IntoIter = StorageIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct Node<T> {
    id: u32,
    _phantom: PhantomData<T>,
}
impl<T> Node<T> {
    pub fn id(&self) -> u32 {
        self.id
    }
}
impl<T> Debug for Node<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.id))
    }
}
impl<T> Clone for Node<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            _phantom: PhantomData,
        }
    }
}
impl<T> Copy for Node<T> {}
impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> Eq for Node<T> {}
impl<T> Hash for Node<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
