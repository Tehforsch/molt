use std::marker::PhantomData;

pub trait StorageIndex {
    fn to_index(self) -> usize;
    fn from_index(index: usize) -> Self;
}

pub struct Storage<I: StorageIndex, T> {
    items: Vec<T>,
    _marker: PhantomData<I>,
}

impl<I: StorageIndex, T> Default for Storage<I, T> {
    fn default() -> Self {
        Self {
            items: vec![],
            _marker: PhantomData,
        }
    }
}

impl<I: StorageIndex, T> Storage<I, T> {
    pub(crate) fn add(&mut self, t: T) -> I {
        self.items.push(t);
        I::from_index(self.items.len() - 1)
    }

    pub(crate) fn into_iter_enumerate(self) -> impl Iterator<Item = (I, T)> {
        self.items
            .into_iter()
            .enumerate()
            .map(|(i, t)| (I::from_index(i), t))
    }
}

impl<I: StorageIndex, T> std::ops::Index<I> for Storage<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.items[index.to_index()]
    }
}
impl<I: StorageIndex, T> FromIterator<T> for Storage<I, T> {
    fn from_iter<It: IntoIterator<Item = T>>(iter: It) -> Self {
        Self {
            items: iter.into_iter().collect(),
            _marker: PhantomData,
        }
    }
}
