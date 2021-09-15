#[derive(Clone, Debug)]
pub struct Archive<T> {
    /// sorted by T::get_min_better_dimension()
    list: Vec<T>,
}

impl<T: Dominance> Archive<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add(&mut self, v: T) {
        let possible_dominators_start = self
            .list
            .partition_point(|e| e.get_ascending_dim() < v.get_ascending_dim());

        if self.list.iter().skip(possible_dominators_start).any(|e| e.covers(&v)) {
            return;
        }

        self.list.retain(|e| !v.covers(e));
        self.list.push(v);
        for i in (1..self.list.len()).rev() {
            if self.list[i].get_ascending_dim() < self.list[i-1].get_ascending_dim() {
                self.list.swap(i, i-1);
            }
        }
    }
}

impl<T> From<Archive<T>> for Vec<T> {
    fn from(a: Archive<T>) -> Self {
        a.list
    }
}

impl<T> std::ops::Deref for Archive<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Vec<T> {
        &self.list
    }
}

impl<T> Default for Archive<T> {
    fn default() -> Self {
        Archive { list: vec![] }
    }
}

impl<T> IntoIterator for Archive<T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.list.into_iter()
    }
}

pub trait Dominance {
    /// non-strict dominance.
    fn covers(&self, other: &Self) -> bool;

    type Dim: Ord;
    fn get_ascending_dim(&self) -> Self::Dim;
}
