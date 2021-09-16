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
        if self.list.iter().any(|e| e.covers(&v)) {
            return;
        }

        self.list.retain(|e| !v.covers(e));
        self.list.push(v);
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
}
