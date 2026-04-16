use enumset::EnumSet;

use crate::data::Piece;

#[derive(Clone)]
pub struct BagStates {
    states: Vec<BagState>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct BagState {
    bag: EnumSet<Piece>,
    extra: EnumSet<Piece>,
}

impl BagStates {
    pub fn new() -> Self {
        let mut states: Vec<_> = BagState::all().collect();
        states.sort_unstable();
        BagStates { states }
    }

    pub fn remove(&self, p: Piece) -> Option<Self> {
        let mut states: Vec<_> = self
            .states
            .iter()
            .filter(|state| state.bag.contains(p))
            .map(|state| state.remove(p))
            .collect();
        if states.is_empty() {
            return None;
        }
        states.sort_unstable();
        states.dedup();
        Some(BagStates { states })
    }
}


impl BagState {
    fn all() -> impl Iterator<Item = Self> {
        (1..128).flat_map(|i| {
            let bag = EnumSet::from_usize(i);
            bag.iter()
                .map(move |p| BagState {
                    bag,
                    extra: EnumSet::only(p),
                })
                .chain((bag.len() > 1).then_some(BagState {
                    bag,
                    extra: EnumSet::empty(),
                }))
        })
    }

    fn remove(self, piece: Piece) -> Self {
        let mut this = self;
        if this.extra.contains(piece) {
            if this.bag.len() == 1 {
                this.extra = this.bag;
                this.bag = EnumSet::all();
            } else {
                this.extra = EnumSet::empty();
            }
        } else {
            this.bag.remove(piece);
            if this.extra.is_empty() && this.bag.len() == 1 {
                this.extra = this.bag;
                this.bag = EnumSet::all();
            }
        }
        this
    }
}
