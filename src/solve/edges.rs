use arrayvec::ArrayVec;

use crate::archive::{Archive, Dominance};
use crate::data::{Piece, Placement};

pub struct Edge {
    pub score: u32,
    pub time: u32,
    pub b2b: bool,
    pub valid_b2b: bool,
    pub valid_nob2b: bool,
    pub hold: Option<Piece>,
    pub placements: ArrayVec<Placement, 15>,
}

pub struct Edges {
    nob2b: [Archive<InternalEdge>; 8],
    b2b: [Archive<InternalEdge>; 8],
}

struct InternalEdge {
    score: u32,
    time: u32,
    b2b: bool,
    placements: ArrayVec<Placement, 15>,
}

impl Dominance for InternalEdge {
    fn covers(&self, other: &Self) -> bool {
        self.score >= other.score && self.time <= other.time && self.b2b >= other.b2b
    }

    type Dim = u32;
    fn get_ascending_dim(&self) -> Self::Dim {
        self.score
    }
}

impl Edges {
    pub fn new() -> Self {
        Edges {
            nob2b: [
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
            ],
            b2b: [
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
                Archive::new(),
            ],
        }
    }

    pub fn add_edge(&mut self, edge: Edge) {
        if edge.valid_b2b {
            self.b2b[hold_to_index(edge.hold)].add(InternalEdge {
                score: edge.score,
                time: edge.time,
                b2b: edge.b2b,
                placements: edge.placements.clone(),
            });
        }
        if edge.valid_nob2b {
            self.nob2b[hold_to_index(edge.hold)].add(InternalEdge {
                score: edge.score,
                time: edge.time,
                b2b: edge.b2b,
                placements: edge.placements.clone(),
            });
        }
    }

    pub fn get_edges(&self, b2b: bool) -> impl Iterator<Item = Edge> + '_ {
        let set = if b2b { &self.b2b } else { &self.nob2b };
        set.iter().enumerate().flat_map(move |(i, a)| {
            a.iter().map(move |e| Edge {
                score: e.score,
                time: e.time,
                b2b: e.b2b,
                valid_b2b: b2b,
                valid_nob2b: !b2b,
                hold: index_to_hold(i),
                placements: e.placements.clone(),
            })
        })
    }
}

fn hold_to_index(h: Option<Piece>) -> usize {
    match h {
        Some(p) => p as usize,
        None => 7,
    }
}

fn index_to_hold(i: usize) -> Option<Piece> {
    Piece::ALL.get(i).copied()
}
