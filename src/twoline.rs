use std::convert::TryInto;

use pcf::{BitBoard, PieceSet};

use crate::archive::{Archive, Dominance};
use crate::data::{Piece, Placement};
use crate::placement_search::find_placement_sequences;

pub struct TwoLineDb {
    db: Vec<Archive<Entry>>,
}

impl TwoLineDb {
    pub fn gen() -> Self {
        let mut db = vec![Archive::new(); 1024];

        #[rustfmt::skip]
        let pieces = PieceSet::default()
            .with(pcf::Piece::I).with(pcf::Piece::I).with(pcf::Piece::I).with(pcf::Piece::I)
            .with(pcf::Piece::O).with(pcf::Piece::O).with(pcf::Piece::O).with(pcf::Piece::O)
            .with(pcf::Piece::L).with(pcf::Piece::L).with(pcf::Piece::L).with(pcf::Piece::L)
            .with(pcf::Piece::J).with(pcf::Piece::J).with(pcf::Piece::J).with(pcf::Piece::J);

        pcf::find_combinations(pieces, BitBoard(0), &Default::default(), 2, |combo| {
            find_placement_sequences(
                &mut vec![],
                BitBoard(0),
                &mut combo.into(),
                &mut |placements, score, time, _| {
                    let placements: [_; 5] = placements.try_into().unwrap();
                    let index = compute_index(placements.map(|p| p.piece)).unwrap();
                    db[index].add(Entry {
                        score,
                        time,
                        placements,
                    });
                },
                0,
                0,
                false,
                0,
            );
        });

        TwoLineDb { db }
    }

    pub fn query(&self, pieces: [Piece; 5]) -> &[Entry] {
        let index = match compute_index(pieces) {
            Some(idx) => idx,
            None => return &[],
        };

        &self.db[index]
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Entry {
    pub score: u32,
    pub time: u32,
    pub placements: [Placement; 5],
}

impl Dominance for Entry {
    fn covers(&self, other: &Self) -> bool {
        self.score >= other.score && self.time <= other.time
    }

    type Dim = u32;
    fn get_ascending_dim(&self) -> Self::Dim {
        self.score
    }
}

fn piece_value(p: Piece) -> Option<usize> {
    Some(match p {
        Piece::I => 0,
        Piece::J => 1,
        Piece::L => 2,
        Piece::O => 3,
        _ => return None,
    })
}

fn compute_index(pieces: [Piece; 5]) -> Option<usize> {
    let mut index = 0;
    for p in pieces {
        index *= 4;
        index += piece_value(p)?;
    }
    Some(index)
}
