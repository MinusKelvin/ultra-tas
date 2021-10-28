use arrayvec::ArrayVec;
use once_cell::sync::Lazy;

use crate::data::{Piece, Placement};
use crate::fourline::LargeDbEntry;

use super::{compute_index, Entry, SmallDbEntry};

static DATABASE: Lazy<(Vec<u8>, Vec<u8>)> = Lazy::new(|| {
    let index = std::fs::read("4ldb-index.dat").unwrap();
    let data = std::fs::read("4ldb-data.dat").unwrap();
    (index, data)
});

pub struct FourLineDb {
    _priv: (),
}

#[derive(Clone, Copy, Debug)]
pub struct DbEntry {
    pub time: u32,
    pub score: u32,
    pub end_b2b: bool,
    pub valid_nob2b: bool,
    pub valid_b2b: bool,
    pub placements: [Placement; 10],
}

impl FourLineDb {
    pub fn open() -> Self {
        Lazy::force(&DATABASE);
        FourLineDb { _priv: () }
    }

    pub fn query(&mut self, queue: [Piece; 10]) -> Vec<DbEntry> {
        let index = compute_index(queue);

        let entry: &LargeDbEntry =
            bytemuck::from_bytes(&DATABASE.0[16 * index as usize..16 * (index + 1) as usize]);

        match entry.len {
            0 => vec![],
            1 => vec![convert(
                &queue,
                bytemuck::cast_ref::<_, SmallDbEntry>(entry).entry,
            )],
            _ => {
                let entries = bytemuck::cast_slice(
                    &DATABASE.1[entry.offset as usize
                        ..entry.offset as usize
                            + entry.len as usize * std::mem::size_of::<Entry>()],
                );
                entries.iter().map(|&e| convert(&queue, e)).collect()
            }
        }
    }
}

fn convert(queue: &[Piece; 10], raw_entry: Entry) -> DbEntry {
    DbEntry {
        time: raw_entry.time() as u32,
        score: raw_entry.score as u32,
        end_b2b: raw_entry.b2b(),
        valid_b2b: raw_entry.valid_b2b(),
        valid_nob2b: raw_entry.valid_nob2b(),
        placements: raw_entry
            .placements
            .iter()
            .zip(queue.iter())
            .map(|(&packed, &piece)| Placement::unpack(packed, piece))
            .collect::<ArrayVec<_, 10>>()
            .into_inner()
            .unwrap(),
    }
}
