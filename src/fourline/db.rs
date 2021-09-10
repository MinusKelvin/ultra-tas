use std::fs::File;
use std::io::{Read, Seek, SeekFrom};

use arrayvec::ArrayVec;
use bytemuck::Zeroable;

use crate::data::{Piece, Placement};
use crate::fourline::LargeDbEntry;

use super::{compute_index, Entry, SmallDbEntry};

pub struct FourLineDb {
    index: File,
    data: File,
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
        FourLineDb {
            index: File::open("4ldb-index.dat").unwrap(),
            data: File::open("4ldb-data.dat").unwrap(),
        }
    }

    pub fn query(&mut self, queue: [Piece; 10]) -> Vec<DbEntry> {
        let index = compute_index(queue);

        let mut entry = LargeDbEntry::zeroed();
        self.index
            .seek(SeekFrom::Start(16 * index as u64))
            .unwrap();
        self.index.read_exact(bytemuck::bytes_of_mut(&mut entry)).unwrap();

        match entry.len {
            0 => vec![],
            1 => vec![convert(&queue, bytemuck::cast::<_, SmallDbEntry>(entry).entry)],
            _ => {
                let mut entries = vec![Entry::zeroed(); entry.len as usize];
                self.data.seek(SeekFrom::Start(entry.offset)).unwrap();
                self.data.read_exact(bytemuck::cast_slice_mut(&mut entries)).unwrap();
                entries.into_iter().map(|e| convert(&queue, e)).collect()
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
