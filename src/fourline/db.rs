use std::convert::TryInto;
use std::io::Read;

use arrayvec::ArrayVec;
use bytemuck::Zeroable;

use crate::data::{Piece, Placement};

use super::{compute_index, DataEntry, IndexEntry};

pub struct FourLineDb {
    index: Vec<IndexEntry>,
    data: Vec<DataEntry>,
}

pub struct FourLinePlacementsDb {
    data: Vec<[u8; 10]>,
}

#[derive(Clone, Copy, Debug)]
pub struct DbEntry {
    pub time: u32,
    pub score: u32,
    pub end_b2b: bool,
    pub valid_nob2b: bool,
    pub valid_b2b: bool,
    pub index: u32,
}

impl FourLineDb {
    pub fn load() -> Self {
        let mut index_file =
            std::io::BufReader::new(std::fs::File::open("4ldb-index.dat").unwrap());
        let mut index = vec![];
        let mut buffer = IndexEntry::zeroed();
        while index_file
            .read_exact(bytemuck::bytes_of_mut(&mut buffer))
            .is_ok()
        {
            index.push(buffer);
        }

        let mut data_file = std::io::BufReader::new(std::fs::File::open("4ldb-data.dat").unwrap());
        let mut data = vec![];
        let mut buffer = DataEntry::zeroed();
        while data_file
            .read_exact(bytemuck::bytes_of_mut(&mut buffer))
            .is_ok()
        {
            data.push(buffer);
        }

        FourLineDb { index, data }
    }

    pub fn query(&self, queue: [Piece; 10]) -> Vec<DbEntry> {
        let index = compute_index(queue);

        let entry = self.index[index];
        self.data
            .iter()
            .enumerate()
            .skip(entry.index as usize)
            .take(entry.len as usize)
            .map(convert)
            .collect()
    }
}

fn convert((index, raw_entry): (usize, &DataEntry)) -> DbEntry {
    DbEntry {
        time: raw_entry.time() as u32,
        score: raw_entry.score as u32,
        end_b2b: raw_entry.b2b(),
        valid_b2b: raw_entry.valid_b2b(),
        valid_nob2b: raw_entry.valid_nob2b(),
        index: index.try_into().unwrap(),
    }
}

impl FourLinePlacementsDb {
    pub fn load() -> Self {
        FourLinePlacementsDb {
            data: bytemuck::allocation::cast_vec(std::fs::read("4ldb-placements.dat").unwrap()),
        }
    }

    pub fn get(&self, index: u32, queue: [Piece; 10]) -> [Placement; 10] {
        self.data[index as usize]
            .iter()
            .zip(queue.iter())
            .map(|(&packed, &piece)| Placement::unpack(packed, piece))
            .collect::<ArrayVec<_, 10>>()
            .into_inner()
            .unwrap_or_else(|_| unreachable!())
    }
}
