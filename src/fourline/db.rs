use std::io::SeekFrom;

use arrayvec::ArrayVec;
use bytemuck::Zeroable;
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncSeekExt};

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
    pub placements: [Placement; 10],
}

impl FourLineDb {
    pub async fn open(b2b: bool) -> Self {
        let (index, data) = match b2b {
            false => ("4ldb-index.dat", "4ldb-data.dat"),
            true => ("4ldb-b2b-index.dat", "4ldb-b2b-data.dat"),
        };
        FourLineDb {
            index: File::open(index).await.unwrap(),
            data: File::open(data).await.unwrap(),
        }
    }

    pub async fn query(&mut self, queue: [Piece; 10]) -> Vec<DbEntry> {
        let index = compute_index(queue);

        let mut entry = LargeDbEntry::zeroed();
        self.index
            .seek(SeekFrom::Start(16 * index as u64))
            .await
            .unwrap();
        self.index.read_exact(bytemuck::bytes_of_mut(&mut entry)).await.unwrap();

        match entry.len {
            0 => vec![],
            1 => vec![convert(&queue, bytemuck::cast::<_, SmallDbEntry>(entry).entry)],
            _ => {
                let mut entries = vec![Entry::zeroed(); entry.len as usize];
                self.data.seek(SeekFrom::Start(entry.offset)).await.unwrap();
                self.data.read_exact(bytemuck::cast_slice_mut(&mut entries)).await.unwrap();
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
