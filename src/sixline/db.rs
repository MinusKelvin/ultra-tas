use std::convert::TryInto;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};

use arrayvec::ArrayVec;

use crate::data::{Piece, Placement};
use crate::sixline::{idx, packed_sequence, IndexEntry};

pub struct SixLineDb {
    index: File,
    data: File,
}

impl SixLineDb {
    pub fn open() -> Self {
        SixLineDb {
            index: File::open("tsd-tet-pc-index.dat").unwrap(),
            data: File::open("tsd-tet-pc-data.dat").unwrap(),
        }
    }

    pub fn query(&mut self, queue: [Piece; 15]) -> Vec<[Placement; 15]> {
        let (start, end) = queue.split_at(7);
        let end = end.try_into().unwrap_or_else(|_| unreachable!());
        let index = idx(start);
        let key = packed_sequence(end);

        self.index
            .seek(SeekFrom::Start(16 * index as u64))
            .unwrap();
        let mut offset = 0u64;
        self.index.read_exact(bytemuck::bytes_of_mut(&mut offset)).unwrap();
        let mut count = 0u64;
        self.index.read_exact(bytemuck::bytes_of_mut(&mut count)).unwrap();
        println!("{}", count);
        if count == 0 {
            return vec![];
        }

        self.index.seek(SeekFrom::Start(offset)).unwrap();
        let mut buffer = vec![IndexEntry::default(); count as usize];
        self.index
            .read_exact(bytemuck::cast_slice_mut(&mut buffer))
            .unwrap();

        println!("{:08X}", buffer[0].sequence);

        match buffer.binary_search_by(|a| a.sequence.cmp(&key)) {
            Ok(idx) => {
                let entry = buffer[idx];
                self.data.seek(SeekFrom::Start(entry.offset)).unwrap();
                let mut result = vec![[0u8; 15]; entry.length as usize];
                self.data
                    .read_exact(bytemuck::cast_slice_mut(&mut result))
                    .unwrap();

                result
                    .into_iter()
                    .map(|entry| {
                        entry
                            .iter()
                            .enumerate()
                            .map(|(i, &p)| Placement::unpack(p, queue[i]))
                            .collect::<ArrayVec<_, 15>>()
                            .into_inner()
                            .unwrap()
                    })
                    .collect()
            }
            Err(_) => vec![],
        }
    }
}
