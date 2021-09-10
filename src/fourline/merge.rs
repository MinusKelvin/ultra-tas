use std::collections::BinaryHeap;
use std::io::Read;
use std::iter::Peekable;

use smallvec::SmallVec;

use crate::archive::Archive;
use crate::data::*;

use super::Entry;

pub(super) struct MergedBatches {
    src: Peekable<Merger>,
}

impl MergedBatches {
    pub fn new(b2b: bool) -> Self {
        MergedBatches {
            src: Merger::new(b2b).peekable(),
        }
    }
}

impl Iterator for MergedBatches {
    type Item = ([Piece; 10], Vec<Entry>);

    fn next(&mut self) -> Option<Self::Item> {
        let key = self.src.peek()?.0;
        let mut archive = Archive::new();
        while let Some((_, v)) = self.src.next_if(|&(k, _)| k == key) {
            for entry in v {
                archive.add(entry);
            }
        }
        Some((key, archive.into()))
    }
}

struct Merger {
    batches: Vec<Peekable<BatchStream>>,
    priority_queue: BinaryHeap<BatchPriority>,
}

impl Merger {
    fn new(b2b: bool) -> Self {
        let mut batches = vec![];
        for i in 0..1024 {
            let path = match b2b {
                false => format!("4lbatches/{}.dat", i),
                true => format!("4lbatches/{}-b2b.dat", i),
            };
            batches.push(
                BatchStream {
                    reader: zstd::Decoder::new(std::fs::File::open(path).unwrap()).unwrap(),
                }
                .peekable(),
            );
        }

        let mut priority_queue = BinaryHeap::new();
        for (index, batch) in batches.iter_mut().enumerate() {
            if let Some(&(k, _)) = batch.peek() {
                priority_queue.push(BatchPriority { index, value: k });
            }
        }

        Merger {
            priority_queue,
            batches,
        }
    }
}

impl Iterator for Merger {
    type Item = ([Piece; 10], SmallVec<[Entry; 1]>);

    fn next(&mut self) -> Option<Self::Item> {
        let BatchPriority { index, .. } = self.priority_queue.pop()?;
        let result = self.batches[index].next()?;

        if let Some(&(k, _)) = self.batches[index].peek() {
            self.priority_queue.push(BatchPriority { index, value: k });
        }

        Some(result)
    }
}

struct BatchStream {
    reader: zstd::Decoder<'static, std::io::BufReader<std::fs::File>>,
}

impl Iterator for BatchStream {
    type Item = ([Piece; 10], SmallVec<[Entry; 1]>);

    fn next(&mut self) -> Option<Self::Item> {
        let mut len = [0; 8];
        self.reader.read_exact(&mut len).ok()?;
        let len = u64::from_le_bytes(len);

        let mut buf = vec![0; len as usize];
        self.reader.read_exact(&mut buf).unwrap();

        Some(bincode::deserialize(&buf).unwrap())
    }
}

struct BatchPriority {
    index: usize,
    value: [Piece; 10],
}

impl Ord for BatchPriority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value).reverse()
    }
}

impl PartialOrd for BatchPriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for BatchPriority {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for BatchPriority {}
