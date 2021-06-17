use std::collections::BinaryHeap;
use std::io::Read;
use std::iter::Peekable;

use crate::data::*;

pub struct MergedBatches {
    src: Peekable<Merger>,
}

impl MergedBatches {
    pub fn new() -> Self {
        MergedBatches {
            src: Merger::new().peekable(),
        }
    }
}

impl Iterator for MergedBatches {
    type Item = ([Piece; 15], Vec<[Placement; 15]>);

    fn next(&mut self) -> Option<Self::Item> {
        let (key, mut value) = self.src.next()?;
        while let Some((_, mut v)) = self.src.next_if(|&(k, _)| k == key) {
            value.append(&mut v);
        }
        Some((key, value))
    }
}

struct Merger {
    batches: Vec<Peekable<BatchStream>>,
    priority_queue: BinaryHeap<BatchPriority>,
}

impl Merger {
    fn new() -> Self {
        let mut batches = vec![];
        for f in std::fs::read_dir("batches").unwrap() {
            let f = f.unwrap();
            if f.file_name().to_str().unwrap().ends_with(".dat") {
                batches.push(
                    BatchStream {
                        reader: zstd::Decoder::new(std::fs::File::open(f.path()).unwrap()).unwrap(),
                    }
                    .peekable(),
                );
            }
        }
        if batches.len() != 3264 {
            eprintln!(
                "Warning: only {} of 3264 batches are present",
                batches.len()
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
    type Item = ([Piece; 15], Vec<[Placement; 15]>);

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
    type Item = ([Piece; 15], Vec<[Placement; 15]>);

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
    value: [Piece; 15],
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
