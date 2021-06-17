use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::io::{Seek, Write};

use arrayvec::ArrayVec;
use dashmap::DashMap;
use enumset::EnumSet;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use structopt::StructOpt;

mod data;
mod merge;
mod search;

use crate::data::*;

#[derive(StructOpt)]
enum Options {
    GenBatches,
    ProcessBatches,
    CountStats,
    BuildDatabase,
}

fn main() {
    match Options::from_args() {
        Options::GenBatches => gen_batches(),
        Options::ProcessBatches => process_batches(),
        Options::CountStats => count_stats(),
        Options::BuildDatabase => build_db(),
    }
}

fn gen_batches() {
    std::fs::create_dir("batches").unwrap();

    let bags: Vec<_> = Piece::ALL
        .iter()
        .flat_map(|&hold| {
            (1..128).map(move |b| BagState {
                hold,
                bag: EnumSet::from_usize(b),
            })
        })
        .collect();

    let states = DashMap::new();

    let t = std::time::Instant::now();
    rayon::scope(|scope| {
        search::search_intermediates(scope, 1, &mut vec![], &Board([0; 10]), &bags, &states);
    });
    println!("{:?}", t.elapsed());

    println!("{}", states.len());

    let mut iter = states.into_iter();
    for i in 0.. {
        let batch: Vec<_> = (&mut iter).take(1000).collect();
        if batch.is_empty() {
            break;
        }
        bincode::serialize_into(
            std::io::BufWriter::new(std::fs::File::create(format!("batches/{}.batch", i)).unwrap()),
            &batch,
        )
        .unwrap();
    }
}

fn process_batches() {
    let targets: Vec<_> = std::fs::read_dir("batches")
        .unwrap()
        .filter_map(|f| {
            let name = f.unwrap().file_name();
            let name = name.to_str().unwrap();
            let base = name.trim_end_matches(".batch");
            if base == name {
                None
            } else {
                Some(base.to_owned())
            }
        })
        .collect();

    for i in targets {
        let output = format!("batches/{}.dat", i);
        let input = format!("batches/{}.batch", i);

        if std::fs::metadata(&output).is_ok() {
            continue;
        }

        let data: Vec<(Board, HashSet<BagState>, Vec<Vec<Placement>>)> =
            match std::fs::File::open(&input) {
                Ok(f) => bincode::deserialize_from(std::io::BufReader::new(f)).unwrap(),
                Err(_) => continue,
            };

        println!("Processing Batch {}", i);

        let pcs = DashMap::new();

        let t = std::time::Instant::now();
        data.into_par_iter().for_each(|(board, bags, partials)| {
            let bags: Vec<_> = bags.into_iter().collect();
            rayon::scope(|scope| {
                search::search_complete(scope, 2, &mut vec![], &board, &bags, &pcs, &partials)
            })
        });
        println!("{:?}", t.elapsed());
        println!(
            "{} seqs, {} pcs",
            pcs.len(),
            pcs.iter().map(|v| v.len()).sum::<usize>()
        );

        let mut pcs: Vec<_> = pcs.into_iter().collect();
        pcs.sort_by_key(|&(seq, _)| seq);

        let t = std::time::Instant::now();
        let mut target =
            zstd::Encoder::new(std::fs::File::create("batches/.tmp").unwrap(), 9).unwrap();
        target.multithread(16).unwrap();
        for v in pcs {
            let buf = bincode::serialize(&v).unwrap();
            target.write_all(&(buf.len() as u64).to_le_bytes()).unwrap();
            target.write_all(&buf).unwrap();
        }

        target.finish().unwrap();
        println!("{:?}", t.elapsed());

        std::fs::rename("batches/.tmp", &output).unwrap();
    }
}

fn count_stats() {
    let mut pcs_count = 0;
    let mut seq_count = 0u64;

    let mut prev_seq_part = [Piece::I; 3];
    let mut partial_seq_count = 0;

    let t = std::time::Instant::now();
    for (seq, pcs) in merge::MergedBatches::new() {
        seq_count += 1;
        pcs_count += pcs.len();
        if seq[..3] != prev_seq_part {
            if partial_seq_count != 0 {
                println!("{:?}: {} seqs", prev_seq_part, partial_seq_count);
            }
            partial_seq_count = 0;
            prev_seq_part = [seq[0], seq[1], seq[2]];
        }
        partial_seq_count += 1;
    }
    println!("{:?}: {}", prev_seq_part, partial_seq_count);

    println!("{:?}", t.elapsed());

    println!("{} sequences, {} pcs", seq_count, pcs_count);
}

fn build_db() {
    // All integers are native-endian.
    //
    // tsd-tet-pc-index.dat starts with an array of 7^7 `(index_offset: u64, seqs: u64)`.
    // `index_offset`: offset in bytes into tsd-tst-pc-index.dat of the `seq array`
    // `seqs`: length in number of entries of the `seq array`
    // `seq array`: array of `(packed sequence: u32, data_length: u32, data_offset: u64)`
    // `packed sequence`: 8 nibbles, with the i-th piece type being stored in the i-th most
    //                    significant nibble. This allows simple u32 comparisons to binary search
    //                    correctly.
    // `data_offset`: offset in bytes into tsd-tet-pc-data.dat of the `pcs array`
    // `data_length`: length in number of entries of the `pcs array`
    // `pcs array`: array of arrays of 15 `packed placement`s.
    // `packed placement`: 1 byte. This is (x + y * 10) * 4 + rotation. The piece type is inferred
    //                     from its position in the piece sequence.
    // Piece type values:
    // I = 0
    // O = 1
    // T = 2
    // L = 3
    // J = 4
    // S = 5
    // Z = 6

    let t = std::time::Instant::now();

    let mut index = std::io::BufWriter::new(std::fs::File::create("tsd-tet-pc-index.dat").unwrap());
    let mut data = std::io::BufWriter::new(std::fs::File::create("tsd-tet-pc-data.dat").unwrap());

    let mut seven_piece_index = Box::new([Location::default(); 7 * 7 * 7 * 7 * 7 * 7 * 7]);
    let seven_piece_index_size = bytemuck::cast_slice::<_, u8>(&*seven_piece_index).len() as u64;

    // Will be replaced with the offset of the seven_piece_index table.
    index
        .seek(std::io::SeekFrom::Start(seven_piece_index_size))
        .unwrap();

    let mut index_offset = seven_piece_index_size;
    let mut index_len = 0;
    let mut data_offset = 0u64;

    let mut current_partial = [Piece::I; 7];

    for (seq, pcs) in merge::MergedBatches::new() {
        let (partial, rest) = seq.split_at(7);
        let partial: [_; 7] = partial.try_into().unwrap();
        let rest: [_; 8] = rest.try_into().unwrap();

        if partial != current_partial {
            let index = idx(&current_partial);
            seven_piece_index[index] = Location {
                offset: index_offset,
                length: index_len,
            };
            index_offset += index_len * 16;
            index_len = 0;
            current_partial = partial;
            let new_index = idx(&partial);
            if (index as f64 / 7.0f64.powi(7) * 1000.0).floor()
                != (new_index as f64 / 7.0f64.powi(7) * 1000.0).floor()
            {
                println!("{:.1}%", new_index as f64 / 7.0f64.powi(7) * 100.0);
            }
        }

        index_len += 1;
        index
            .write_all(bytemuck::bytes_of(&IndexEntry {
                sequence: packed_sequence(rest),
                length: u32::try_from(pcs.len()).unwrap(),
                offset: data_offset,
            }))
            .unwrap();

        for pc in pcs {
            for &mv in &pc {
                data.write_all(&[packed_placement(mv)]).unwrap();
                data_offset += 1;
            }
        }
    }

    seven_piece_index[idx(&current_partial)] = Location {
        offset: index_offset,
        length: index_len,
    };

    index.seek(std::io::SeekFrom::Start(0)).unwrap();
    index
        .write_all(bytemuck::cast_slice(&*seven_piece_index))
        .unwrap();

    println!("{:?}", t.elapsed());
}

#[derive(Default, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
struct Location {
    offset: u64,
    length: u64,
}

#[derive(Default, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
struct IndexEntry {
    sequence: u32,
    length: u32,
    offset: u64,
}

fn idx(partial: &[Piece]) -> usize {
    partial.iter().fold(0, |acc, &p| acc * 7 + p as usize)
}

fn packed_sequence(seq: [Piece; 8]) -> u32 {
    seq.iter().fold(0, |acc, &p| acc << 4 | p as u32)
}

fn packed_placement(mv: Placement) -> u8 {
    (mv.x as u8 + mv.y as u8 * 10) << 2 | mv.rotation as u8
}

// stable polyfill for [T; N]::map
trait ArrayExt<A, B> {
    type Map;
    fn amap(self, f: impl FnMut(A) -> B) -> Self::Map;
}

impl<A, B, const N: usize> ArrayExt<A, B> for [A; N] {
    type Map = [B; N];
    fn amap(self, f: impl FnMut(A) -> B) -> [B; N] {
        std::array::IntoIter::new(self)
            .map(f)
            .collect::<ArrayVec<_, N>>()
            .into_inner()
            .unwrap_or_else(|_| unreachable!())
    }
}
