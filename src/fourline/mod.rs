use std::collections::BTreeMap;
use std::convert::TryInto;
use std::io::Write;
use std::sync::Mutex;

use arrayvec::ArrayVec;
use bytemuck::{Pod, Zeroable};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use structopt::StructOpt;

use crate::archive::{Archive, Dominance};
use crate::data::{Piece, Placement};
use crate::parse_seq;
use crate::placement_search::find_placement_sequences;

use self::merge::MergedBatches;

mod db;
mod merge;

pub use self::db::{FourLineDb, FourLinePlacementsDb};

#[derive(StructOpt)]
pub enum Options {
    GenBatches {
        #[structopt(default_value = "0")]
        start: usize,
        #[structopt(default_value = "1024")]
        end: usize,
    },
    BuildDb,
    Lookup {
        seq: String,
    },
}

impl Options {
    pub fn run(self) {
        match self {
            Options::GenBatches { start, end } => generate_batches(start, end),
            Options::BuildDb => build_db(),
            Options::Lookup { seq } => {
                let r = parse_seq(&seq).unwrap();
                if r.len() != 10 {
                    panic!("bad length");
                }
                let seq = r.try_into().unwrap();
                let t = std::time::Instant::now();
                let db = db::FourLineDb::load();
                let results = db.query(seq);
                println!("{:#?}", results);
                println!("{:?}", t.elapsed());
            }
        }
    }
}

fn generate_batches(start: usize, end: usize) {
    let piece_set = pcf::PIECES.repeat(4).into_iter().collect();

    let combos = Mutex::new(vec![]);

    let t = std::time::Instant::now();
    pcf::find_combinations_mt(
        piece_set,
        pcf::BitBoard(0),
        &Default::default(),
        4,
        |combo| {
            combos.lock().unwrap().push([
                PlacementOrd(combo[0]),
                PlacementOrd(combo[1]),
                PlacementOrd(combo[2]),
                PlacementOrd(combo[3]),
                PlacementOrd(combo[4]),
                PlacementOrd(combo[5]),
                PlacementOrd(combo[6]),
                PlacementOrd(combo[7]),
                PlacementOrd(combo[8]),
                PlacementOrd(combo[9]),
            ]);
        },
    );
    println!("Took {:.2?} to compute combos", t.elapsed());

    let mut combos = combos.into_inner().unwrap();
    let t = std::time::Instant::now();
    combos.sort();
    println!("Took {:.2?} to sort combos", t.elapsed());

    let mut subdivs = vec![&combos[..]];
    for _ in 0..10 {
        let mut new_subdivs = vec![];
        for div in subdivs {
            let (left, right) = div.split_at(div.len() / 2);
            new_subdivs.push(left);
            new_subdivs.push(right);
        }
        subdivs = new_subdivs;
    }

    std::fs::create_dir_all("4lbatches").unwrap();
    for (i, div) in subdivs
        .into_iter()
        .enumerate()
        .skip(start)
        .take(end - start)
    {
        if std::fs::metadata(format!("4lbatches/{}.dat", i)).is_ok() {
            println!("Skipping existing batch {}", i);
            continue;
        }

        let t = std::time::Instant::now();
        let normal_db = Mutex::new(BTreeMap::new());
        let b2b_db = Mutex::new(BTreeMap::new());
        div.par_iter().for_each(|combo| {
            find_placement_sequences(
                &mut vec![],
                pcf::BitBoard(0),
                &mut combo.iter().map(|p| p.0).collect(),
                &mut |order, score, time, b2b| add(&normal_db, order, score, time, b2b),
                0,
                0,
                false,
                0,
            );
            find_placement_sequences(
                &mut vec![],
                pcf::BitBoard(0),
                &mut combo.iter().map(|p| p.0).collect(),
                &mut |order, score, time, b2b| add(&b2b_db, order, score, time, b2b),
                0,
                0,
                true,
                0,
            );
        });
        println!("Batch {} took {:.2?}", i, t.elapsed());

        let t = std::time::Instant::now();
        let mut into =
            zstd::Encoder::new(std::fs::File::create("4lbatches/tmp.dat").unwrap(), 9).unwrap();
        into.multithread(16).unwrap();
        for (key, archive) in b2b_db.into_inner().unwrap() {
            let v = (key, Vec::from(archive));
            let buf = bincode::serialize(&v).unwrap();
            into.write_all(&(buf.len() as u64).to_le_bytes()).unwrap();
            into.write_all(&buf).unwrap();
        }
        into.finish().unwrap();
        std::fs::rename("4lbatches/tmp.dat", format!("4lbatches/{}-b2b.dat", i)).unwrap();
        println!("Saved b2b in {:.2?}", t.elapsed());

        let t = std::time::Instant::now();
        let mut into =
            zstd::Encoder::new(std::fs::File::create("4lbatches/tmp.dat").unwrap(), 9).unwrap();
        into.multithread(16).unwrap();
        for (key, archive) in normal_db.into_inner().unwrap() {
            let v = (key, Vec::from(archive));
            let buf = bincode::serialize(&v).unwrap();
            into.write_all(&(buf.len() as u64).to_le_bytes()).unwrap();
            into.write_all(&buf).unwrap();
        }
        into.finish().unwrap();
        std::fs::rename("4lbatches/tmp.dat", format!("4lbatches/{}.dat", i)).unwrap();
        println!("Saved normal in {:.2?}", t.elapsed());
    }
}

fn build_db() {
    let mut index = std::io::BufWriter::new(std::fs::File::create("4ldb-index.dat").unwrap());
    let mut data = std::io::BufWriter::new(std::fs::File::create("4ldb-data.dat").unwrap());
    let mut solns = std::io::BufWriter::new(std::fs::File::create("4ldb-placements.dat").unwrap());

    let mut next_data = 0;
    let mut next_index = 0;

    let nob2b = MergedBatches::new(false);
    let mut b2b = MergedBatches::new(true);

    for (pieces, nob2b_entries) in nob2b {
        let (key2, mut b2b_entries) = b2b.next().unwrap();
        assert_eq!(pieces, key2);

        // collate entries
        let mut entries = vec![];
        for mut entry in nob2b_entries {
            b2b_entries.retain(|&v| {
                if v == entry {
                    entry.mark_valid_b2b();
                    false
                } else {
                    true
                }
            });
            entry.mark_valid_nob2b();
            entries.push(entry);
        }
        for mut entry in b2b_entries {
            entry.mark_valid_b2b();
            entries.push(entry);
        }

        // write to DB
        let idx = compute_index(pieces);
        for _ in next_index..idx {
            index.write_all(&[0u8; 16]).unwrap();
        }
        if next_index / 282475 < (idx + 1) / 282475 {
            println!("{:.1}%", idx as f64 / 2824752.49);
        }
        next_index = idx + 1;

        index.write_all(bytemuck::bytes_of(&IndexEntry {
            index: next_data,
            len: entries.len() as u32,
        })).unwrap();

        for entry in entries {
            data.write_all(bytemuck::bytes_of(&DataEntry {
                score: entry.score,
                time_and_flags: entry.time_and_flags,
            })).unwrap();
            solns.write_all(bytemuck::bytes_of(&entry.placements)).unwrap();
            
            next_data += 1;
        }
    }
    assert!(b2b.next().is_none());

    for _ in next_index..7usize.pow(10) {
        index.write_all(&[0; 16]).unwrap();
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct DataEntry {
    score: u16,
    time_and_flags: u16,
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct IndexEntry {
    index: u32,
    len: u32,
}

impl DataEntry {
    fn time(&self) -> u16 {
        self.time_and_flags & (1 << 13) - 1
    }

    fn b2b(&self) -> bool {
        self.time_and_flags & 1 << 15 != 0
    }

    fn valid_nob2b(&self) -> bool {
        self.time_and_flags & 1 << 14 != 0
    }

    fn valid_b2b(&self) -> bool {
        self.time_and_flags & 1 << 13 != 0
    }
}

#[derive(PartialEq, Eq)]
struct PlacementOrd(pcf::Placement);

impl Ord for PlacementOrd {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .x
            .cmp(&other.0.x)
            .then((self.0.kind as usize).cmp(&(other.0.kind as usize)))
    }
}

impl PartialOrd for PlacementOrd {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq)]
struct PieceOrder(Placement);

impl PartialEq for PieceOrder {
    fn eq(&self, other: &Self) -> bool {
        self.0.piece == other.0.piece
    }
}

impl Ord for PieceOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0.piece as usize).cmp(&(other.0.piece as usize))
    }
}

impl PartialOrd for PieceOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Pod, Zeroable)]
#[repr(C)]
struct Entry {
    score: u16,
    time_and_flags: u16,
    placements: [u8; 10],
}

impl Entry {
    fn new(score: u16, time: u16, b2b: bool, placements: [u8; 10]) -> Self {
        Entry {
            score,
            placements,
            time_and_flags: time | (b2b as u16) << 15,
        }
    }

    fn time(&self) -> u16 {
        self.time_and_flags & (1 << 13) - 1
    }

    fn b2b(&self) -> bool {
        self.time_and_flags & 1 << 15 != 0
    }

    fn mark_valid_nob2b(&mut self) {
        self.time_and_flags |= 1 << 14;
    }

    fn mark_valid_b2b(&mut self) {
        self.time_and_flags |= 1 << 13;
    }
}

impl Dominance for Entry {
    fn covers(&self, other: &Self) -> bool {
        self.score >= other.score && self.time() <= other.time() && self.b2b() >= other.b2b()
    }
}

fn add(
    db: &Mutex<BTreeMap<[Piece; 10], Archive<Entry>>>,
    soln: &[Placement],
    score: u32,
    time: u32,
    b2b: bool,
) {
    let pieces = soln
        .iter()
        .map(|p| p.piece)
        .collect::<ArrayVec<_, 10>>()
        .into_inner()
        .unwrap_or_else(|_| unreachable!());

    let packed = soln
        .iter()
        .map(|p| p.pack())
        .collect::<ArrayVec<_, 10>>()
        .into_inner()
        .unwrap_or_else(|_| unreachable!());

    let entry = Entry::new(score as u16, time as u16, b2b, packed);

    let mut db = db.lock().unwrap();
    db.entry(pieces).or_default().add(entry);
}

fn compute_index(p: [Piece; 10]) -> usize {
    let mut idx = 0;
    for p in p {
        idx *= 7;
        idx += p as usize;
    }
    idx
}
