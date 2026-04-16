mod packings;
mod placements;

use std::path::PathBuf;
use std::slice::Iter;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use arrayvec::ArrayVec;
use enumset::EnumSet;
use foldhash::HashSet;
use pcf::{BitBoard, Piece, PieceSet};
use structopt::StructOpt;

use crate::bag_hold_seq::BagStates;
use crate::parse_seq;

#[derive(StructOpt)]
pub enum Options {
    GenSets,
    CountCombos { set: String },
    GenPackings,
    ComputePlacements { packings_file: PathBuf },
    GenSeqs { n: usize, set: String },
}

impl Options {
    pub fn run(self) {
        match self {
            Options::GenSets => {
                let sets = gen_sets();
                let mut sets: Vec<_> = sets.into_iter().collect();
                sets.sort_by_key(|s| s.0);
                for set in sets {
                    println!("{set}");
                }
            }
            Options::CountCombos { set } => {
                let mut s = PieceSet::default();
                for p in parse_seq(&set).unwrap() {
                    s = s.with(p);
                }

                let t = std::time::Instant::now();
                let count = AtomicU64::new(0);
                pcf::find_combinations_mt(s, BitBoard(0), &AtomicBool::new(false), 6, |_| {
                    count.fetch_add(1, Ordering::Relaxed);
                });
                dbg!(count.into_inner(), t.elapsed());
            }
            Options::GenPackings => packings::gen_packings(),
            Options::ComputePlacements { packings_file } => {
                placements::compute_placements(packings_file);
            }
            Options::GenSeqs { n, set } => {
                let mut s = PieceSet::default();
                for p in parse_seq(&set).unwrap() {
                    s = s.with(p);
                }
                gen_seqs(n, s);
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct PackedPieceSeq {
    raw: u64,
}

impl From<[Piece; 15]> for PackedPieceSeq {
    fn from(value: [Piece; 15]) -> Self {
        let mut v = 0;
        for i in 0..value.len() {
            v |= (value[i] as u64) << 3 * i;
        }
        PackedPieceSeq { raw: v }
    }
}

impl From<PackedPieceSeq> for [Piece; 15] {
    fn from(value: PackedPieceSeq) -> Self {
        let mut result = [Piece::S; 15];
        for i in 0..result.len() {
            let p = value.raw >> 3 * i & 7;
            result[i] = Piece::from(p as usize);
        }
        result
    }
}

fn gen_sets() -> HashSet<PieceSet> {
    let mut bags: [_; 7] = std::array::from_fn(|_| vec![]);
    for i in 0..7 {
        gen_bag(i, |bag| bags[i].push(bag));
    }

    let mut sets = HashSet::default();
    for pieces in 0..7 {
        for mut bag in bags[pieces].iter().copied() {
            let mut remain = 14 - pieces;
            while remain >= 7 {
                for p in pcf::PIECES {
                    bag = bag.with(p);
                }
                remain -= 7;
            }
            for &lastbag in &bags[remain] {
                let mut set = bag;
                for p in pcf::PIECES {
                    if lastbag.contains(p) {
                        set = set.with(p);
                    }
                }
                for p in pcf::PIECES {
                    sets.insert(set.with(p));
                }
            }
        }
    }

    sets
}

fn gen_bag(size: usize, mut f: impl FnMut(PieceSet)) {
    gen_bag_impl(size, pcf::PIECES.iter(), PieceSet::default(), &mut f);
}

fn gen_bag_impl(remain: usize, mut next: Iter<Piece>, bag: PieceSet, f: &mut impl FnMut(PieceSet)) {
    if remain == 0 {
        f(bag);
        return;
    }
    if next.as_slice().len() < remain {
        return;
    }
    while let Some(&p) = next.next() {
        gen_bag_impl(remain - 1, next.as_slice().iter(), bag.with(p), f);
    }
}

fn gen_seqs(n: usize, set: PieceSet) {
    let mut seqs = HashSet::default();
    let t = std::time::Instant::now();
    gen_seqs_impl(
        &mut seqs,
        &mut ArrayVec::new(),
        BagStates::new(),
        n,
        set,
    );
    dbg!(t.elapsed());

    let seqs = seqs.len();
    let total = 7u64.pow(n as u32);
    println!(
        "{seqs} / {total} ~ {:.2}%",
        seqs as f64 / total as f64 * 100.0,
    );
}

fn gen_seqs_impl(
    seqs: &mut HashSet<ArrayVec<crate::data::Piece, 15>>,
    current: &mut ArrayVec<crate::data::Piece, 15>,
    state: BagStates,
    n: usize,
    set: PieceSet,
) {
    if current.len() == n {
        seqs.insert(current.clone());
        return;
    }

    for p in crate::data::Piece::ALL {
        if !set.contains(p.into()) {
            continue;
        }
        let Some(new_state) = state.remove(p) else {
            continue;
        };
        current.push(p);
        gen_seqs_impl(seqs, current, new_state, n, set.without(p.into()));
        current.pop();
    }
}
