use std::collections::HashSet;
use std::slice::Iter;

use pcf::{Piece, PieceSet};
use structopt::StructOpt;

#[derive(StructOpt)]
pub enum Options {
    GenSets,
}

impl Options {
    pub fn run(self) {
        match self {
            Options::GenSets => gen_sets(),
        }
    }
}

fn gen_sets() {
    let mut bags: [_; 7] = std::array::from_fn(|_| vec![]);
    for i in 0..7 {
        gen_bag(i, |bag| bags[i].push(bag));
    }

    let mut sets = HashSet::new();
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

    let mut sets: Vec<_> = sets.into_iter().collect();
    sets.sort_by_key(|s| s.0);
    for s in sets {
        println!("{s}");
    }
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
