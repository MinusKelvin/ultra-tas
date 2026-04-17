use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use foldhash::HashMap;
use rand::prelude::*;

use crate::archive::{Archive, Dominance};
use crate::placement_search::{find_placement_sequences, B2bStatus};
use crate::sixline2::packings::{i_placement_may_tetris_pc, t_placement_may_tspin, RawPlacement};
use crate::sixline2::PackedPieceSeq;

pub fn compute_placements(packings_file: PathBuf) {
    let mut packings = vec![];
    for f in packings_file.read_dir().unwrap() {
        let f = f.unwrap();
        if f.file_type().unwrap().is_file() {
            std::fs::File::open(f.path())
                .unwrap()
                .read_to_end(&mut packings)
                .unwrap();
        }
    }

    let mut packings: Vec<[RawPlacement; 15]> = bytemuck::cast_vec(packings);
    packings.shuffle(&mut rand::rngs::Xoshiro256PlusPlus::from_seed([
        0xef, 0xdb, 0xe1, 0xab, 0x15, 0xb8, 0x73, 0x60, 0xf1, 0xc7, 0x65, 0x81, 0xf6, 0xb4, 0x56,
        0x0e, 0x76, 0x4d, 0x88, 0x60, 0xb2, 0xe1, 0x7a, 0x3c, 0xae, 0x3a, 0xc6, 0x66, 0xbf, 0x99,
        0x23, 0xe9,
    ]));

    let progress = AtomicU64::new(0);
    let count = AtomicU64::new(0);

    let t = Instant::now();

    let mut results_nob2b = HashMap::<_, Archive<_>>::default();
    let mut results_b2b = HashMap::<_, Archive<_>>::default();

    packings.iter().take(1000).for_each(|&packing| {
        let packing: Vec<pcf::Placement> = packing.into_iter().map(From::from).collect();
        let hurdles = packing.iter().fold(0, |a, p| a | p.kind.hurdles());

        let mut t_candidates = packing
            .iter()
            .filter(|p| t_placement_may_tspin(p, hurdles, &packing));
        let only_t = t_candidates.next().xor(t_candidates.next()).copied();

        let mut i_candidates = packing
            .iter()
            .filter(|p| i_placement_may_tetris_pc(p, hurdles));
        let only_i = i_candidates.next().xor(i_candidates.next()).copied();

        find_placement_sequences(
            &mut vec![],
            pcf::BitBoard(0),
            &mut packing.clone(),
            &mut |placement, score, time, b2b| {
                count.fetch_add(1, Ordering::Relaxed);

                let order: [_; 15] = std::array::from_fn(|i| (placement[i].piece as usize).into());
                let order = PackedPieceSeq::from(order);

                let packed_placements = <[_; 15]>::try_from(placement).unwrap().map(|p| p.pack());

                // tsd-tetris pcs always differ in b2b
                assert_ne!(score[0], score[1]);

                let mut nob2b_entry = Entry::new(
                    score[0] as u16,
                    time as u16,
                    matches!(b2b, B2bStatus::B2b),
                    packed_placements,
                );
                nob2b_entry.mark_valid_nob2b();

                let mut b2b_entry = Entry::new(
                    score[1] as u16,
                    time as u16,
                    matches!(b2b, B2bStatus::B2b),
                    packed_placements,
                );
                b2b_entry.mark_valid_b2b();

                results_nob2b.entry(order).or_default().add(nob2b_entry);
                results_b2b.entry(order).or_default().add(b2b_entry);
            },
            [0; 2],
            0,
            B2bStatus::Uncertain,
            0,
            true,
            only_t,
            only_i,
        );

        let prog = progress.fetch_add(1, Ordering::Relaxed) + 1;

        // if prog % 1_000 == 0 {
        let count = count.load(Ordering::Relaxed);
        let d = t.elapsed();
        let eta = d.as_secs_f64() / prog as f64 * (packings.len() - prog as usize) as f64;
        print!(
            "  {prog}  {count} placement seqs found (x{:.2} amplification), eta {:3}:{:02}:{:02}\r",
            count as f64 / prog as f64,
            (eta / 3600.0).round(),
            (eta / 60.0 % 60.0).round(),
            (eta % 60.0).round(),
        );
        _ = std::io::stdout().flush();
        // }
    });

    println!();
    println!("{} piece sequences", results_b2b.len());
    println!(
        "{} non-dominated solutions (x{:.2} amplification)",
        results_b2b
            .values()
            .map(|archive| archive.len())
            .sum::<usize>()
            + results_nob2b
                .values()
                .map(|archive| archive.len())
                .sum::<usize>(),
        (results_b2b
            .values()
            .map(|archive| archive.len())
            .sum::<usize>()
            + results_nob2b
                .values()
                .map(|archive| archive.len())
                .sum::<usize>()) as f64
            / progress.into_inner() as f64,
    );
    println!("took {:.2?}", t.elapsed());
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
struct Entry {
    score: u16,
    time_and_flags: u16,
    placements: [u8; 15],
}

impl Entry {
    fn new(score: u16, time: u16, b2b: bool, placements: [u8; 15]) -> Self {
        Entry {
            score,
            time_and_flags: time | (b2b as u16) << 15,
            placements,
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
