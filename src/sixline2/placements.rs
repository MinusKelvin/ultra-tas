use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use pcf::PieceSet;

use crate::archive::{Archive, Dominance};
use crate::parse_seq;
use crate::placement_search::find_placement_sequences;
use crate::sixline2::packings::RawPlacement;
use crate::sixline2::PackedPieceSeq;

pub fn compute_placements(packings_file: PathBuf) {
    let set = packings_file.file_stem().unwrap().to_str().unwrap();
    let set: PieceSet = parse_seq(set).unwrap().into_iter().collect();
    let set = format!("{set}");

    let packings: Vec<[RawPlacement; 15]> =
        bytemuck::cast_vec(std::fs::read(packings_file).unwrap());

    let progress = AtomicU64::new(0);
    let count = AtomicU64::new(0);
    let real_count = AtomicU64::new(0);

    let t = Instant::now();

    let mut result_counts = HashMap::<_, u64>::new();
    let mut results = HashMap::<_, Archive<_>>::new();

    packings.iter().take(1).for_each(|&packing| {
        let mut non_dominated = HashMap::<_, Archive<_>>::new();

        find_placement_sequences(
            &mut vec![],
            pcf::BitBoard(0),
            &mut packing.into_iter().map(From::from).collect(),
            &mut |placement, score, time, b2b| {
                let order: [_; 15] = std::array::from_fn(|i| (placement[i].piece as usize).into());
                let order = PackedPieceSeq::from(order);
                *result_counts.entry(order).or_default() += 1;
                non_dominated.entry(order).or_default().add(Entry::new(
                    score as u16,
                    time as u16,
                    b2b,
                ));
                results.entry(order).or_default().add(Entry::new(
                    score as u16,
                    time as u16,
                    b2b,
                ));
                count.fetch_add(1, Ordering::Relaxed);
            },
            0,
            0,
            false,
            0,
            true,
        );

        let nondom = non_dominated
            .values()
            .map(|archive| archive.len() as u64)
            .sum();
        let real_count = real_count.fetch_add(nondom, Ordering::Relaxed) + nondom;
        let prog = progress.fetch_add(1, Ordering::Relaxed) + 1;

        // if prog % 1_000 == 0 {
        let count = count.load(Ordering::Relaxed);
        let d = t.elapsed();
        let eta = d.as_secs_f64() / prog as f64 * (packings.len() - prog as usize) as f64;
        print!(
            "    {count} ({real_count}) placement seqs found (x{:.2} (x{:.2}) amplification), eta {:3}:{:02}:{:02}\r",
            count as f64 / prog as f64,
            real_count as f64 / prog as f64,
            (eta / 3600.0).round(),
            (eta / 60.0 % 60.0).round(),
            (eta % 60.0).round(),
        );
        _ = std::io::stdout().flush();
        // }
    });

    println!();
    println!("{} sequences", result_counts.len());
    println!(
        "{} non-dominated (x{:.2} amplification)",
        results.values().map(|archive| archive.len()).sum::<usize>(),
        results.values().map(|archive| archive.len()).sum::<usize>() as f64
            / progress.into_inner() as f64,
    );
    println!("took {:.2?}", t.elapsed());
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
struct Entry {
    score: u16,
    time_and_flags: u16,
}

impl Entry {
    fn new(score: u16, time: u16, b2b: bool) -> Self {
        Entry {
            score,
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
