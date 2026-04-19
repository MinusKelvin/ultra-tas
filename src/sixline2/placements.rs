use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use bytemuck::{Pod, Zeroable};
use dashmap::DashMap;
use rayon::prelude::*;

use crate::archive::{Archive, Dominance};
use crate::placement_search::{find_placement_sequences, B2bStatus};
use crate::sixline2::packings::{i_placement_may_tetris_pc, t_placement_may_tspin, RawPlacement};
use crate::sixline2::PackedPieceSeq;

pub fn compute_placements(packings_file: PathBuf) {
    let packings: Vec<[RawPlacement; 15]> =
        bytemuck::cast_vec(std::fs::read(&packings_file).unwrap());

    let progress = AtomicU64::new(0);
    let count = AtomicU64::new(0);

    std::fs::create_dir_all("6l-solutions").unwrap();

    let t = Instant::now();

    let results = DashMap::<_, Archive<_>>::default();

    packings.par_iter().for_each(|&packing| {
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
            &mut |placement, score, time, _| {
                count.fetch_add(1, Ordering::Relaxed);

                let order: [_; 15] = std::array::from_fn(|i| (placement[i].piece as usize).into());
                let order = PackedPieceSeq::from(order);

                let packed_placements = <[_; 15]>::try_from(placement).unwrap().map(|p| p.pack());

                // tsd-tetris pcs nob2b/b2b always differs by 600 points
                assert_eq!(score[0] + 600, score[1]);

                results.entry(order).or_default().add(Entry {
                    score: (score[0] as u16).to_le_bytes(),
                    time: (time as u16).to_le_bytes(),
                    placements: packed_placements,
                });
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
            (eta / 3600.0).floor(),
            (eta / 60.0 % 60.0).floor(),
            (eta % 60.0).floor(),
        );
        _ = std::io::stdout().flush();
        // }
    });

    println!();
    println!("{} piece sequences", results.len());
    println!(
        "{} non-dominated solutions (x{:.2} amplification)",
        results.iter().map(|data| data.len()).sum::<usize>(),
        results.iter().map(|data| data.len()).sum::<usize>() as f64 / progress.into_inner() as f64,
    );
    let d = t.elapsed().as_secs_f64();
    println!(
        "took {:3}:{:02}:{:02}",
        (d / 3600.0).floor(),
        (d / 60.0 % 60.0).floor(),
        (d % 60.0).floor(),
    );

    let t = Instant::now();

    let mut results: Vec<_> = results.into_iter().collect();
    results.par_sort_unstable_by_key(|&(seq, _)| seq);

    println!("took {:.2?} to sort", t.elapsed());

    let t = Instant::now();

    let mut file = BufWriter::new(
        File::create(Path::new("6l-solutions").join(packings_file.file_name().unwrap())).unwrap(),
    );

    for (seq, archive) in results {
        let size = archive.len();
        assert!(size < u16::MAX as usize);
        file.write_all(&(seq.raw | (size as u64) << 48).to_le_bytes())
            .unwrap();
        file.write_all(&bytemuck::cast_slice(&archive)).unwrap();
    }

    file.flush().unwrap();
    drop(file);

    println!("took {:.2?} to write to disk", t.elapsed());

    std::process::exit(0);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Pod, Zeroable)]
#[repr(C)]
struct Entry {
    score: [u8; 2],
    time: [u8; 2],
    placements: [u8; 15],
}

impl Entry {
    fn score(&self) -> u16 {
        u16::from_le_bytes(self.score)
    }

    fn time(&self) -> u16 {
        u16::from_le_bytes(self.time)
    }
}

impl Dominance for Entry {
    fn covers(&self, other: &Self) -> bool {
        self.score() >= other.score() && self.time() <= other.time()
    }
}
