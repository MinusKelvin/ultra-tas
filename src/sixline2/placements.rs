use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use pcf::PieceSet;

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

    let t = Instant::now();

    let mut result_counts = HashMap::<_, u64>::new();

    packings.iter().take(1).for_each(|&packing| {
        find_placement_sequences(
            &mut vec![],
            pcf::BitBoard(0),
            &mut packing.into_iter().map(From::from).collect(),
            &mut |placement, _, _, _| {
                let order: [_; 15] = std::array::from_fn(|i| (placement[i].piece as usize).into());
                let order = PackedPieceSeq::from(order);
                *result_counts.entry(order).or_default() += 1;
                count.fetch_add(1, Ordering::Relaxed);
            },
            0,
            0,
            false,
            0,
        );

        let prog = progress.fetch_add(1, Ordering::Relaxed) + 1;

        // if prog % 1_000 == 0 {
        let count = count.load(Ordering::Relaxed);
        let d = t.elapsed();
        let eta = d.as_secs_f64() / prog as f64 * (packings.len() - prog as usize) as f64;
        print!(
            "    {count} placement seqs found (x{:.2} amplification), eta {:3}:{:02}:{:02}\r",
            count as f64 / prog as f64,
            (eta / 3600.0).round(),
            (eta / 60.0 % 60.0).round(),
            (eta % 60.0).round(),
        );
        _ = std::io::stdout().flush();
        // }
    });

    println!();
    println!("{} sequences", result_counts.len());
    println!("took {:.2?}", t.elapsed());
}
