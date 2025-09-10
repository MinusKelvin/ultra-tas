use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Mutex;

use bytemuck::{Pod, Zeroable};
use pcf::{BitBoard, PieceSet, Placement};

use crate::sixline2::gen_sets;

pub fn gen_packings() {
    let valid_sets = gen_sets();

    let mut big_set = PieceSet::default();
    for p in pcf::PIECES {
        big_set = big_set.with(p).with(p).with(p).with(p);
    }

    let t = std::time::Instant::now();
    let count = AtomicU64::new(0);
    let valid_count = AtomicU64::new(0);

    std::fs::create_dir_all("6l-packings").unwrap();

    let packings: HashMap<_, _> = valid_sets
        .into_iter()
        .map(|set| {
            let path = Path::new("6l-packings").join(format!("{set}.dat"));
            let file = File::create(path).unwrap();
            (set, Mutex::new(BufWriter::new(file)))
        })
        .collect();

    pcf::find_combinations_with_pruning_mt(
        big_set,
        BitBoard(0),
        &AtomicBool::new(false),
        6,
        |placements, _, _, _, _| packing_could_support_tsd_tetris(placements),
        |placements| {
            if !packing_could_support_tsd_tetris(placements) {
                return;
            }
            let c = count.fetch_add(1, Ordering::Relaxed) + 1;

            let mut set = PieceSet::default();
            for placement in placements {
                set = set.with(placement.kind.piece());
            }

            if let Some(writer) = packings.get(&set) {
                let mut iter = placements.iter();
                let data = [(); 15]
                    .map(|_| *iter.next().unwrap())
                    .map(RawPlacement::from);
                writer
                    .lock()
                    .unwrap()
                    .write_all(bytemuck::cast_slice(&data))
                    .unwrap();
                valid_count.fetch_add(1, Ordering::Relaxed);
            }

            if c % 100_000 == 0 {
                let valid = valid_count.load(Ordering::Relaxed);
                let d = t.elapsed();
                print!(
                    "    {valid} packings found ({}k/sec), running for {:3}:{:02}:{:02}\r",
                    (valid as f64 / d.as_secs_f64() / 1000.0).round(),
                    d.as_secs() / 3600,
                    d.as_secs() / 60 % 60,
                    d.as_secs() % 60,
                );
                _ = std::io::stdout().flush();
            }
        },
    );

    let d = t.elapsed();
    println!(
        "Found {} combinations, of which {} used valid piece sets in {}:{:02}:{:02}",
        count.into_inner(),
        valid_count.into_inner(),
        d.as_secs() / 3600,
        d.as_secs() / 60 % 60,
        d.as_secs() % 60,
    );
}

fn packing_could_support_tsd_tetris(placements: &[Placement]) -> bool {
    let hurdles = placements.iter().fold(0, |a, p| a | p.kind.hurdles());

    if hurdles == 0 {
        return true;
    }

    if hurdles >> hurdles.trailing_zeros() != 3 {
        return false;
    }

    placements
        .iter()
        .all(|p| p.kind.hurdles() == 0 || p.kind.hurdles() == hurdles)
}

#[derive(Copy, Clone, Debug, Pod, Zeroable)]
#[repr(C)]
struct RawPlacement(u16);

impl From<Placement> for RawPlacement {
    fn from(value: Placement) -> Self {
        RawPlacement(value.kind as u16 | (value.x as u16) << 9)
    }
}

impl From<RawPlacement> for Placement {
    fn from(value: RawPlacement) -> Self {
        let kind = value.0 & 0x1FF;
        let x = value.0 >> 9;
        Placement {
            kind: unsafe {
                assert!(kind <= pcf::PieceState::IHorizontal5 as u16);
                std::mem::transmute(kind)
            },
            x: x as u8,
        }
    }
}
