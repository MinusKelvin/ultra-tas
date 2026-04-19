use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{LazyLock, Mutex};

use bytemuck::{Pod, Zeroable};
use pcf::{BitBoard, Piece, PieceSet, Placement, Rotation};

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
            (
                set,
                LazyLock::new(move || {
                    let path = Path::new("6l-packings").join(format!("{set}.dat"));
                    let file = File::create(path).unwrap();
                    Mutex::new(BufWriter::new(file))
                }),
            )
        })
        .collect();

    pcf::find_combinations_with_pruning_mt(
        big_set,
        BitBoard(0),
        &AtomicBool::new(false),
        6,
        |placements, _, _, _, _| packing_has_viable_hurdles(placements),
        |placements| {
            if !packing_has_viable_hurdles(placements) {
                return;
            }
            if !packing_has_t_placement(placements) {
                return;
            }
            if !packing_has_i_placement(placements) {
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

            if c % 10_000 == 0 {
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

fn packing_has_viable_hurdles(placements: &[Placement]) -> bool {
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

fn packing_has_t_placement(placements: &[Placement]) -> bool {
    let hurdles = placements.iter().fold(0, |a, p| a | p.kind.hurdles());

    for piece in placements {
        if t_placement_may_tspin(piece, hurdles, placements) {
            return true;
        }
    }

    false
}

pub fn t_placement_may_tspin(piece: &Placement, hurdles: u8, placements: &[Placement]) -> bool {
    let top_row = BitBoard::filled(6).remove(BitBoard::filled(5));

    // only ordinary T-slots are considered
    if piece.kind.piece() == Piece::T
        && piece.kind.piece_srs()[0].rotation == Rotation::South
        // T-spin is the only allowed clear, so the piece must not hurdle anything
        && piece.kind.hurdles() == 0
        // T-spin is not possible if the T is in the top row
        && !piece.kind.board().overlaps(top_row)
        // If there are hurdles, then the T must be in the row where the hurdle happens
        && (hurdles == 0 || piece.kind.y() as u32 == hurdles.trailing_zeros())
        // If there are no hurdles, then the T must be in the bottom row
        && (hurdles != 0 || piece.kind.y() == 0)
    {
        // check that at least one corner cell is filled by a piece which is not hurdled
        let corner_y = piece.kind.y() as usize + 2;
        let corner_x = piece.x as usize;

        for piece in placements {
            // such a placement must occur before the T-spin, so it must not hurdle
            // additionally, it must fill a corner
            if piece.kind.hurdles() == 0
                && (piece.board().cell_filled(corner_x, corner_y)
                    || piece.board().cell_filled(corner_x + 2, corner_y))
                && !piece.board().cell_filled(corner_x + 1, corner_y)
            {
                return true;
            }
        }
    }

    false
}

fn packing_has_i_placement(placements: &[Placement]) -> bool {
    let hurdles = placements.iter().fold(0, |a, p| a | p.kind.hurdles());

    for piece in placements {
        if i_placement_may_tetris_pc(piece, hurdles) {
            return true;
        }
    }

    false
}

pub fn i_placement_may_tetris_pc(piece: &Placement, hurdles: u8) -> bool {
    // note: first srs piece for I is always West orientation
    piece.kind.piece() == Piece::I
        && piece.kind.piece_srs()[0].rotation == Rotation::West
        // I must hurdle T-spin in order to perform tetris afterwards
        && piece.kind.hurdles() == hurdles
        // If there are no hurdles, the T-spin is in the bottom rows, so the I must be in the top
        && (hurdles != 0 || piece.kind.y() == 2)
}

#[derive(Copy, Clone, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct RawPlacement([u8; 2]);

impl From<Placement> for RawPlacement {
    fn from(value: Placement) -> Self {
        RawPlacement((value.kind as u16 | (value.x as u16) << 9).to_le_bytes())
    }
}

impl From<RawPlacement> for Placement {
    fn from(value: RawPlacement) -> Self {
        let raw = u16::from_le_bytes(value.0);
        let kind = raw & 0x1FF;
        let x = raw >> 9;
        Placement {
            kind: unsafe {
                assert!(kind <= pcf::PieceState::IHorizontal5 as u16);
                std::mem::transmute(kind)
            },
            x: x as u8,
        }
    }
}
