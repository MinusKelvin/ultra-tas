use structopt::StructOpt;

use crate::data::{Board, Piece, Placement, Rotation};
use crate::pathfind::pathfind;

#[derive(StructOpt)]
pub enum Options {
    Generate,
}

impl Options {
    pub fn run(self) {
        let piece_set = pcf::PIECES.repeat(4).into_iter().collect();
        let soln_count = std::sync::atomic::AtomicU64::new(0);
        let combo_count = std::sync::atomic::AtomicU64::new(0);
        pcf::find_combinations_mt(
            piece_set,
            pcf::BitBoard(0),
            &Default::default(),
            4,
            |combo| {
                find_placement_sequences(
                    &mut vec![],
                    pcf::BitBoard(0),
                    &mut combo.to_owned(),
                    &mut |_| {
                        soln_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    },
                    0,
                    0,
                );
                let count = combo_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if 1000 * count / 24663998 != 1000 * (count + 1) / 24663998 {
                    println!(
                        "{:.1}%, {} solns",
                        (count + 1) as f64 * 100.0 / 24663998.0,
                        soln_count.load(std::sync::atomic::Ordering::Relaxed)
                    );
                    if 100 * (count + 1) / 24663998 != 0 {
                        std::process::exit(0);
                    }
                }
            },
        );

        println!();
        println!(
            "{} combos, {} solns",
            combo_count.into_inner(),
            soln_count.into_inner()
        );
    }
}

fn find_placement_sequences(
    current: &mut Vec<Placement>,
    board: pcf::BitBoard,
    remaining: &mut Vec<pcf::Placement>,
    found: &mut impl FnMut(&[Placement]),
    score: u32,
    time: u32,
) {
    if remaining.is_empty() {
        found(current);
    }
    for i in 0..remaining.len() {
        let placement = remaining[i];
        if board.overlaps(placement.board()) || !placement.supported(board) {
            continue;
        }

        let cleared = board.lines_cleared();
        let mut board_2 = Board([0; 10]);
        for y in 0..6 {
            for x in 0..10 {
                if cleared.cell_filled(x, y) {
                    board_2.0[x] |= 1 << y;
                }
            }
        }

        let place = placement.srs_piece(board)[0].into();
        let (extra_score, extra_time) = match pathfind(&board_2, place) {
            Some((es, mvs)) => (es, mvs.len() as u32),
            None => continue,
        };

        let new_board = board.combine(placement.board());

        remaining.swap_remove(i);
        current.push(place);

        find_placement_sequences(
            current,
            new_board,
            remaining,
            found,
            score + extra_score,
            time + extra_time,
        );

        current.pop();
        remaining.push(placement);
        let last_index = remaining.len() - 1;
        remaining.swap(i, last_index);
    }
}

impl From<pcf::SrsPiece> for Placement {
    fn from(p: pcf::SrsPiece) -> Self {
        Placement {
            piece: p.piece.into(),
            rotation: p.rotation.into(),
            x: p.x as i8,
            y: p.y as i8,
        }
    }
}

impl From<pcf::Piece> for Piece {
    fn from(p: pcf::Piece) -> Self {
        match p {
            pcf::Piece::I => Piece::I,
            pcf::Piece::O => Piece::O,
            pcf::Piece::T => Piece::T,
            pcf::Piece::L => Piece::L,
            pcf::Piece::J => Piece::J,
            pcf::Piece::S => Piece::S,
            pcf::Piece::Z => Piece::Z,
        }
    }
}

impl From<pcf::Rotation> for Rotation {
    fn from(r: pcf::Rotation) -> Self {
        match r {
            pcf::Rotation::North => Rotation::North,
            pcf::Rotation::East => Rotation::East,
            pcf::Rotation::South => Rotation::South,
            pcf::Rotation::West => Rotation::West,
        }
    }
}
