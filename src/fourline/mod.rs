use std::sync::Mutex;

use arrayvec::ArrayVec;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use structopt::StructOpt;

use crate::data::{line_clear_delay, line_clear_score, Board, Piece, Placement, Rotation, Spin};
use crate::pathfind::{pathfind, Input};

#[derive(StructOpt)]
pub enum Options {
    Generate {
        #[structopt(long)]
        b2b: bool,
    },
}

const DATABASE_SIZE: usize = 7usize.pow(10);

impl Options {
    pub fn run(self) {
        let b2b = match self {
            Options::Generate { b2b } => b2b,
        };

        let piece_set = pcf::PIECES.repeat(4).into_iter().collect();

        let combo_count = std::sync::atomic::AtomicU64::new(0);
        let mut database = Mutex::new(
            std::iter::repeat_with(SmallVec::new)
                .take(DATABASE_SIZE)
                .collect(),
        );
        let t = std::time::Instant::now();

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
                    &mut |soln, score, time| {
                        add(&database, soln, score, time);
                    },
                    0,
                    0,
                    b2b,
                );
                let count = combo_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if 1000 * count / 24663998 != 1000 * (count + 1) / 24663998 {
                    println!(
                        "{:.1}%, {:?}",
                        (count + 1) as f64 * 100.0 / 24663998.0,
                        t.elapsed()
                    );
                }
            },
        );
        let mut total = 0;
        let mut spilled = 0;
        let mut longest = 0;
        for c in database.get_mut().unwrap().iter() {
            if !c.is_empty() {
                total += 1;
            }
            if c.spilled() {
                spilled += 1;
            }
            longest = longest.max(c.len());
        }
        println!(
            "{:.1}% spilled, {} longest, {:.2?}",
            spilled as f64 / total as f64 * 100.0,
            longest,
            t.elapsed(),
        );

        let f = std::fs::File::create(match b2b {
            true => "4line-b2b.dat",
            false => "4line-nob2b.dat",
        })
        .unwrap();
        let f = std::io::BufWriter::new(f);
        bincode::serialize_into(f, database.get_mut().unwrap()).unwrap();
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
struct Entry {
    score: u16,
    time: u16,
    placements: [u8; 10],
}

impl Entry {
    fn dominates(&self, other: &Entry) -> bool {
        self.score >= other.score && self.time <= other.time
    }
}

fn add(db: &Mutex<Vec<SmallVec<[Entry; 1]>>>, soln: &[Placement], score: u32, time: u32) {
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

    let entry = Entry {
        placements: packed,
        score: score as u16,
        time: time as u16,
    };

    let mut db = db.lock().unwrap();
    let archive = &mut db[compute_index(pieces)];
    if !archive.iter().any(|e| e.dominates(&entry)) {
        archive.retain(|e| !entry.dominates(e));
        archive.push(entry);
        if archive.len() <= archive.inline_size() && archive.spilled() {
            archive.shrink_to_fit();
        }
    }
}

fn find_placement_sequences(
    current: &mut Vec<Placement>,
    board: pcf::BitBoard,
    remaining: &mut Vec<pcf::Placement>,
    found: &mut impl FnMut(&[Placement], u32, u32),
    score: u32,
    time: u32,
    b2b: bool,
) {
    if remaining.is_empty() {
        found(current, score, time);
    }
    for i in 0..remaining.len() {
        let placement = remaining[i];
        if board.overlaps(placement.board()) || !placement.supported(board) {
            continue;
        }

        let cleared = board.lines_cleared();

        let place = placement.srs_piece(board)[0].into();
        let info = match evaluate(cleared, place, b2b) {
            Some(info) => info,
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
            score + info.score,
            time + info.time,
            info.b2b,
        );

        current.pop();
        remaining.push(placement);
        let last_index = remaining.len() - 1;
        remaining.swap(i, last_index);
    }
}

fn evaluate(b: pcf::BitBoard, place: Placement, b2b: bool) -> Option<PlacementEvaluation> {
    let mut board = Board([0; 10]);
    for y in 0..6 {
        for x in 0..10 {
            if b.cell_filled(x, y) {
                board.0[x] |= 1 << y;
            }
        }
    }

    let (movement_score, movements) = pathfind(&board, place)?;

    let mut spin = Spin::Nope;
    let &last_move = movements.last().unwrap();
    if place.piece == Piece::T
        && !(Input::Cw | Input::Ccw).is_disjoint(last_move)
        && (Placement {
            y: place.y + 1,
            ..place
        })
        .obstructed(&board)
    {
        let mini_corners = [(-1, 1), (1, 1)];
        let other_corners = [(-1, -1), (1, -1)];

        let mini_corners = IntoIterator::into_iter(mini_corners)
            .map(|c| place.rotation.rotate_cell(c))
            .filter(|&(x, y)| board.is_filled((x + place.x, y + place.y)))
            .count();

        let other_corners = IntoIterator::into_iter(other_corners)
            .map(|c| place.rotation.rotate_cell(c))
            .filter(|&(x, y)| board.is_filled((x + place.x, y + place.y)))
            .count();

        if mini_corners + other_corners > 3 {
            if mini_corners == 2 {
                spin = Spin::Full;
            } else {
                spin = Spin::Mini;
            }
        }
    }

    for c in place.cells() {
        board.fill(c);
    }

    let perfect_clear = board.0 == [board.line_clears(); 10];
    let lines_cleared = board.line_clears().count_ones();

    Some(PlacementEvaluation {
        score: movement_score + line_clear_score(lines_cleared, perfect_clear, b2b, spin),
        time: movements.len() as u32 + line_clear_delay(lines_cleared, perfect_clear),
        b2b: match (lines_cleared, spin) {
            (0, _) => b2b,
            (4, _) => true,
            (_, Spin::Nope) => false,
            _ => true,
        },
    })
}

struct PlacementEvaluation {
    score: u32,
    time: u32,
    b2b: bool,
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

fn compute_index(p: [Piece; 10]) -> usize {
    let mut idx = 0;
    for p in p {
        idx *= 7;
        idx += p as usize;
    }
    idx
}
