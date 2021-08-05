use std::collections::BTreeMap;
use std::convert::TryInto;
use std::io::Write;
use std::sync::Mutex;

use arrayvec::ArrayVec;
use bytemuck::{Pod, Zeroable};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use structopt::StructOpt;

use crate::data::{line_clear_delay, line_clear_score, Board, Piece, Placement, Rotation, Spin};
use crate::pathfind::{pathfind, Input};

use self::merge::MergedBatches;

mod merge;

#[derive(StructOpt)]
pub enum Options {
    GenBatches,
    BuildDb,
}

const DATABASE_SIZE: usize = 7usize.pow(10);

impl Options {
    pub fn run(self) {
        match self {
            Options::GenBatches => generate_batches(),
            Options::BuildDb => {
                rayon::join(|| build_db(false), || build_db(true));
            }
        }
    }
}

fn generate_batches() {
    let piece_set = pcf::PIECES.repeat(4).into_iter().collect();

    let combos = Mutex::new(vec![]);

    let t = std::time::Instant::now();
    pcf::find_combinations_mt(
        piece_set,
        pcf::BitBoard(0),
        &Default::default(),
        4,
        |combo| {
            combos.lock().unwrap().push([
                PlacementOrd(combo[0]),
                PlacementOrd(combo[1]),
                PlacementOrd(combo[2]),
                PlacementOrd(combo[3]),
                PlacementOrd(combo[4]),
                PlacementOrd(combo[5]),
                PlacementOrd(combo[6]),
                PlacementOrd(combo[7]),
                PlacementOrd(combo[8]),
                PlacementOrd(combo[9]),
            ]);
        },
    );
    println!("Took {:.2?} to compute combos", t.elapsed());

    let mut combos = combos.into_inner().unwrap();
    let t = std::time::Instant::now();
    combos.sort();
    println!("Took {:.2?} to sort combos", t.elapsed());

    let mut subdivs = vec![&combos[..]];
    for _ in 0..10 {
        let mut new_subdivs = vec![];
        for div in subdivs {
            let (left, right) = div.split_at(div.len() / 2);
            new_subdivs.push(left);
            new_subdivs.push(right);
        }
        subdivs = new_subdivs;
    }

    std::fs::create_dir_all("4lbatches").unwrap();
    for (i, div) in subdivs.into_iter().enumerate() {
        if std::fs::metadata(format!("4lbatches/{}.dat", i)).is_ok() {
            continue;
        }

        let t = std::time::Instant::now();
        let normal_db = Mutex::new(BTreeMap::new());
        let b2b_db = Mutex::new(BTreeMap::new());
        div.par_iter().for_each(|combo| {
            find_placement_sequences(
                &mut vec![],
                pcf::BitBoard(0),
                &mut combo.iter().map(|p| p.0).collect(),
                &mut |order, score, time| add(&normal_db, order, score, time),
                0,
                0,
                false,
            );
            find_placement_sequences(
                &mut vec![],
                pcf::BitBoard(0),
                &mut combo.iter().map(|p| p.0).collect(),
                &mut |order, score, time| add(&b2b_db, order, score, time),
                0,
                0,
                true,
            );
        });
        println!("Batch {} took {:.2?}", i, t.elapsed());

        let t = std::time::Instant::now();
        let mut into = zstd::Encoder::new(
            std::fs::File::create(format!("4lbatches/{}-b2b.dat", i)).unwrap(),
            9,
        )
        .unwrap();
        into.multithread(16).unwrap();
        for v in b2b_db.into_inner().unwrap() {
            let buf = bincode::serialize(&v).unwrap();
            into.write_all(&(buf.len() as u64).to_le_bytes()).unwrap();
            into.write_all(&buf).unwrap();
        }
        into.finish().unwrap();
        println!("Saved b2b in {:.2?}", t.elapsed());

        let t = std::time::Instant::now();
        let mut into = zstd::Encoder::new(
            std::fs::File::create(format!("4lbatches/{}.dat", i)).unwrap(),
            9,
        )
        .unwrap();
        into.multithread(16).unwrap();
        for v in normal_db.into_inner().unwrap() {
            let buf = bincode::serialize(&v).unwrap();
            into.write_all(&(buf.len() as u64).to_le_bytes()).unwrap();
            into.write_all(&buf).unwrap();
        }
        into.finish().unwrap();
        println!("Saved normal in {:.2?}", t.elapsed());
    }
}

fn build_db(b2b: bool) {
    let mut excess = vec![];

    let mut output = std::io::BufWriter::new(
        std::fs::File::create(match b2b {
            false => "4ldb.dat",
            true => "4ldb-b2b.dat",
        })
        .unwrap(),
    );

    let base_offset = DATABASE_SIZE * 16;

    let mut prev_index = 0;
    for (pieces, entries) in MergedBatches::new(b2b) {
        let idx = compute_index(pieces);
        for _ in prev_index + 1..idx {
            output.write_all(&[0u8; 16]).unwrap();
        }
        prev_index = idx;

        let len: u16 = entries.len().try_into().unwrap();
        let mut data = [0u8; 16];

        match len {
            0 => {}
            1 => {
                let data: &mut SmallDbEntry = bytemuck::cast_mut(&mut data);
                data.len = len;
                data.entry = entries[0];
            }
            _ => {
                let data: &mut LargeDbEntry = bytemuck::cast_mut(&mut data);
                data.len = len;
                data.offset = (base_offset + excess.len() * std::mem::size_of::<Entry>()) as u64;
                excess.extend_from_slice(&entries);
            }
        }

        output.write_all(&data).unwrap();
    }

    output.write_all(bytemuck::cast_slice(&excess)).unwrap();
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct SmallDbEntry {
    len: u16,
    entry: Entry,
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct LargeDbEntry {
    len: u16,
    _padding: [u8; 6],
    offset: u64,
}

#[derive(PartialEq, Eq)]
struct PlacementOrd(pcf::Placement);

impl Ord for PlacementOrd {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .x
            .cmp(&other.0.x)
            .then((self.0.kind as usize).cmp(&(other.0.kind as usize)))
    }
}

impl PartialOrd for PlacementOrd {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq)]
struct PieceOrder(Placement);

impl PartialEq for PieceOrder {
    fn eq(&self, other: &Self) -> bool {
        self.0.piece == other.0.piece
    }
}

impl Ord for PieceOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0.piece as usize).cmp(&(other.0.piece as usize))
    }
}

impl PartialOrd for PieceOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, Pod, Zeroable)]
#[repr(C)]
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

fn add(
    db: &Mutex<BTreeMap<[Piece; 10], SmallVec<[Entry; 1]>>>,
    soln: &[Placement],
    score: u32,
    time: u32,
) {
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
    add_to_archive(db.entry(pieces).or_default(), entry);
}

fn add_to_archive(archive: &mut SmallVec<[Entry; 1]>, entry: Entry) {
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
        time: movements.len() as u32 + line_clear_delay(lines_cleared, perfect_clear) + 7,
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
