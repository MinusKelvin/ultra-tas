use std::collections::VecDeque;
use std::convert::TryInto;
use std::io::Write;
use std::rc::Rc;

use enumset::EnumSet;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use structopt::StructOpt;

use crate::archive::{Archive, Dominance};
use crate::data::{line_clear_delay, Board, Piece, Placement, SPAWN_DELAY};
use crate::fourline::{FourLineDb, FourLinePlacementsDb};
use crate::pathfind::{pathfind, Input};
use crate::solve::edges::Edges;
use crate::twoline::TwoLineDb;

mod edges;

const NUM_SEEDS: u32 = 1 << 16;
const ULTRA_LENGTH: u32 = 10803;

#[derive(StructOpt)]
pub struct Options {}

impl Options {
    pub fn run(self) {
        let two_line_db = TwoLineDb::gen();
        let four_line_db = FourLineDb::load();

        std::fs::create_dir_all("solutions").unwrap();
        (0..NUM_SEEDS).into_par_iter().for_each(|seed| {
            let target = format!("solutions/{:04X}", seed);
            if std::path::Path::new(&target).exists() {
                return;
            }
            let queue = gen_queue_ppt1(seed);

            let mut solutions: Vec<_> = solve_sequence(&queue, &two_line_db, &four_line_db).into();
            solutions.sort_by_key(|a| a.score);

            let best = match solutions.last() {
                Some(v) => v,
                None => return,
            };
            println!("{:04X}: {}, {}", seed, best.score, best.time,);

            let mut placement_set = vec![];
            let mut seq = best.placement_sequence.as_ref();
            while let Some(s) = seq {
                placement_set.push(s.placements);
                seq = s.next.as_ref();
            }
            placement_set.reverse();

            bincode::serialize_into(std::fs::File::create(target).unwrap(), &placement_set)
                .unwrap();
        });

        drop(two_line_db);
        drop(four_line_db);

        let four_line_placements = FourLinePlacementsDb::load();

        (0..NUM_SEEDS).into_par_iter().for_each(|seed| {
            let file = match std::fs::File::open(format!("solutions/{:04X}", seed)) {
                Ok(f) => f,
                Err(_) => return,
            };
            let placement_sets: Vec<PlacementSet> = bincode::deserialize_from(file).unwrap();
            let queue = gen_queue_ppt1(seed);

            let mut placements = vec![];
            for set in placement_sets {
                match set {
                    PlacementSet::FivePiece(places) => {
                        placements.extend_from_slice(&places);
                    }
                    PlacementSet::TenPiece(index, queue) => {
                        placements.extend_from_slice(&four_line_placements.get(
                            index,
                            queue,
                        ));
                    }
                }
            }

            let result = input_sequence(&placements, &queue);
            write_tas(seed, &result[2..]);
        });
    }
}

#[derive(Clone, Default)]
struct SolveState {
    score: u32,
    time: u32,
    b2b: bool,
    placement_sequence: Option<Rc<PlacementSequenceList>>,
}

struct PlacementSequenceList {
    next: Option<Rc<PlacementSequenceList>>,
    placements: PlacementSet,
}

impl Dominance for SolveState {
    fn covers(&self, other: &Self) -> bool {
        self.score >= other.score && self.time <= other.time && self.b2b >= other.b2b
    }
}

#[derive(Default)]
struct Layer {
    hold_piece: [Archive<SolveState>; 7],
    empty_hold: Archive<SolveState>,
}

#[derive(Copy, Clone, Serialize, Deserialize)]
pub enum PlacementSet {
    FivePiece([Placement; 5]),
    TenPiece(u32, [Piece; 10]),
}

fn input_sequence(placements: &[Placement], queue: &[Piece]) -> Vec<EnumSet<Input>> {
    let mut inputs = vec![];
    let mut queue = queue;
    let mut has_done_hold = false;
    let mut board = Board([0; 10]);
    for &placement in placements {
        inputs.extend_from_slice(&[EnumSet::empty(); SPAWN_DELAY as usize]);
        if placement.piece != queue[0] {
            inputs.push(EnumSet::only(Input::Hold));
            if !has_done_hold {
                has_done_hold = true;
                inputs.extend_from_slice(&[EnumSet::empty(); SPAWN_DELAY as usize]);
                queue = &queue[1..];
            }
        }
        queue = &queue[1..];
        inputs.append(&mut pathfind(&board, placement).unwrap().1);
        for c in placement.cells() {
            board.fill(c);
        }
        let clears = board.line_clears();
        for c in &mut board.0 {
            unsafe {
                *c = std::arch::x86_64::_pext_u32(*c as u32, !clears as u32) as u8;
            }
        }
        let delay = line_clear_delay(clears.count_ones(), board == Board([0; 10]));
        for _ in 0..delay {
            inputs.push(EnumSet::empty());
        }
    }
    inputs
}

fn write_tas(seed: u32, inputs: &[EnumSet<Input>]) {
    let mut file =
        std::io::BufWriter::new(std::fs::File::create(format!("solutions/{:04X}", seed)).unwrap());

    file.write(format!("{:04X}\n", seed).as_bytes()).unwrap();
    for &frame in inputs {
        if frame.is_empty() {
            file.write_all(b"_").unwrap();
        }
        for input in frame {
            match input {
                Input::Left => file.write_all(b"<").unwrap(),
                Input::Right => file.write_all(b">").unwrap(),
                Input::Cw => file.write_all(b"R").unwrap(),
                Input::Ccw => file.write_all(b"L").unwrap(),
                Input::Softdrop => file.write_all(b"v").unwrap(),
                Input::HardDrop => file.write_all(b"D").unwrap(),
                Input::Hold => file.write_all(b"H").unwrap(),
            };
        }
        file.write_all(b"\n").unwrap();
    }
}

fn solve_sequence(
    queue: &[Piece],
    twoline_db: &TwoLineDb,
    fourline_db: &FourLineDb,
) -> Archive<SolveState> {
    let mut start_layer = Layer::default();
    start_layer.empty_hold.add(SolveState::default());

    let mut layers = VecDeque::new();
    layers.push_back(start_layer);

    let mut finish_states = Archive::new();

    let mut queue = queue;

    while let Some(layer) = layers.pop_front() {
        for p in Piece::ALL {
            let starts = &layer.hold_piece[p as usize];
            if starts.is_empty() {
                continue;
            }
            let mut edges = Edges::new();
            twoline_edges(&mut edges, twoline_db, Some(p), &queue[1..]);
            fourline_edges(&mut edges, fourline_db, Some(p), &queue[1..]);
            advance_edges(&mut finish_states, &mut layers, starts, &edges);
        }
        if !layer.empty_hold.is_empty() {
            let mut edges = Edges::new();
            twoline_edges(&mut edges, twoline_db, None, queue);
            fourline_edges(&mut edges, fourline_db, None, queue);
            advance_edges(&mut finish_states, &mut layers, &layer.empty_hold, &edges);
        }

        queue = &queue[5..];
        assert!(queue.len() > 16);
    }

    finish_states
}

fn twoline_edges(
    edges: &mut edges::Edges,
    twoline_db: &TwoLineDb,
    hold: Option<Piece>,
    queue: &[Piece],
) {
    hold_sequences(5, hold, queue, |extra_hold_cost, hold, seq| {
        let seq: [_; 5] = seq.try_into().unwrap();
        for entry in twoline_db.query(seq) {
            edges.add_edge(edges::Edge {
                score: entry.score,
                time: entry.time + extra_hold_cost,
                hold,
                b2b: false,
                valid_b2b: true,
                valid_nob2b: true,
                placements: PlacementSet::FivePiece(entry.placements),
            });
        }
    });
}

fn fourline_edges(
    edges: &mut edges::Edges,
    fourline_db: &FourLineDb,
    hold: Option<Piece>,
    queue: &[Piece],
) {
    hold_sequences(10, hold, queue, |extra_hold_cost, hold, seq| {
        let seq: [_; 10] = seq.try_into().unwrap();
        for entry in fourline_db.query(seq) {
            edges.add_edge(edges::Edge {
                score: entry.score,
                time: entry.time + extra_hold_cost,
                hold,
                b2b: entry.end_b2b,
                valid_b2b: entry.valid_b2b,
                valid_nob2b: entry.valid_nob2b,
                placements: PlacementSet::TenPiece(entry.index, seq),
            });
        }
    });
}

fn advance_edges(
    finish_states: &mut Archive<SolveState>,
    layers: &mut VecDeque<Layer>,
    starts: &Archive<SolveState>,
    edges: &edges::Edges,
) {
    while layers.len() < 3 {
        layers.push_back(Layer::default());
    }

    for start in starts.iter() {
        let mut had_successor = false;
        for edge in edges.get_edges(start.b2b) {
            if start.time + edge.time > ULTRA_LENGTH {
                continue;
            }
            had_successor = true;

            let placement_sequence = PlacementSequenceList {
                next: start.placement_sequence.clone(),
                placements: edge.placements,
            };
            let result_state = SolveState {
                score: start.score + edge.score,
                time: start.time + edge.time,
                placement_sequence: Some(Rc::new(placement_sequence)),
                b2b: edge.b2b,
            };

            let layer_idx = match edge.placements {
                PlacementSet::FivePiece(_) => 0,
                PlacementSet::TenPiece(_, _) => 1,
                // PlacementSet::FifteenPiece(_) => 2,
            };
            let layer = &mut layers[layer_idx];
            let archive = match edge.hold {
                None => &mut layer.empty_hold,
                Some(p) => &mut layer.hold_piece[p as usize],
            };

            archive.add(result_state);
        }

        if !had_successor {
            finish_states.add(start.clone());
        }
    }
}

fn hold_sequences(
    target_pieces: usize,
    hold: Option<Piece>,
    queue: &[Piece],
    each: impl FnMut(u32, Option<Piece>, &[Piece]),
) {
    hold_seq_impl(
        target_pieces,
        0,
        hold,
        queue,
        &mut Vec::with_capacity(target_pieces),
        &mut { each },
    );
}

fn hold_seq_impl(
    target_pieces: usize,
    hold_cost: u32,
    hold: Option<Piece>,
    queue: &[Piece],
    so_far: &mut Vec<Piece>,
    each: &mut impl FnMut(u32, Option<Piece>, &[Piece]),
) {
    if so_far.len() == target_pieces {
        each(hold_cost, hold, so_far);
        return;
    }

    let (&next, rest) = queue.split_first().unwrap();

    so_far.push(next);
    hold_seq_impl(target_pieces, hold_cost, hold, rest, so_far, each);
    so_far.pop();

    match hold {
        None => {
            let (&after, rest) = rest.split_first().unwrap();
            so_far.push(after);
            hold_seq_impl(
                target_pieces,
                hold_cost + 1 + SPAWN_DELAY,
                Some(next),
                rest,
                so_far,
                each,
            );
            so_far.pop();
        }
        Some(hold) if hold != next => {
            so_far.push(hold);
            hold_seq_impl(target_pieces, hold_cost + 1, Some(next), rest, so_far, each);
            so_far.pop();
        }
        _ => {}
    }
}

fn gen_queue_ppt1(mut seed: u32) -> Vec<Piece> {
    let mut rng = || {
        seed = seed.wrapping_mul(0x5D588B65).wrapping_add(0x269EC3);
        seed
    };
    for _ in 0..1973 {
        rng();
    }

    let mut result = Vec::with_capacity(1007);
    while result.len() < 1000 {
        let mut bag = [
            Piece::S,
            Piece::Z,
            Piece::J,
            Piece::L,
            Piece::T,
            Piece::O,
            Piece::I,
        ];

        for i in 0..7 {
            let j = ((rng() >> 16) * (7 - i) >> 16) + i;
            bag.swap(i as usize, j as usize);
        }

        result.extend_from_slice(&bag);
    }
    result
}
