use std::collections::VecDeque;
use std::convert::TryInto;
use std::io::Write;
use std::rc::Rc;

use arrayvec::ArrayVec;
use enumset::EnumSet;
use structopt::StructOpt;

use crate::archive::{Archive, Dominance};
use crate::data::{line_clear_delay, Board, Piece, Placement, SPAWN_DELAY};
use crate::fourline::FourLineDb;
use crate::pathfind::{pathfind, Input};
use crate::twoline::TwoLineDb;

const NUM_SEEDS: u32 = 1 << 16;
const ULTRA_LENGTH: u32 = 10803;
const WORKERS: u32 = 1;

#[derive(StructOpt)]
pub struct Options {}

impl Options {
    pub fn run(self) {
        let handles: Vec<_> = (0..WORKERS)
            .map(|base| {
                std::thread::spawn(move || {
                    for seed in (base..NUM_SEEDS).step_by(WORKERS as usize) {
                        let queue = gen_queue_ppt1(seed);
                        let mut finish_states: Vec<_> = solve_sequence(&queue).into();

                        let mut max = finish_states.pop().unwrap();
                        for state in finish_states {
                            if state.score > max.score {
                                max = state;
                            }
                        }

                        let mut pieces = vec![];
                        let mut seq = max.placement_sequence.as_ref();
                        while let Some(s) = seq {
                            pieces.push(&s.placements);
                            seq = s.next.as_ref();
                        }
                        pieces.reverse();
                        let placement_sequence: Vec<_> =
                            pieces.into_iter().flatten().copied().collect();

                        let result = input_sequence(&placement_sequence, &queue);
                        println!(
                            "{:04X}: {} points in {} frames ({})",
                            seed,
                            max.score,
                            max.time,
                            result.len()
                        );
                        write_tas(seed, &result[2..]);

                        break;
                    }
                })
            })
            .collect();
        for handle in handles {
            handle.join().unwrap();
        }
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
    placements: ArrayVec<(Placement, Option<u32>), 15>,
}

impl Dominance for SolveState {
    fn covers(&self, other: &Self) -> bool {
        self.score >= other.score && self.time <= other.time && self.b2b >= other.b2b
    }

    type Dim = u32;
    fn get_ascending_dim(&self) -> Self::Dim {
        self.score
    }
}

#[derive(Default)]
struct Layer {
    hold_piece: [Archive<SolveState>; 7],
    empty_hold: Archive<SolveState>,
}

fn input_sequence(
    placements: &[(Placement, Option<u32>)],
    queue: &[Piece],
) -> Vec<(EnumSet<Input>, Option<u32>)> {
    let mut inputs = vec![];
    let mut queue = queue;
    let mut has_done_hold = false;
    let mut board = Board([0; 10]);
    for &(placement, score) in placements {
        inputs.extend_from_slice(&[(EnumSet::empty(), None); SPAWN_DELAY as usize]);
        if placement.piece != queue[0] {
            inputs.push((EnumSet::only(Input::Hold), None));
            if !has_done_hold {
                has_done_hold = true;
                inputs.extend_from_slice(&[(EnumSet::empty(), None); SPAWN_DELAY as usize]);
                queue = &queue[1..];
            }
        }
        queue = &queue[1..];
        inputs.extend(
            pathfind(&board, placement)
                .unwrap()
                .1
                .into_iter()
                .map(|i| (i, None)),
        );
        inputs.last_mut().unwrap().1 = score;
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
            inputs.push((EnumSet::empty(), None));
        }
    }
    inputs
}

fn write_tas(seed: u32, inputs: &[(EnumSet<Input>, Option<u32>)]) {
    let mut file = std::io::BufWriter::new(std::fs::File::create(format!("{:04X}", seed)).unwrap());

    file.write(format!("{:04X}\n", seed).as_bytes()).unwrap();
    for &(frame, score) in inputs {
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
        if let Some(score) = score {
            file.write_all(b" ").unwrap();
            file.write_all(format!("{}", score).as_bytes()).unwrap();
        }
        file.write_all(b"\n").unwrap();
    }
}

fn solve_sequence(queue: &[Piece]) -> Archive<SolveState> {
    let twoline_db = TwoLineDb::gen();

    let mut fourline_db = FourLineDb::open();

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
            let mut edges = twoline_edges(&twoline_db, Some(p), &queue[1..]);
            edges.append(&mut fourline_edges(&mut fourline_db, Some(p), &queue[1..]));
            advance_edges(&mut finish_states, &mut layers, starts, &edges);
        }
        if !layer.empty_hold.is_empty() {
            let mut edges = twoline_edges(&twoline_db, None, queue);
            edges.append(&mut fourline_edges(&mut fourline_db, None, queue));
            advance_edges(&mut finish_states, &mut layers, &layer.empty_hold, &edges);
        }

        queue = &queue[5..];
        assert!(queue.len() > 16);

        let mut shortest_entry = 100000;
        let mut count = 0;
        for entry in layer
            .hold_piece
            .iter()
            .flat_map(|i| i.iter())
            .chain(layer.empty_hold.iter())
        {
            shortest_entry = shortest_entry.min(entry.time);
            count += 1;
        }
        if shortest_entry != 100000 {
            println!(
                "{:.0}%, {} entries",
                shortest_entry as f64 * 100.0 / ULTRA_LENGTH as f64,
                count
            );
        }
    }

    finish_states
}

struct Edge {
    score: u32,
    time: u32,
    b2b: bool,
    valid_b2b: bool,
    valid_nob2b: bool,
    hold: Option<Piece>,
    placements: ArrayVec<Placement, 15>,
}

fn twoline_edges(twoline_db: &TwoLineDb, hold: Option<Piece>, queue: &[Piece]) -> Vec<Edge> {
    let mut edges = vec![];
    hold_sequences(5, hold, queue, |extra_hold_cost, hold, seq| {
        let seq: [_; 5] = seq.try_into().unwrap();
        for entry in twoline_db.query(seq) {
            edges.push(Edge {
                score: entry.score,
                time: entry.time + extra_hold_cost,
                hold,
                b2b: false,
                valid_b2b: true,
                valid_nob2b: true,
                placements: entry.placements.iter().copied().collect(),
            });
        }
    });
    edges
}

fn fourline_edges(fourline_db: &mut FourLineDb, hold: Option<Piece>, queue: &[Piece]) -> Vec<Edge> {
    let mut edges = vec![];
    hold_sequences(10, hold, queue, |extra_hold_cost, hold, seq| {
        let seq: [_; 10] = seq.try_into().unwrap();
        for entry in fourline_db.query(seq) {
            edges.push(Edge {
                score: entry.score,
                time: entry.time + extra_hold_cost,
                hold,
                b2b: entry.end_b2b,
                valid_b2b: entry.valid_b2b,
                valid_nob2b: entry.valid_nob2b,
                placements: entry.placements.iter().copied().collect(),
            });
        }
    });
    edges
}

fn advance_edges(
    finish_states: &mut Archive<SolveState>,
    layers: &mut VecDeque<Layer>,
    starts: &Archive<SolveState>,
    edges: &[Edge],
) {
    while layers.len() < 3 {
        layers.push_back(Layer::default());
    }

    for start in starts.iter() {
        let mut had_successor = false;
        for edge in edges {
            if start.time + edge.time > ULTRA_LENGTH {
                continue;
            }
            if start.b2b && !edge.valid_b2b {
                continue;
            } else if !start.b2b && !edge.valid_nob2b {
                continue;
            }
            had_successor = true;

            let mut placement_sequence = PlacementSequenceList {
                next: start.placement_sequence.clone(),
                placements: edge.placements.iter().map(|&p| (p, None)).collect(),
            };
            placement_sequence.placements.last_mut().unwrap().1 = Some(start.score + edge.score);
            let result_state = SolveState {
                score: start.score + edge.score,
                time: start.time + edge.time,
                placement_sequence: Some(Rc::new(placement_sequence)),
                b2b: edge.b2b,
            };

            let layer_idx = match edge.placements.len() {
                5 => 0,
                10 => 1,
                15 => 2,
                _ => unreachable!(),
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
