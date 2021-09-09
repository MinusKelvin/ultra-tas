use std::collections::VecDeque;
use std::convert::TryInto;
use std::io::Write;

use enumset::EnumSet;
use structopt::StructOpt;

use crate::data::{line_clear_delay, Board, Piece, Placement, SPAWN_DELAY};
use crate::fourline::FourLineDb;
use crate::pathfind::{pathfind, Input};

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
                        let mut finish_states = solve_sequence(&queue);

                        let mut max = finish_states.pop().unwrap();
                        for state in finish_states {
                            if state.score > max.score {
                                max = state;
                            }
                        }
                        let result = input_sequence(&max.placement_sequence, &queue);
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
    placement_sequence: Vec<(Placement, Option<u32>)>,
}

impl SolveState {
    fn dominates(&self, other: &Self) -> bool {
        self.score >= other.score && self.time <= other.time && self.b2b >= other.b2b
    }
}

#[derive(Default)]
struct Layer {
    hold_piece: [Vec<SolveState>; 7],
    empty_hold: Vec<SolveState>,
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

fn solve_sequence(queue: &[Piece]) -> Vec<SolveState> {
    let mut fourline_dbs = [FourLineDb::open(false), FourLineDb::open(true)];

    let mut start_layer = Layer::default();
    start_layer.empty_hold.push(SolveState::default());

    let mut layers = VecDeque::new();
    layers.push_back(start_layer);

    let mut finish_states = vec![];

    let mut queue = queue;

    while let Some(layer) = layers.pop_front() {
        for p in Piece::ALL {
            advance(
                &mut fourline_dbs,
                &mut finish_states,
                &mut layers,
                Some(p),
                &layer.hold_piece[p as usize],
                &queue[1..],
            );
        }
        advance(
            &mut fourline_dbs,
            &mut finish_states,
            &mut layers,
            None,
            &layer.empty_hold,
            queue,
        );

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

fn advance(
    &mut [ref mut normal_db, ref mut b2b_db]: &mut [FourLineDb; 2],
    finish_states: &mut Vec<SolveState>,
    layers: &mut VecDeque<Layer>,
    hold: Option<Piece>,
    starts: &Vec<SolveState>,
    queue: &[Piece],
) {
    if starts.is_empty() {
        return;
    }

    let mut seqs = vec![];
    hold_sequences(10, hold, queue, |cost, hold, seq| {
        seqs.push((cost, hold, seq.try_into().unwrap()))
    });

    while layers.len() < 2 {
        layers.push_back(Layer::default());
    }

    struct EdgeSet<'a> {
        db: &'a mut FourLineDb,
        seqs: &'a [(u32, Option<Piece>, [Piece; 10])],
        cache: Option<Vec<(u32, u32, bool, Option<Piece>, [Placement; 10])>>,
    }
    impl EdgeSet<'_> {
        fn get(&mut self) -> &[(u32, u32, bool, Option<Piece>, [Placement; 10])] {
            if self.cache.is_none() {
                let mut edges = vec![];
                for &(extra_hold_cost, hold, seq) in self.seqs {
                    for entry in self.db.query(seq) {
                        edges.push((
                            entry.score,
                            entry.time + extra_hold_cost,
                            entry.end_b2b,
                            hold,
                            entry.placements,
                        ));
                    }
                }
                self.cache = Some(edges);
            }
            self.cache.as_ref().unwrap()
        }
    }

    let mut edges = [
        EdgeSet {
            db: normal_db,
            seqs: &seqs,
            cache: None,
        },
        EdgeSet {
            db: b2b_db,
            seqs: &seqs,
            cache: None,
        },
    ];

    for start in starts {
        let mut had_successor = false;
        for &(score, time, b2b, hold, placements) in edges[start.b2b as usize].get() {
            if start.time + time > ULTRA_LENGTH {
                continue;
            }
            had_successor = true;

            let mut placement_sequence = start.placement_sequence.clone();
            placement_sequence.extend(placements.iter().map(|&p| (p, None)));
            placement_sequence.last_mut().unwrap().1 = Some(start.score + score);
            let result_state = SolveState {
                score: start.score + score,
                time: start.time + time,
                placement_sequence,
                b2b,
            };

            let list = match hold {
                None => &mut layers[1].empty_hold,
                Some(p) => &mut layers[1].hold_piece[p as usize],
            };

            add_to_archive(list, result_state);
        }

        if !had_successor {
            add_to_archive(finish_states, start.clone());
        }
    }
}

fn add_to_archive(archive: &mut Vec<SolveState>, entry: SolveState) {
    if !archive.iter().any(|e| e.dominates(&entry)) {
        archive.retain(|e| !entry.dominates(e));
        archive.push(entry);
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
