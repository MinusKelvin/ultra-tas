use std::borrow::Cow;
use std::collections::HashMap;
use std::io::prelude::*;

use arrayvec::ArrayVec;
use enumset::{EnumSet, EnumSetType};
use pcf::{BitBoard, Piece, PieceSet, Placement, Rotation, SrsPiece, PIECES};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

cfg_if::cfg_if! {
    if #[cfg(feature = "ppt2")] {
        mod ppt2;
        use ppt2 as ppt;
    } else {
        mod ppt1;
        use ppt1 as ppt;
    }
}

fn main() {
    let all_pieces = pcf::PIECES.repeat(4).into_iter().collect();

    let combos = std::sync::Mutex::new(Combos::new());

    let start = std::time::Instant::now();
    let cmbs = &combos;
    pcf::find_combinations_mt(all_pieces, BitBoard(0), &Default::default(), 4, |combo| {
        let mut a = ArrayVec::<[_; 10]>::new();
        a.try_extend_from_slice(combo).unwrap();
        let combo = a.into_inner().unwrap();
        let set = combo.iter().map(|p| p.kind.piece()).collect();
        cmbs.lock().unwrap().entry(set).or_default().push(combo);
    });

    let combos = combos.into_inner().unwrap();
    println!(
        "Found {} combos in {:?}",
        combos.values().map(|v| v.len()).sum::<usize>(),
        start.elapsed()
    );

    std::fs::create_dir_all("solutions").unwrap();

    let mut to_do = vec![true; 65536];
    for entry in std::fs::read_dir("solutions").unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name();
        let name = name.to_str().unwrap();
        let seed = name
            .rsplit('-')
            .next()
            .and_then(|v| usize::from_str_radix(v, 16).ok());
        if let Some(seed) = seed {
            to_do[seed] = false;
        }
    }

    let to_do: Vec<_> = to_do
        .into_iter()
        .enumerate()
        .filter_map(|(seed, ntbd)| ntbd.then(|| seed as u32))
        .collect();

    to_do.into_par_iter().for_each(|seed| {
        let t = std::time::Instant::now();
        let set = solve_seed(&combos, seed);
        println!(
            "{:04X} solved in {:?} with {} solutions",
            seed,
            t.elapsed(),
            set.len()
        );
        for soln in &set {
            println!("  {}pts in {} frames", soln.info.points, soln.info.time);
        }

        for soln in set {
            let mut f = std::io::BufWriter::new(
                std::fs::File::create(&format!(
                    "solutions/{:6}-{}-{:04X}",
                    soln.info.points, ppt::ULTRA_LENGTH - soln.info.time, seed
                ))
                .unwrap(),
            );
            writeln!(f, "{:X}", seed).unwrap();
            for &inputs in &soln.inputs[2..] {
                if inputs.is_empty() {
                    write!(f, "_").unwrap();
                }
                for input in inputs {
                    match input {
                        Input::Left => write!(f, "<").unwrap(),
                        Input::Right => write!(f, ">").unwrap(),
                        Input::RotateLeft => write!(f, "L").unwrap(),
                        Input::RotateRight => write!(f, "R").unwrap(),
                        Input::Hold => write!(f, "H").unwrap(),
                        Input::SoftDrop => write!(f, "v").unwrap(),
                        Input::HardDrop => write!(f, "D").unwrap(),
                    }
                }
                writeln!(f).unwrap();
            }
        }
    });
}

type Combos = HashMap<PieceSet, Vec<[Placement; 10]>>;

#[derive(Clone)]
struct PcState {
    placements: Vec<(SrsPiece, u32)>,
    inputs: Vec<EnumSet<Input>>,
    generator: ppt::PieceGenerator,
    pieces: u32,
    info: PcInfo,
}

#[derive(Copy, Clone, Debug)]
struct PcInfo {
    points: u32,
    time: u32,
    b2b: bool,
    reserve: Piece,
    reserve_is_hold: bool,
}

impl PcInfo {
    fn is_worse_or_equal(&self, other: &Self) -> bool {
        if self.reserve_is_hold != other.reserve_is_hold || self.reserve != other.reserve {
            return false;
        }
        if self.points <= other.points && self.time >= other.time {
            return true;
        }
        // We can use the rotate-then-hold trick to gain 2 points from each extra frame,
        // so if we take N extra frames to score less than 2N extra points we're worse.
        if self.time > other.time {
            let extra_points = (self.time - other.time) * 2;
            return self.points <= other.points + extra_points;
        }
        false
    }

    fn final_is_worse_or_equal(&self, other: &Self) -> bool {
        let leftover_time = ppt::ULTRA_LENGTH - self.time;
        let extra_pieces = leftover_time / (ppt::PIECE_SPAWN_TIME + 1);
        let leftover_leftover_time = leftover_time - extra_pieces * (ppt::PIECE_SPAWN_TIME + 1);
        let extra_points = extra_pieces * 38 + leftover_leftover_time * 2;
        self.points + extra_points <= other.points
    }
}

impl PcState {
    fn new(seed: u32) -> Self {
        let mut generator = ppt::PieceGenerator::new(seed);
        let reserve = generator.next().unwrap();
        PcState {
            placements: vec![],
            inputs: vec![],
            generator,
            pieces: 0,
            info: PcInfo {
                points: 0,
                time: 0,
                b2b: false,
                reserve,
                reserve_is_hold: false,
            },
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SolveState {
    queue: [Piece; 11],
    b2b: bool,
}

#[derive(Clone)]
struct SolutionInfo {
    inputs: Vec<EnumSet<Input>>,
    placements: ArrayVec<[(SrsPiece, u32); 10]>,
    info: PcInfo,
}

#[derive(Default)]
struct Cache {
    // old_values: HashSet<SolveState>,
    cached: HashMap<SolveState, Vec<SolutionInfo>>,
}

impl Cache {
    fn get(
        &mut self,
        combos: &Combos,
        queue: [Piece; 11],
        info: &PcInfo,
    ) -> Cow<Vec<SolutionInfo>> {
        if !info.reserve_is_hold {
            // Don't bother caching early game PCs
            Cow::Owned(calculate_branches(
                combos,
                queue,
                info.reserve,
                info.reserve_is_hold,
                info.b2b,
            ))
        } else {
            Cow::Borrowed(
                self.cached
                    .entry(SolveState {
                        queue,
                        b2b: info.b2b,
                    })
                    .or_insert_with(|| {
                        calculate_branches(
                            combos,
                            queue,
                            info.reserve,
                            info.reserve_is_hold,
                            info.b2b,
                        )
                    }),
            )
        }
    }
}

fn advance(cache: &mut Cache, combos: &Combos, state: &PcState, mut f: impl FnMut(PcState)) {
    let mut queue = ArrayVec::<[_; 11]>::new();
    queue.push(state.info.reserve);
    queue.extend(state.generator.clone());
    let queue = queue.into_inner().unwrap();

    let solutions = cache.get(combos, queue, &state.info);

    for soln_info in &*solutions {
        let mut new_state = state.clone();
        (&mut new_state.generator)
            .take(soln_info.placements.len())
            .for_each(|_| {});
        new_state.pieces += soln_info.placements.len() as u32;
        let points = new_state.info.points;
        new_state
            .placements
            .extend(soln_info.placements.iter().map(|&(p, s)| (p, s + points)));
        new_state.info.points += soln_info.info.points;
        new_state.info.time += soln_info.info.time;
        new_state.info.reserve_is_hold = soln_info.info.reserve_is_hold;
        new_state.info.reserve = soln_info.info.reserve;
        new_state.info.b2b = soln_info.info.b2b;
        new_state.inputs.extend_from_slice(&soln_info.inputs);
        f(new_state);
    }
}

fn calculate_branches(
    combos: &Combos,
    queue: [Piece; 11],
    reserve: Piece,
    reserve_is_hold: bool,
    b2b: bool,
) -> Vec<SolutionInfo> {
    let mut branches: Vec<SolutionInfo> = vec![];

    pcf::solve_pc(
        &queue[..6],
        BitBoard(0),
        true,
        false,
        &std::sync::atomic::AtomicBool::new(false),
        // pcf::placeability::simple_srs_spins,
        pcf::placeability::hard_drop_only,
        |pc| {
            let result = score_pc(
                pc,
                queue[1..].iter().copied(),
                reserve,
                reserve_is_hold,
                b2b,
            );
            branches.retain(|soln| !soln.info.is_worse_or_equal(&result.info));
            if !branches
                .iter()
                .any(|soln| result.info.is_worse_or_equal(&soln.info))
            {
                branches.push(result);
            }
        },
    );

    let set: PieceSet = queue.iter().cloned().collect();
    for &piece in &PIECES {
        if !set.contains(piece) {
            continue;
        }
        let set = set.without(piece);

        for combo in combos.get(&set).map(Vec::as_slice).unwrap_or(&[]) {
            pcf::solve_placement_combination(
                &queue,
                BitBoard(0),
                combo,
                true,
                false,
                &std::sync::atomic::AtomicBool::new(false),
                simple_srs_spins,
                // pcf::placeability::simple_srs_spins,
                // pcf::placeability::hard_drop_only,
                |pc| {
                    let result = score_pc(
                        pc,
                        queue[1..].iter().copied(),
                        reserve,
                        reserve_is_hold,
                        b2b,
                    );
                    branches.retain(|soln| !soln.info.is_worse_or_equal(&result.info));
                    if !branches
                        .iter()
                        .any(|soln| result.info.is_worse_or_equal(&soln.info))
                    {
                        branches.push(result);
                    }
                },
            );
        }
    }

    branches
}

fn score_pc(
    pc: &[Placement],
    mut queue: impl Iterator<Item = Piece>,
    reserve: Piece,
    reserve_is_hold: bool,
    b2b: bool,
) -> SolutionInfo {
    let pc_lines = pc.len() / 5 * 2;
    let mut board = BitBoard(0);
    let mut combo = 0;
    let mut info = PcInfo {
        points: 0,
        time: 0,
        reserve,
        reserve_is_hold,
        b2b,
    };
    let mut placements = ArrayVec::new();
    let mut inputs = vec![];
    for &placement in pc {
        let srs = placement.srs_piece(board)[0];

        info.time += ppt::PIECE_SPAWN_TIME;
        inputs.extend_from_slice(&[EnumSet::empty(); ppt::PIECE_SPAWN_TIME as usize]);

        let next = queue.next().unwrap();
        if info.reserve_is_hold {
            if placement.kind.piece() != next {
                assert!(placement.kind.piece() == info.reserve);
                info.reserve = next;
                info.time += 1;
                inputs.push(EnumSet::only(Input::Hold));
            }
        } else {
            if placement.kind.piece() == info.reserve {
                info.reserve = next;
            } else {
                assert!(placement.kind.piece() == next);
                info.reserve_is_hold = true;
                info.time += ppt::FIRST_HOLD_TIME + 1;
                inputs.push(EnumSet::only(Input::Hold));
                inputs.extend_from_slice(&[EnumSet::empty(); ppt::FIRST_HOLD_TIME as usize]);
            }
        }

        let (manuever, movement_score) = ppt::placement_data(placement, board);
        info.points += movement_score;
        info.time += manuever.len() as u32;
        inputs.append(&mut { manuever });

        let cleared_lines = (0..4).filter(|&y| board.line_filled(y)).count();
        board = board.combine(placement.board());
        let total_cleared_lines = (0..4).filter(|&y| board.line_filled(y)).count();
        let cleared_lines = total_cleared_lines - cleared_lines;

        if cleared_lines != 0 {
            info.time += ppt::LINE_CLEAR_DELAY;
            inputs.extend_from_slice(&[EnumSet::empty(); ppt::LINE_CLEAR_DELAY as usize]);
            info.points += combo * 50;

            let i = if cleared_lines == 4 {
                4 + info.b2b as usize
            } else {
                cleared_lines
            } - 1;

            info.points += ppt::LINE_CLEAR_POINTS[i];

            if total_cleared_lines == pc_lines {
                info.points += ppt::LINE_CLEAR_PC_POINTS[i];
            } else {
                info.time += ppt::LINE_CLEAR_DELAY_EXTRA[i];
                for _ in 0..ppt::LINE_CLEAR_DELAY_EXTRA[i] {
                    inputs.push(EnumSet::empty());
                }
            }

            info.b2b = cleared_lines == 4;
            combo += 1;
        } else {
            combo = 0;
        }

        placements.push((srs, info.points));
    }

    SolutionInfo {
        inputs,
        placements,
        info,
    }
}

fn solve_seed(combos: &Combos, seed: u32) -> Vec<PcState> {
    let mut solutions_cache = Cache::default();
    let mut next = (vec![PcState::new(seed)], vec![]);
    let mut endings = vec![];
    while !next.0.is_empty() || !next.1.is_empty() {
        let group = next.0;
        next.0 = next.1;
        next.1 = vec![];
        for state in group {
            let pieces = state.pieces;
            let mut ended = true;
            advance(&mut solutions_cache, combos, &state, |new_state| {
                if new_state.info.time > ppt::ULTRA_LENGTH {
                    return;
                }
                ended = false;
                let set = if new_state.pieces == pieces + 5 {
                    &mut next.0
                } else {
                    &mut next.1
                };
                set.retain(|v| !v.info.is_worse_or_equal(&new_state.info));
                if set
                    .iter()
                    .all(|v| !new_state.info.is_worse_or_equal(&v.info))
                {
                    set.push(new_state);
                }
            });
            if ended {
                endings.retain(|v: &PcState| !v.info.final_is_worse_or_equal(&state.info));
                if endings
                    .iter()
                    .all(|v| !state.info.final_is_worse_or_equal(&v.info))
                {
                    endings.push(state);
                }
            }
        }
    }
    endings
}

#[derive(EnumSetType, Debug)]
enum Input {
    Left,
    Right,
    RotateLeft,
    RotateRight,
    Hold,
    SoftDrop,
    HardDrop,
}

pub fn simple_srs_spins(board: BitBoard, placement: Placement) -> bool {
    if pcf::placeability::hard_drop_only(board, placement) {
        return true;
    }

    let piece = placement.srs_piece(board).into_iter().next().unwrap();
    let board = board.lines_cleared();
    let x = placement.x as usize;
    let y = piece.y as usize;

    let check_empty = |mask: u64| board.0 & mask << 10 * y + x == 0;

    // this is a visible description of all the spins we're detecting:
    // http://fumen.zui.jp/?v115@pgxhHexhIewhReA8cevEn9gwhIexhlenpfpgQaAewh?GeQaAewhGeRawhGeRaAeA8FeAAceflf+gwhIexhkenpuEBU?9UTASIB5DjB98AQWrrDTG98AXO98AwyjXEroo2AseirDFbE?cEoe0TAyE88AQzgeEFbMwDv3STASorJEvwh1DIhRaAAGeA8?beaquAAIhxhkeyufIhRaGeA8AAAeA8ZeaqfIhxhkeyuf+gR?aHeQ4QaGeAABeAAZealf+gxhIewhkeipf+gRaGeA8AAQaA8?jealf+gxhIewhkeipf/gQaHewhQakeelf/gwhIewhkempfH?hAAAeQaAAFeA8BeA8ZedqfJhwhIewhae1ufIhQaJeQaaetp?fIhwhIewhbeVvfIhQaHeAAQaAeAAZetpfIhwhlelpfIhQaJ?ewhae9pfpgwhAeQaGewhAeQaGewhAeQaGewhAeQaIeQaae9?pfIhwhlelpfIhQaHewhcetpf
    // the cyan blocks are the areas check_empty calls check, the gray blocks are blocks
    // that we check to make sure are filled
    #[allow(unused_parens)]
    match (piece.piece, piece.rotation) {
        (Piece::S, Rotation::North) => {
            (check_empty(0b_0000000011_0000000011_0000000011_0000000011_0000000000_0000000000))
                || (check_empty(
                    0b_0000000110_0000000110_0000000110_0000000110_0000000000_0000000000,
                ) && (placement.x == 7 || board.cell_filled(x + 3, y + 2)))
        }
        (Piece::Z, Rotation::North) => {
            (check_empty(0b_0000000110_0000000110_0000000110_0000000110_0000000000_0000000000))
                || (check_empty(
                    0b_0000000011_0000000011_0000000011_0000000011_0000000000_0000000000,
                ) && (placement.x == 0 || board.cell_filled(x - 1, y + 2)))
        }
        (Piece::L, Rotation::North) => {
            (check_empty(0b_0000000110_0000000110_0000000110_0000000110_0000000000_0000000000)
                && (board.cell_filled(x + 1, y + 1)
                    || board.cell_filled(x, y + 1) && (x == 7 || board.cell_filled(x + 3, y + 1))))
        }
        (Piece::J, Rotation::North) => {
            (check_empty(0b_0000000011_0000000011_0000000011_0000000011_0000000000_0000000000)
                && (board.cell_filled(x + 1, y + 1)
                    || board.cell_filled(x + 2, y + 1)
                        && (x == 0 || board.cell_filled(x - 1, y + 1))))
        }
        (Piece::L, Rotation::South) => {
            (check_empty(0b_0000000110_0000000110_0000000110_0000000110_0000000100_0000000000)
                && (board.cell_filled(x + 1, y + 1)
                    || board.cell_filled(x, y + 1) && (x == 7 || board.cell_filled(x + 3, y + 1))))
                || (check_empty(
                    0b_0000000011_0000000011_0000000011_0000000011_0000000011_0000000000,
                ) && board.cell_filled(x + 2, y + 1)
                    && (x == 0 || board.cell_filled(x - 1, y + 1)))
        }
        (Piece::J, Rotation::South) => {
            (check_empty(0b_0000000011_0000000011_0000000011_0000000011_0000000001_0000000000)
                && (board.cell_filled(x + 1, y + 1)
                    || board.cell_filled(x + 2, y + 1)
                        && (x == 0 || board.cell_filled(x - 1, y + 1))))
                || (check_empty(
                    0b_0000000110_0000000110_0000000110_0000000110_0000000110_0000000000,
                ) && board.cell_filled(x, y + 1)
                    && (x == 7 || board.cell_filled(x + 3, y + 1)))
        }
        (Piece::T, Rotation::North) => {
            (check_empty(0b_0000000110_0000000110_0000000110_0000000110_0000000110_0000000110)
                && board.cell_filled(x, y + 1)
                && (x == 7 || board.cell_filled(x + 3, y + 1)))
                || (check_empty(
                    0b_0000000011_0000000011_0000000011_0000000011_0000000011_0000000011,
                ) && board.cell_filled(x + 2, y + 1)
                    && (x == 0 || board.cell_filled(x - 1, y + 1)))
        }
        (Piece::T, Rotation::West) => {
            (x != 8
                && check_empty(
                    0b_0000000110_0000000110_0000000110_0000000110_0000000110_0000000110,
                ))
        }
        (Piece::T, Rotation::East) => {
            (
                x != 0
                    && check_empty(
                        0b_0000000011_0000000011_0000000011_0000000011_0000000011_0000000011 >> 1,
                    )
                    && !board.cell_filled(x - 1, y)
                // due to jank we need to check that last bit manually
            )
        }
        (Piece::T, Rotation::South) => {
            (check_empty(0b_0000000011_0000000011_0000000011_0000000011_0000000011_0000000011))
                || (check_empty(
                    0b_0000000110_0000000110_0000000110_0000000110_0000000110_0000000110,
                ))
        }
        _ => false,
    }
}
