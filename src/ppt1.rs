use arrayvec::ArrayVec;
use enumset::{enum_set, EnumSet};
use pcf::{BitBoard, Piece, Placement, Rotation, SrsPiece};

use crate::Input;

#[derive(Clone)]
pub struct PieceGenerator {
    rng: u32,
    current_bag: ArrayVec<[Piece; 7]>,
}

impl PieceGenerator {
    pub fn new(seed: u32) -> Self {
        let mut this = PieceGenerator {
            rng: seed,
            current_bag: ArrayVec::new(),
        };
        for _ in 0..1973 {
            this.rng();
        }
        this
    }

    fn rng(&mut self) -> u32 {
        self.rng = self.rng.wrapping_mul(0x5D588B65).wrapping_add(0x269EC3);
        self.rng
    }
}

impl Iterator for PieceGenerator {
    type Item = Piece;
    fn next(&mut self) -> Option<Piece> {
        if let Some(piece) = self.current_bag.pop() {
            return Some(piece);
        }

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
            let new_index = (((self.rng() >> 16) * (7 - i)) >> 16) + i;
            bag.swap(i as usize, new_index as usize);
        }

        bag.reverse();
        self.current_bag = ArrayVec::from(bag);

        self.current_bag.pop()
    }
}

impl std::fmt::Debug for PieceGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ppt1::PieceGenerator").finish()
    }
}

pub const ULTRA_LENGTH: u32 = 3 * 60 * 60 + 3;
pub const FIRST_HOLD_TIME: u32 = 7;
pub const PIECE_SPAWN_TIME: u32 = 7;
pub const LINE_CLEAR_DELAY: u32 = 1;

pub const LINE_CLEAR_POINTS: [u32; 5] = [100, 300, 500, 800, 1200];
pub const LINE_CLEAR_PC_POINTS: [u32; 5] = [800, 1200, 1800, 2000, 3200];
pub const LINE_CLEAR_DELAY_EXTRA: [u32; 5] = [35, 40, 40, 45, 45];

pub(crate) fn placement_data(placement: Placement, board: BitBoard) -> (Vec<EnumSet<Input>>, u32) {
    if pcf::placeability::hard_drop_only(board, placement) {
        let piece = placement.srs_piece(board)[0];
        let manuever = match (piece.piece, piece.rotation, piece.x) {
            (_, Rotation::North, 0) => &[
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ][..],
            (_, Rotation::North, 1) => &[
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::North, 2) => &[
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::North, 3) => &[enum_set!(Input::Left), enum_set!(Input::HardDrop)],
            (_, Rotation::North, 4) => &[enum_set!(Input::HardDrop)],
            (_, Rotation::North, 5) => &[enum_set!(Input::Right), enum_set!(Input::HardDrop)],
            (_, Rotation::North, 6) => &[
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::North, 7) => &[
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::North, 8) => &[
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::South, 1) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::South, 2) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::South, 3) => &[
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::South, 4) => &[
                enum_set!(Input::RotateLeft),
                enum_set!(),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::South, 5) => &[
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight | Input::HardDrop),
            ],
            (_, Rotation::South, 6) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight | Input::HardDrop),
            ],
            (_, Rotation::South, 7) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::South, 8) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 0) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 1) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 2) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 3) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateRight | Input::HardDrop),
            ],
            (_, Rotation::East, 4) => &[enum_set!(Input::RotateRight | Input::HardDrop)],
            (_, Rotation::East, 5) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight | Input::HardDrop),
            ],
            (_, Rotation::East, 6) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 7) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::East, 8) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 0) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 1)
            | (Piece::S, Rotation::West, 1)
            | (Piece::Z, Rotation::West, 1) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 2)
            | (Piece::S, Rotation::West, 2)
            | (Piece::Z, Rotation::West, 2) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 3)
            | (Piece::S, Rotation::West, 3)
            | (Piece::Z, Rotation::West,3) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 4)
            | (Piece::S, Rotation::West, 4)
            | (Piece::Z, Rotation::West, 4) => &[enum_set!(Input::RotateLeft | Input::HardDrop)],
            (Piece::I, Rotation::West, 5)
            | (Piece::S, Rotation::West, 5)
            | (Piece::Z, Rotation::West, 5) => &[enum_set!(Input::RotateRight | Input::HardDrop)],
            (Piece::I, Rotation::West, 6)
            | (Piece::S, Rotation::West, 6)
            | (Piece::Z, Rotation::West,6) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight | Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 7)
            | (Piece::S, Rotation::West, 7)
            | (Piece::Z, Rotation::West, 7) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 8)
            | (Piece::S, Rotation::West, 8)
            | (Piece::Z, Rotation::West, 8) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (Piece::I, Rotation::West, 9)
            | (Piece::S, Rotation::West, 9)
            | (Piece::Z, Rotation::West, 9) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateRight),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 1) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 2) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Left),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 3) => &[
                enum_set!(Input::Left),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::West, 4) => &[
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::West, 5) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateLeft | Input::HardDrop),
            ],
            (_, Rotation::West, 6) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 7) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 8) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            (_, Rotation::West, 9) => &[
                enum_set!(Input::Right),
                enum_set!(Input::RotateLeft),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(),
                enum_set!(Input::Right),
                enum_set!(Input::HardDrop),
            ],
            unmatched => todo!("{:?}", unmatched),
        };
        let distance = if piece.rotation == Rotation::North {
            19 - piece.y as u32
        } else if piece.piece == Piece::I {
            18 - piece.y as u32
        } else {
            19 - piece.y as u32
        };
        (manuever.to_vec(), distance * 2)
    } else {
        todo!()
    }
}

pub fn spin_tuck_data(placement: Placement, board: BitBoard) -> (u32, u32) {
    // let mut
    // let piece = placement.srs_piece(board)[0];
    // let cleared_board = board.lines_cleared();
    todo!()
}
