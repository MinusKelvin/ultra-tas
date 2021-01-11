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
        // ISZ pieces are hard dropped to the right fastest by using the east rotation instead of
        // the west rotation (which we consider canonical for lock location). We convert the target
        // piece location to the east variant for these cases.
        let piece = if piece.rotation == Rotation::West
            && matches!(piece.piece, Piece::I | Piece::S | Piece::Z)
            && piece.x >= 5
        {
            SrsPiece {
                rotation: Rotation::East,
                x: if piece.piece != Piece::I {
                    piece.x - 1
                } else {
                    piece.x
                },
                y: if piece.piece == Piece::I {
                    piece.y + 1
                } else {
                    piece.y
                },
                ..piece
            }
        } else {
            piece
        };
        let (mut manuever, distance) = piece_manuever(piece);
        manuever.last_mut().unwrap().insert(Input::HardDrop);
        (manuever, distance * 2)
    } else {
        todo!()
    }
}

fn piece_manuever(target: SrsPiece) -> (Vec<EnumSet<Input>>, u32) {
    let mut drop_distance = 19 - target.y as u32;

    let mut dx = (target.x - 4).abs();
    if target.piece == Piece::I {
        match target.rotation {
            Rotation::North | Rotation::West => {}
            Rotation::East | Rotation::South => dx -= 1,
        }
        if matches!(target.rotation, Rotation::West | Rotation::South) {
            drop_distance -= 1;
        }
    }
    let dr = match target.rotation {
        Rotation::West => -1i32,
        Rotation::North => 0,
        Rotation::East => 1,
        Rotation::South => 2,
    };
    let movement = if dx < 0 { Input::Left } else { Input::Right };
    let rotation = if dr < 0 {
        Input::RotateLeft
    } else {
        Input::RotateRight
    };
    if dx.abs() >= dr.abs() {
        let mut movements = [enum_set!(movement), enum_set!()].repeat(dx.abs() as usize);
        if movements.is_empty() {
            movements.push(enum_set!());
        }
        for i in 0..dr.abs() as usize {
            movements[2 * i + 1].insert(rotation);
        }
        (movements, drop_distance)
    } else if dr == 2 {
        let mut movements = vec![
            enum_set!(rotation),
            enum_set!(),
            enum_set!(rotation),
        ];
        if dx != 0 {
            movements[1].insert(movement);
        }
        (movements, drop_distance)
    } else {
        (vec![enum_set!(rotation)], drop_distance)
    }
}
