use std::ops::Range;

use enumset::{EnumSet, EnumSetType};
use serde::{Deserialize, Serialize};

use crate::ArrayExt;

#[derive(Debug, EnumSetType, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Piece {
    I,
    O,
    T,
    L,
    J,
    S,
    Z,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Rotation {
    North,
    East,
    South,
    West,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Placement {
    pub piece: Piece,
    pub rotation: Rotation,
    pub x: i8,
    pub y: i8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BagState {
    pub hold: Piece,
    pub bag: EnumSet<Piece>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Board(pub [u8; 10]);

impl Piece {
    pub const ALL: [Piece; 7] = [
        Piece::I,
        Piece::O,
        Piece::T,
        Piece::L,
        Piece::J,
        Piece::S,
        Piece::Z,
    ];

    #[inline(always)]
    pub const fn cells(self) -> [(i8, i8); 4] {
        match self {
            Piece::I => [(-1, 0), (0, 0), (1, 0), (2, 0)],
            Piece::J => [(-1, 1), (-1, 0), (0, 0), (1, 0)],
            Piece::L => [(-1, 0), (0, 0), (1, 0), (1, 1)],
            Piece::O => [(0, 0), (1, 0), (1, 1), (0, 1)],
            Piece::S => [(-1, 0), (0, 0), (0, 1), (1, 1)],
            Piece::T => [(-1, 0), (0, 0), (1, 0), (0, 1)],
            Piece::Z => [(-1, 1), (0, 1), (0, 0), (1, 0)],
        }
    }

    #[inline(always)]
    pub const fn relevant_rotations(self) -> &'static [Rotation] {
        match self {
            Piece::I | Piece::S | Piece::Z => &[Rotation::North, Rotation::East],
            Piece::O => &[Rotation::North],
            _ => &[
                Rotation::North,
                Rotation::East,
                Rotation::South,
                Rotation::West,
            ],
        }
    }
}

impl Rotation {
    #[inline(always)]
    pub const fn rotate_cell(self, (x, y): (i8, i8)) -> (i8, i8) {
        match self {
            Rotation::North => (x, y),
            Rotation::East => (y, -x),
            Rotation::South => (-x, -y),
            Rotation::West => (-y, x),
        }
    }
}

impl Placement {
    #[inline(always)]
    pub const fn valid_x_span(self) -> Range<i8> {
        match self.piece {
            Piece::I => match self.rotation {
                Rotation::North => 1..8,
                Rotation::West | Rotation::East => 0..10,
                Rotation::South => 2..9,
            },
            Piece::O => match self.rotation {
                Rotation::North | Rotation::East => 0..9,
                Rotation::South | Rotation::West => 1..10,
            },
            _ => match self.rotation {
                Rotation::North | Rotation::South => 1..9,
                Rotation::East => 0..9,
                Rotation::West => 1..10,
            },
        }
    }

    #[inline(always)]
    pub fn cells(self) -> [(i8, i8); 4] {
        self.piece
            .cells()
            .amap(|c| self.rotation.rotate_cell(c))
            .amap(|(x, y)| (x + self.x, y + self.y))
    }

    #[inline(always)]
    pub fn drop(&mut self, b: &Board) {
        self.y = 0;
        self.y = self
            .cells()
            .iter()
            .map(|&(x, y)| b.column_height(x as usize) - y)
            .max()
            .unwrap()
    }

    /// Get the other representation of this location, if it exists.
    pub fn other(self) -> Option<Placement> {
        Some(match (self.piece, self.rotation) {
            (Piece::I, Rotation::North) => Placement {
                rotation: Rotation::South,
                x: self.x + 1,
                ..self
            },
            (Piece::I, Rotation::South) => Placement {
                rotation: Rotation::North,
                x: self.x - 1,
                ..self
            },
            (Piece::I, Rotation::East) => Placement {
                rotation: Rotation::West,
                y: self.y + 1,
                ..self
            },
            (Piece::I, Rotation::West) => Placement {
                rotation: Rotation::East,
                y: self.y - 1,
                ..self
            },
            (Piece::S | Piece::Z, Rotation::North) => Placement {
                rotation: Rotation::South,
                y: self.y + 1,
                ..self
            },
            (Piece::S | Piece::Z, Rotation::South) => Placement {
                rotation: Rotation::North,
                y: self.y - 1,
                ..self
            },
            (Piece::S | Piece::Z, Rotation::East) => Placement {
                rotation: Rotation::West,
                x: self.x + 1,
                ..self
            },
            (Piece::S | Piece::Z, Rotation::West) => Placement {
                rotation: Rotation::East,
                x: self.x - 1,
                ..self
            },
            (Piece::O | Piece::L | Piece::J | Piece::T, _) => return None,
        })
    }
}

impl BagState {
    #[inline(always)]
    pub fn remove(mut self, p: Piece) -> Option<Self> {
        if p == self.hold {
            self.hold = self.bag.iter().next().unwrap();
            self.bag.remove(self.hold);
            if self.bag.is_empty() {
                self.bag = EnumSet::all();
            }
            Some(self)
        } else if self.bag.remove(p) {
            if self.bag.is_empty() {
                self.bag = EnumSet::all();
            }
            Some(self)
        } else {
            None
        }
    }
}

impl Board {
    #[inline(always)]
    pub fn line_clears(self) -> u8 {
        self.0.iter().fold(!0, |a, &b| a & b)
    }

    #[inline(always)]
    pub fn column_height(self, c: usize) -> i8 {
        8 - self.0[c].leading_zeros() as i8
    }

    #[inline(always)]
    pub fn fill(&mut self, (x, y): (i8, i8)) {
        self.0[x as usize] |= 1 << y;
    }

    #[inline(always)]
    pub fn contains_holes(self) -> bool {
        self.0
            .iter()
            .any(|&c| (!0u8 as u16 >> c.leading_zeros()) as u8 & !c != 0)
    }
}
