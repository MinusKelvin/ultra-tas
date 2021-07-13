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

    pub const fn cw(self) -> Self {
        match self {
            Rotation::North => Rotation::East,
            Rotation::East => Rotation::South,
            Rotation::South => Rotation::West,
            Rotation::West => Rotation::North,
        }
    }

    pub const fn ccw(self) -> Self {
        self.cw().cw().cw()
    }
}

impl Placement {
    pub fn pack(self) -> u8 {
        (self.x as u8 + self.y as u8 * 10) << 2 | self.rotation as u8
    }

    pub fn unpack(packed: u8, piece: Piece) -> Self {
        Placement {
            piece,
            rotation: match packed & 3 {
                0 => Rotation::North,
                1 => Rotation::East,
                2 => Rotation::South,
                3 => Rotation::West,
                _ => unreachable!()
            },
            x: ((packed >> 2) % 10) as i8,
            y: ((packed >> 2) / 10) as i8,
        }
    }

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
        const fn rotate(cells: [(i8, i8); 4], r: Rotation) -> [(i8, i8); 4] {
            [
                r.rotate_cell(cells[0]),
                r.rotate_cell(cells[1]),
                r.rotate_cell(cells[2]),
                r.rotate_cell(cells[3]),
            ]
        }
        const fn rotations(piece: Piece) -> [[(i8, i8); 4]; 4] {
            [
                rotate(piece.cells(), Rotation::North),
                rotate(piece.cells(), Rotation::East),
                rotate(piece.cells(), Rotation::South),
                rotate(piece.cells(), Rotation::West),
            ]
        }
        const LUT: [[[(i8, i8); 4]; 4]; 7] = [
            rotations(Piece::I),
            rotations(Piece::O),
            rotations(Piece::T),
            rotations(Piece::L),
            rotations(Piece::J),
            rotations(Piece::S),
            rotations(Piece::Z),
        ];
        LUT[self.piece as usize][self.rotation as usize].amap(|(x, y)| (x + self.x, y + self.y))
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

    pub fn obstructed(self, b: &Board) -> bool {
        for (x, y) in self.cells() {
            if x < 0 || x >= 10 || y < 0 {
                return true;
            }
            if b.0[x as usize] & 1 << y != 0 {
                return true;
            }
        }
        false
    }

    pub fn kicks(self, to: Rotation) -> [(i8, i8); 5] {
        let offsets_1 = self.offsets();
        let offsets_2 = Placement {
            rotation: to,
            ..self
        }
        .offsets();
        offsets_1.azip(offsets_2, |(x1, y1), (x2, y2)| (x1 - x2, y1 - y2))
    }

    fn offsets(self) -> [(i8, i8); 5] {
        match self.piece {
            Piece::O => match self.rotation {
                Rotation::North => [(0, 0); 5],
                Rotation::East => [(0, -1); 5],
                Rotation::South => [(-1, -1); 5],
                Rotation::West => [(-1, 0); 5],
            },
            Piece::I => match self.rotation {
                Rotation::North => [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)],
                Rotation::East => [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)],
                Rotation::South => [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)],
                Rotation::West => [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)],
            },
            _ => match self.rotation {
                Rotation::North => [(0, 0); 5],
                Rotation::East => [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],
                Rotation::South => [(0, 0); 5],
                Rotation::West => [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)],
            },
        }
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

    #[inline(always)]
    pub fn is_filled(self, (x, y): (i8, i8)) -> bool {
        if x < 0 || x >= 10 || y < 0 {
            true
        } else if y >= 8 {
            false
        } else {
            self.0[x as usize] & 1 << y != 0
        }
    }

    #[cfg(test)]
    pub(crate) fn from_natural(rows: &[[bool; 10]]) -> Self {
        let mut this = Board([0; 10]);
        for row in rows {
            for (x, &filled) in row.iter().enumerate() {
                this.0[x] <<= 1;
                if filled {
                    this.0[x] |= 1;
                }
            }
        }
        this
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Spin {
    Nope,
    Mini,
    Full,
}

pub const SPAWN_DELAY: u32 = 7;

pub fn line_clear_delay(lines_cleared: u32, perfect_clear: bool) -> u32 {
    if perfect_clear {
        1
    } else {
        match lines_cleared {
            0 => 0,
            1 => 36,
            2 | 3 => 41,
            4 => 46,
            _ => unreachable!(),
        }
    }
}

pub fn line_clear_score(lines_cleared: u32, perfect_clear: bool, b2b: bool, spin: Spin) -> u32 {
    if perfect_clear {
        match lines_cleared {
            1 => 900,
            2 => 1500,
            3 => 2300,
            4 if !b2b => 2800,
            4 if b2b => 4400,
            _ => unreachable!(),
        }
    } else {
        match (lines_cleared, spin, b2b) {
            (0, Spin::Nope, _) => 0,
            (0, Spin::Mini, _) => 100,
            (0, Spin::Full, _) => 400,
            (1, Spin::Nope, _) => 100,
            (1, Spin::Mini, false) => 200,
            (1, Spin::Mini, true) => 300,
            (1, Spin::Full, false) => 800,
            (1, Spin::Full, true) => 1200,
            (2, Spin::Nope, _) => 300,
            (2, Spin::Mini, false) => 400,
            (2, Spin::Mini, true) => 600,
            (2, Spin::Full, false) => 1200,
            (2, Spin::Full, true) => 1800,
            (3, Spin::Nope, _) => 500,
            (3, Spin::Full, false) => 1600,
            (3, Spin::Full, true) => 2400,
            (4, Spin::Nope, false) => 800,
            (4, Spin::Nope, true) => 1200,
            _ => unreachable!(),
        }
    }
}
