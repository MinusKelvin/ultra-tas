use std::collections::HashSet;

use arrayvec::ArrayVec;
use dashmap::DashMap;
use rayon::Scope;
use serde::{Deserialize, Serialize};

use crate::{ArrayExt, data::*};

#[inline(always)]
fn search_impl(bag_states: &[BagState], b: &Board, mut option: impl FnMut(&[BagState], Placement)) {
    for &piece in &Piece::ALL {
        let bags: Vec<_> = bag_states.iter().filter_map(|&b| b.remove(piece)).collect();
        if !bags.is_empty() {
            for &rotation in piece.relevant_rotations() {
                let mut placement = Placement {
                    piece,
                    rotation,
                    x: 0,
                    y: 0,
                };
                for x in placement.valid_x_span() {
                    placement.x = x;
                    placement.drop(b);
                    option(&bags, placement);
                }
            }
        }
    }
}

fn search_pc(
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    pcs: &DashMap<[Piece; 15], Vec<[Placement; 15]>>,
    partials: &[Vec<Placement>],
) {
    search_impl(bags, board, |bags, placement| {
        let mut board = *board;
        for &cell in &placement.cells() {
            if cell.1 >= 4 {
                return;
            }
            board.fill(cell);
        }

        placements.push(placement);
        if board.line_clears() == 0b1111 {
            for start in partials {
                let mut result: ArrayVec<_, 15> = start.iter().copied().collect();
                result.try_extend_from_slice(placements).unwrap();
                let placements = result.into_inner().unwrap();
                let key = placements.amap(|p| p.piece);
                pcs.entry(key).or_default().push(placements);
            }
        } else if check_pcable(board) {
            search_pc(placements, &board, bags, pcs, partials);
        }
        placements.pop();
    })
}

#[inline(always)]
fn search_tsd(
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    mut op: impl FnMut(&mut Vec<Placement>, Board, &[BagState]),
    mut op_tsd: impl FnMut(&mut Vec<Placement>, Board, &[BagState]),
) {
    search_impl(bags, board, |bags, placement| {
        let mut new_board = *board;
        for &cell in &placement.cells() {
            if cell.1 >= 6 {
                return;
            }
            new_board.fill(cell);
        }

        if is_valid_before_tsd(&new_board) {
            placements.push(placement);
            op(placements, new_board, bags);
            placements.pop();
        } else if is_tspinable(board, placement) {
            let placement = Placement {
                rotation: Rotation::South,
                ..placement
            };

            let mut new_board = *board;
            for &cell in &placement.cells() {
                new_board.fill(cell);
            }

            if new_board.line_clears().count_ones() == 2 {
                let mask = (1 << new_board.line_clears().trailing_zeros()) - 1;
                let new_board = Board(new_board.0.amap(|c| c & mask | (c >> 2) & !mask));

                placements.push(placement);
                op_tsd(placements, new_board, bags);
                placements.pop();
            }
        }
    });
}

fn search_complete_st(
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    pcs: &DashMap<[Piece; 15], Vec<[Placement; 15]>>,
    partials: &[Vec<Placement>],
) {
    search_tsd(
        placements,
        board,
        bags,
        |placements, board, bags| search_complete_st(placements, &board, bags, pcs, partials),
        |placements, board, bags| search_pc(placements, &board, bags, pcs, partials),
    );
}

pub fn search_complete<'s>(
    scope: &Scope<'s>,
    until_st: usize,
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    pcs: &'s DashMap<[Piece; 15], Vec<[Placement; 15]>>,
    partials: &'s [Vec<Placement>],
) {
    if until_st == 0 {
        search_complete_st(placements, board, bags, pcs, partials);
    } else {
        search_tsd(
            placements,
            board,
            bags,
            |placements, board, bags| {
                let bags = bags.to_owned();
                let mut placements = placements.clone();
                scope.spawn(move |scope| {
                    search_complete(
                        scope,
                        until_st - 1,
                        &mut placements,
                        &board,
                        &bags,
                        pcs,
                        partials,
                    )
                });
            },
            |placements, board, bags| {
                let bags = bags.to_owned();
                let mut s = placements.clone();
                scope.spawn(move |_| search_pc(&mut s, &board, &bags, pcs, partials));
            },
        )
    }
}

#[derive(Clone, Serialize, Deserialize, Default)]
pub struct Intermediate {
    pub bags: HashSet<BagState>,
    pub partials: Vec<Vec<Placement>>,
}

fn search_intermediates_st(
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    data: &DashMap<Board, Intermediate>,
) {
    search_tsd(
        placements,
        board,
        bags,
        |placements, board, bags| {
            if placements.len() == 4 {
                let mut guard = data.entry(board).or_default();
                guard.bags.extend(bags.iter().copied());
                guard.partials.push(placements.clone());
            } else {
                search_intermediates_st(placements, &board, bags, data);
            }
        },
        |_, _, _| unreachable!(),
    );
}

pub fn search_intermediates<'s>(
    scope: &Scope<'s>,
    until_st: usize,
    placements: &mut Vec<Placement>,
    board: &Board,
    bags: &[BagState],
    data: &'s DashMap<Board, Intermediate>,
) {
    if until_st == 0 {
        search_intermediates_st(placements, board, bags, data);
    } else {
        search_tsd(
            placements,
            board,
            bags,
            |placements, board, bags| {
                let bags = bags.to_owned();
                let mut placements = placements.to_owned();
                scope.spawn(move |scope| {
                    search_intermediates(scope, until_st - 1, &mut placements, &board, &bags, data)
                })
            },
            |_, _, _| unreachable!(),
        );
    }
}

#[inline(always)]
fn is_valid_before_tsd(board: &Board) -> bool {
    if board.line_clears() != 0 {
        return false;
    }

    for i in 0..4 {
        let mut r = !0u16;
        for j in 0..10 {
            r <<= 1;
            r |= (board.0[j] & 1 << i + 1 != 0) as u16;
        }
        let three_wides = !(r | r >> 1 | r >> 2);
        if three_wides == 0 {
            continue;
        }

        let mask = (1 << i) - 1;
        let cleared_board = Board(board.0.amap(|c| c & mask | (c >> 2) & !mask));
        if !cleared_board.contains_holes() && check_pcable(cleared_board) {
            return true;
        }
    }

    false
}

#[inline(always)]
fn is_tspinable(b: &Board, p: Placement) -> bool {
    let can_spin_from_east = matches!(p.rotation, Rotation::East)
        && p.x != 0
        && b.0[p.x as usize - 1] & 1 << p.y == 0
        && b.0[p.x as usize - 1] & 2 << p.y != 0;
    let can_spin_from_west = matches!(p.rotation, Rotation::West)
        && p.x != 9
        && b.0[p.x as usize + 1] & 1 << p.y == 0
        && b.0[p.x as usize + 1] & 2 << p.y != 0;
    matches!(p.piece, Piece::T) && (can_spin_from_east || can_spin_from_west)
}

#[inline(always)]
fn check_pcable(mut board: Board) -> bool {
    let mut required_tetris_column = None;
    let mut multiple_choices = false;
    for i in 0..10 {
        if board.0[i] == 0 {
            match required_tetris_column {
                Some(c) if c + 1 == i => {
                    required_tetris_column = Some(i);
                }
                None => {
                    required_tetris_column = Some(i);
                }
                _ => {
                    multiple_choices = true;
                }
            }
        }
    }
    match required_tetris_column {
        None => return false,
        Some(i) => {
            if !multiple_choices {
                board.0[i] = 0b1111;
            }
        }
    }

    let mut empties = 0;
    for &c in board.0.iter().chain(std::iter::once(&0b1111)) {
        if c == 0b1111 {
            if empties % 4 != 0 {
                return false;
            }
        } else {
            empties += c.count_zeros();
        }
    }

    true
}
