use crate::pathfind::{pathfind, Input};
use crate::{data::*, ArrayExt};

#[derive(Copy, Clone, Debug)]
pub enum B2bStatus {
    Uncertain,
    B2b,
    NoB2b,
}

pub fn find_placement_sequences(
    current: &mut Vec<Placement>,
    board: pcf::BitBoard,
    remaining: &mut Vec<pcf::Placement>,
    found: &mut impl FnMut(&[Placement], [u32; 2], u32, B2bStatus),
    score: [u32; 2],
    time: u32,
    b2b: B2bStatus,
    combo: u32,
    tsd_tetris_only: bool,
    only_t: Option<pcf::Placement>,
    only_i: Option<pcf::Placement>,
) {
    if remaining.is_empty() {
        found(current, score, time, b2b);
        return;
    }
    for i in 0..remaining.len() {
        let placement = remaining[i];
        if !placement.supported_after_clears(board) {
            continue;
        }

        let new_board = board.combine(placement.board());

        let clears = (0..6)
            .filter(|&y| new_board.line_filled(y) && !board.line_filled(y))
            .count();

        if tsd_tetris_only && clears == 0 && !pcf::placeability::hard_drop_only(board, placement) {
            continue;
        }

        if only_t.is_some_and(|p| p == placement) {
            if clears != 2 {
                continue;
            }
        }

        if only_i.is_some_and(|p| p == placement) {
            if new_board != pcf::BitBoard::filled(6) {
                continue;
            }
        }

        let cleared = board.lines_cleared();
        let mut b = Board([0; 10]);
        for y in 0..6 {
            for x in 0..10 {
                if cleared.cell_filled(x, y) {
                    b.0[x] |= 1 << y;
                }
            }
        }

        let place = placement.srs_piece(board)[0].into();
        let info = match evaluate(b, place, b2b, combo, tsd_tetris_only) {
            Some(info) => info,
            None => continue,
        };

        remaining.swap_remove(i);
        current.push(place);

        find_placement_sequences(
            current,
            new_board,
            remaining,
            found,
            score.azip(info.score, |a, b| a + b),
            time + info.time,
            info.b2b,
            info.combo,
            tsd_tetris_only,
            only_t,
            only_i,
        );

        current.pop();
        remaining.push(placement);
        let last_index = remaining.len() - 1;
        remaining.swap(i, last_index);
    }
}

fn evaluate(
    board: Board,
    place: Placement,
    b2b: B2bStatus,
    combo: u32,
    tsd_tetris_only: bool,
) -> Option<PlacementEvaluation> {
    let mut board_placed = board;
    for c in place.cells() {
        board_placed.fill(c);
    }

    let perfect_clear = board_placed.0 == [board_placed.line_clears(); 10];
    let lines_cleared = board_placed.line_clears().count_ones();
    let combo = match lines_cleared == 0 {
        true => 0,
        false => combo + 1,
    };
    let combo_score = (combo.max(1) - 1) * 50;

    if tsd_tetris_only && (lines_cleared == 1 || lines_cleared == 3) {
        return None;
    }
    if tsd_tetris_only && lines_cleared == 4 && !perfect_clear {
        return None;
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

        if mini_corners + other_corners >= 3 {
            if mini_corners == 2 {
                spin = Spin::Full;
            } else {
                spin = Spin::Mini;
            }
        }
    }

    if tsd_tetris_only && lines_cleared == 2 && !matches!(spin, Spin::Full) {
        return None;
    }

    let base_score = movement_score + combo_score;
    let b2b_score = line_clear_score(lines_cleared, perfect_clear, true, spin);
    let nob2b_score = line_clear_score(lines_cleared, perfect_clear, false, spin);

    let score = match b2b {
        B2bStatus::Uncertain => [base_score + nob2b_score, base_score + b2b_score],
        B2bStatus::B2b => [base_score + b2b_score; 2],
        B2bStatus::NoB2b => [base_score + nob2b_score; 2],
    };

    Some(PlacementEvaluation {
        score,
        time: movements.len() as u32 + line_clear_delay(lines_cleared, perfect_clear) + SPAWN_DELAY,
        b2b: match (lines_cleared, spin) {
            (0, _) => b2b,
            (4, _) => B2bStatus::B2b,
            (_, Spin::Nope) => B2bStatus::NoB2b,
            _ => B2bStatus::B2b,
        },
        combo,
    })
}

struct PlacementEvaluation {
    score: [u32; 2],
    time: u32,
    b2b: B2bStatus,
    combo: u32,
}
