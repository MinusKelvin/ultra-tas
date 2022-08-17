use crate::data::*;
use crate::pathfind::{Input, pathfind};

pub fn find_placement_sequences(
    current: &mut Vec<Placement>,
    board: pcf::BitBoard,
    remaining: &mut Vec<pcf::Placement>,
    found: &mut impl FnMut(&[Placement], u32, u32, bool),
    score: u32,
    time: u32,
    b2b: bool,
    combo: u32,
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
        let info = match evaluate(b, place, b2b, combo) {
            Some(info) => info,
            None => continue,
        };

        let new_board = board.combine(placement.board());

        remaining.swap_remove(i);
        current.push(place);

        find_placement_sequences(
            current,
            new_board,
            remaining,
            found,
            score + info.score,
            time + info.time,
            info.b2b,
            info.combo,
        );

        current.pop();
        remaining.push(placement);
        let last_index = remaining.len() - 1;
        remaining.swap(i, last_index);
    }
}

fn evaluate(
    mut board: Board,
    place: Placement,
    b2b: bool,
    combo: u32,
) -> Option<PlacementEvaluation> {
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

    for c in place.cells() {
        board.fill(c);
    }

    let perfect_clear = board.0 == [board.line_clears(); 10];
    let lines_cleared = board.line_clears().count_ones();
    let combo = match lines_cleared == 0 {
        true => 0,
        false => combo + 1,
    };
    let combo_score = (combo.max(1) - 1) * 50;

    Some(PlacementEvaluation {
        score: movement_score
            + line_clear_score(lines_cleared, perfect_clear, b2b, spin)
            + combo_score,
        time: movements.len() as u32 + line_clear_delay(lines_cleared, perfect_clear) + SPAWN_DELAY,
        b2b: match (lines_cleared, spin) {
            (0, _) => b2b,
            (4, _) => true,
            (_, Spin::Nope) => false,
            _ => true,
        },
        combo,
    })
}

struct PlacementEvaluation {
    score: u32,
    time: u32,
    b2b: bool,
    combo: u32,
}
