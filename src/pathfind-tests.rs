use enumset::EnumSet;

use super::pathfind;
use super::Input;
use crate::data::*;

#[test]
fn check_hard_drops() {
    let empty = Board([0; 10]);
    assert_eq!(
        pathfind(
            &empty,
            Placement {
                piece: Piece::O,
                rotation: Rotation::North,
                x: 2,
                y: 0,
            }
        ),
        Some((
            38,
            vec![
                EnumSet::only(Input::Left),
                EnumSet::empty(),
                EnumSet::only(Input::Left),
                EnumSet::only(Input::HardDrop)
            ]
        ))
    );
    assert_eq!(
        pathfind(
            &empty,
            Placement {
                piece: Piece::T,
                rotation: Rotation::South,
                x: 4,
                y: 1,
            }
        ),
        Some((
            36,
            vec![
                EnumSet::only(Input::Cw),
                EnumSet::empty(),
                Input::Cw | Input::HardDrop
            ]
        ))
    );
    assert_eq!(
        pathfind(
            &empty,
            Placement {
                piece: Piece::S,
                rotation: Rotation::East,
                x: 4,
                y: 1,
            }
        ),
        Some((36, vec![Input::Cw | Input::HardDrop]))
    );
    assert_eq!(
        pathfind(
            &empty,
            Placement {
                piece: Piece::S,
                rotation: Rotation::East,
                x: 3,
                y: 1,
            }
        ),
        Some((36, vec![Input::Ccw | Input::HardDrop]))
    );
}

#[test]
fn check_tucks() {
    #[rustfmt::skip]
    let blocking = Board::from_natural(&[
        [true,  true,  true,  true,  false, false, false, false, false, false],
        [true,  true,  true,  true,  false, false, false, false, false, false],
        [false, false, false, false, false, false, false, false, false, false],
        [false, false, false, false, false, false, false, false, false, false],
    ]);
    assert_eq!(
        pathfind(
            &blocking,
            Placement {
                piece: Piece::O,
                rotation: Rotation::North,
                x: 0,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((19, 19 * 3 + 4 * 2))
    );
    assert_eq!(
        pathfind(
            &blocking,
            Placement {
                piece: Piece::L,
                rotation: Rotation::North,
                x: 1,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((19, 19 * 3 + 3 * 2))
    );

    let blocking = Board([0, 0, 0, 0, 0, 0, 0, 0, 0, 0b1000]);
    assert_eq!(
        pathfind(
            &blocking,
            Placement {
                piece: Piece::J,
                rotation: Rotation::West,
                x: 9,
                y: 1
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 5 * 2))
    );
}

#[test]
fn spins() {
    #[rustfmt::skip]
    let spin_board = Board::from_natural(&[
        [true,  false, false, false, true,  false, false, false, true,  false],
        [false, false, true,  false, false, false, false, false, false, false],
    ]);

    assert_eq!(
        pathfind(
            &spin_board,
            Placement {
                piece: Piece::S,
                rotation: Rotation::North,
                x: 1,
                y: 0,
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((17, 17 * 3 + 4 * 2 + 1))
    );

    assert_eq!(
        pathfind(
            &spin_board,
            Placement {
                piece: Piece::Z,
                rotation: Rotation::North,
                x: 3,
                y: 0,
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((17, 17 * 3 + 1))
    );

    assert_eq!(
        pathfind(
            &spin_board,
            Placement {
                piece: Piece::L,
                rotation: Rotation::North,
                x: 8,
                y: 0,
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 3 * 2))
    );

    assert_eq!(
        pathfind(
            &spin_board,
            Placement {
                piece: Piece::J,
                rotation: Rotation::North,
                x: 4,
                y: 0,
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 1 * 2 + 1))
    );
}

#[test]
fn tst() {
    #[rustfmt::skip]
    let board = Board::from_natural(&[
        [false, false, false, false, false, false, false, false, true,  true],
        [false, false, false, false, false, false, false, false, false, true],
        [false, false, false, false, false, false, true,  true,  false, true],
        [false, false, false, false, false, false, true,  false, false, true],
        [false, false, false, false, false, false, true,  true,  false, true],
    ]);

    assert_eq!(
        pathfind(
            &board,
            Placement {
                piece: Piece::T,
                rotation: Rotation::West,
                x: 8,
                y: 1
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((16, 16 * 3 + 3 * 2))
    );
}

#[test]
fn same_frame_spin_tuck() {
    #[rustfmt::skip]
    let blocking = Board::from_natural(&[
        [true,  true,  true,  true,  false, false, false, false, false, false],
        [true,  true,  true,  true,  false, false, false, false, false, false],
        [false, false, false, false, false, false, false, false, false, false],
        [false, false, false, false, false, false, false, false, false, false],
    ]);
    assert_eq!(
        pathfind(
            &blocking,
            Placement {
                piece: Piece::L,
                rotation: Rotation::South,
                x: 1,
                y: 1
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 3 * 2))
    );
}

#[test]
fn intermediate_drop() {
    #[rustfmt::skip]
    let board = Board::from_natural(&[
        [false, false, false, false, false, true,  true,  true,  false, false],
        [false, false, false, false, false, true,  false, false, false, false],
        [false, false, false, false, false, true,  false, false, false, true],
    ]);
    assert_eq!(
        pathfind(
            &board,
            Placement {
                piece: Piece::S,
                rotation: Rotation::North,
                x: 8,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((2 + 16, 16 * 3 + 3 * 2 + 1))
    );
    assert_eq!(
        pathfind(
            &board,
            Placement {
                piece: Piece::S,
                rotation: Rotation::North,
                x: 7,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((17, 17 * 3 + 4 * 2 + 1))
    );
}

#[test]
fn double_rotate() {
    #[rustfmt::skip]
    let board = Board::from_natural(&[
        [true,  false, false, false, false, false, false, false, false, false],
        [false, false, false, false, false, false, false, false, false, false],
    ]);
    assert_eq!(
        pathfind(
            &board,
            Placement {
                piece: Piece::S,
                rotation: Rotation::North,
                x: 1,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 3 * 2))
    );
    assert_eq!(
        pathfind(
            &board,
            Placement {
                piece: Piece::I,
                rotation: Rotation::North,
                x: 1,
                y: 0
            }
        )
        .map(|(s, v)| (s, v.len())),
        Some((18, 18 * 3 + 3 * 2))
    );
}
