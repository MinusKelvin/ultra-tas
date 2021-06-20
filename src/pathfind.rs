use std::collections::{BinaryHeap, HashMap};

use enumset::{EnumSet, EnumSetType};

use crate::data::{Board, Piece, Placement, Rotation};

#[derive(EnumSetType, Debug)]
pub enum Input {
    Left,
    Right,
    Cw,
    Ccw,
    Softdrop,
    HardDrop,
}

pub fn pathfind(board: &Board, placement: Placement) -> Option<Vec<EnumSet<Input>>> {
    let mut best: Option<Vec<_>> = None;
    let mut reverse_paths = HashMap::new();
    let mut queue = BinaryHeap::new();

    reverse_paths.insert(
        placement,
        ByPrevMove {
            left: Some(vec![]),
            right: Some(vec![]),
            cw: Some(vec![]),
            ccw: Some(vec![]),
        },
    );
    queue.push(QueueItem {
        place: placement,
        inputs: 0,
    });

    if let Some(placement) = placement.other() {
        reverse_paths.insert(
            placement,
            ByPrevMove {
                left: Some(vec![]),
                right: Some(vec![]),
                cw: Some(vec![]),
                ccw: Some(vec![]),
            },
        );
        queue.push(QueueItem {
            place: placement,
            inputs: 0,
        });
    }

    let mut update_best = |new: Vec<_>| match &mut best {
        Some(b) if new.len() < b.len() => *b = new,
        None => best = Some(new),
        _ => {}
    };

    while let Some(item) = queue.pop() {
        if let Some(mut path) = above_stack(board, item.place) {
            let rev_path = reverse_paths.get(&item.place).unwrap().shortest();
            if rev_path.is_empty() {
                add_hard_drop(&mut path);
            } else {
                add_soft_drop(&mut path, (19 - item.place.y) as usize);
                path.extend(rev_path.iter().rev());
                add_hard_drop(&mut path);
            }
            update_best(path);
            continue;
        }

        // todo
    }

    best
}

#[derive(Default)]
struct ByPrevMove {
    left: Option<Vec<EnumSet<Input>>>,
    right: Option<Vec<EnumSet<Input>>>,
    cw: Option<Vec<EnumSet<Input>>>,
    ccw: Option<Vec<EnumSet<Input>>>,
}

impl ByPrevMove {
    fn filtered(&self, inc: EnumSet<Input>) -> impl Iterator<Item = &[EnumSet<Input>]> {
        let left = self.left.as_deref().filter(|_| inc.contains(Input::Left));
        let right = self.right.as_deref().filter(|_| inc.contains(Input::Right));
        let cw = self.cw.as_deref().filter(|_| inc.contains(Input::Cw));
        let ccw = self.ccw.as_deref().filter(|_| inc.contains(Input::Ccw));

        std::array::IntoIter::new([left, right, cw, ccw]).filter_map(|x| x)
    }

    fn shortest(&self) -> &[EnumSet<Input>] {
        self.filtered(EnumSet::all())
            .min_by_key(|v| v.len())
            .unwrap()
    }
}

#[derive(Clone, Copy, Debug)]
struct QueueItem {
    place: Placement,
    inputs: usize,
}

fn above_stack(board: &Board, placement: Placement) -> Option<Vec<EnumSet<Input>>> {
    for (x, y) in placement.cells() {
        if y < board.column_height(x as usize) {
            return None;
        }
    }

    let mut path = match placement.rotation {
        Rotation::North => vec![],
        Rotation::East => vec![EnumSet::only(Input::Cw)],
        Rotation::South => vec![
            EnumSet::only(Input::Cw),
            EnumSet::empty(),
            EnumSet::only(Input::Cw),
        ],
        Rotation::West => vec![EnumSet::only(Input::Ccw)],
    };

    let dx = placement.x - 4;
    let movement = if dx < 0 { Input::Left } else { Input::Right };
    let movement_inputs = [EnumSet::only(movement), EnumSet::empty()];
    let mut required_movements = (0..dx.abs()).flat_map(|_| movement_inputs.iter().copied());

    for (existing, new) in path.iter_mut().zip(&mut required_movements) {
        existing.insert_all(new);
    }
    path.extend(required_movements);

    Some(path)
}

const CANT_DROP: EnumSet<Input> = enumset::enum_set!(Input::Left | Input::Right);

fn add_hard_drop(path: &mut Vec<EnumSet<Input>>) {
    match path.last_mut() {
        Some(inputs) if inputs.is_disjoint(CANT_DROP) => {
            inputs.insert(Input::HardDrop);
        }
        _ => {
            path.push(EnumSet::only(Input::HardDrop));
        }
    }
}

fn add_soft_drop(path: &mut Vec<EnumSet<Input>>, cells: usize) {
    let mut required_movements = (0..cells * 3).map(|_| EnumSet::only(Input::Softdrop));
    let mut prev_couldnt_drop = false;
    for existing in path.iter_mut() {
        if !prev_couldnt_drop && existing.is_disjoint(CANT_DROP) {
            if let Some(mv) = required_movements.next() {
                existing.insert_all(mv);
            } else {
                return;
            }
        }
        prev_couldnt_drop = !existing.is_disjoint(CANT_DROP);
    }
    path.extend(required_movements);
}

impl PartialEq for QueueItem {
    fn eq(&self, other: &Self) -> bool {
        self.inputs == other.inputs
    }
}

impl Eq for QueueItem {}

impl Ord for QueueItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inputs.cmp(&other.inputs)
    }
}

impl PartialOrd for QueueItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn check_ez_hard_drops() {
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
        Some(vec![
            EnumSet::only(Input::Left),
            EnumSet::empty(),
            EnumSet::only(Input::Left),
            EnumSet::only(Input::HardDrop)
        ])
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
        Some(vec![
            EnumSet::only(Input::Cw),
            EnumSet::empty(),
            Input::Cw | Input::HardDrop
        ])
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
        Some(vec![Input::Cw | Input::HardDrop])
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
        Some(vec![Input::Ccw | Input::HardDrop])
    );
}
