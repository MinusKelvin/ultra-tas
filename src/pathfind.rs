use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap};

use arrayvec::ArrayVec;
use enumset::{EnumSet, EnumSetType};

use crate::data::{Board, Piece, Placement, Rotation};

#[derive(EnumSetType, Debug, Hash)]
pub enum Input {
    Left,
    Right,
    Cw,
    Ccw,
    Softdrop,
    HardDrop,
}

pub fn pathfind(board: &Board, placement: Placement) -> Option<(u32, Vec<EnumSet<Input>>)> {
    let mut best: Option<(u32, Vec<_>)> = None;

    let mut reverse_paths = HashMap::<_, (u32, Vec<EnumSet<_>>)>::new();
    let mut queue = BinaryHeap::new();

    let starting_vertex = Vertex {
        place: placement,
        next_input: EnumSet::empty(),
    };
    reverse_paths.insert(starting_vertex, (0, vec![]));
    queue.push(QueueItem {
        vertex: starting_vertex,
        inputs: 0,
    });

    if let Some(placement) = placement.other() {
        let starting_vertex = Vertex {
            place: placement,
            next_input: EnumSet::empty(),
        };
        reverse_paths.insert(starting_vertex, (0, vec![]));
        queue.push(QueueItem {
            vertex: starting_vertex,
            inputs: 0,
        });
    }

    while let Some(item) = queue.pop() {
        let (score, rev_path) = reverse_paths.get(&item.vertex).unwrap().clone();
        if item.inputs != rev_path.len() {
            continue;
        }

        if let Some(mut path) = above_stack(board, item.vertex.place) {
            let top_y = match (item.vertex.place.piece, item.vertex.place.rotation) {
                (Piece::O, Rotation::East | Rotation::South) => 20,
                (Piece::I, Rotation::West | Rotation::South) => 18,
                _ => 19,
            };
            let distance = (top_y - item.vertex.place.y) as u32;
            let mut score = score + distance;
            if item.inputs != 0 {
                add_soft_drop(&mut path, (top_y - item.vertex.place.y) as usize);
                path.extend(rev_path.into_iter().rev());
            } else {
                score += distance;
            }
            add_hard_drop(&mut path);
            match &mut best {
                Some((_, p)) if path.len() < p.len() => best = Some((score, path)),
                None => best = Some((score, path)),
                _ => {}
            }
            continue;
        }

        let mut expand = |place: Placement, inputs: EnumSet<Input>| {
            let vertex = Vertex {
                place,
                next_input: inputs,
            };
            let mut rev_path = rev_path.clone();
            let mut score = score;
            if inputs.contains(Input::HardDrop) {
                score += 2;
            } else if inputs.contains(Input::Softdrop) {
                score += 1;
                rev_path.push(inputs);
                rev_path.push(inputs);
                rev_path.push(inputs);
            } else {
                rev_path.push(inputs);
            }
            match reverse_paths.entry(vertex) {
                Entry::Occupied(mut e) => {
                    if rev_path.len() < e.get().1.len() {
                        queue.push(QueueItem {
                            vertex,
                            inputs: rev_path.len(),
                        });
                        e.insert((score, rev_path));
                    }
                }
                Entry::Vacant(e) => {
                    queue.push(QueueItem {
                        vertex,
                        inputs: rev_path.len(),
                    });
                    e.insert((score, rev_path));
                }
            }
        };

        if item
            .vertex
            .next_input
            .is_disjoint(Input::Left | Input::Softdrop)
        {
            for place in un_move(board, item.vertex.place, Input::Left) {
                expand(place, EnumSet::only(Input::Left));

                // same-frame rotates happen before movements
                if !item.vertex.next_input.contains(Input::Cw) {
                    for place in un_move(board, place, Input::Cw) {
                        expand(place, Input::Left | Input::Cw);
                    }
                }
                if !item.vertex.next_input.contains(Input::Ccw) {
                    for place in un_move(board, place, Input::Ccw) {
                        expand(place, Input::Left | Input::Ccw);
                    }
                }
            }
        }

        if item
            .vertex
            .next_input
            .is_disjoint(Input::Right | Input::Softdrop)
        {
            for place in un_move(board, item.vertex.place, Input::Right) {
                expand(place, EnumSet::only(Input::Right));

                // same-frame rotates happen before movements
                if !item.vertex.next_input.contains(Input::Cw) {
                    for place in un_move(board, place, Input::Cw) {
                        expand(place, Input::Right | Input::Cw);
                    }
                }
                if !item.vertex.next_input.contains(Input::Ccw) {
                    for place in un_move(board, place, Input::Ccw) {
                        expand(place, Input::Right | Input::Ccw);
                    }
                }
            }
        }

        if !item.vertex.next_input.contains(Input::Cw) {
            for place in un_move(board, item.vertex.place, Input::Cw) {
                expand(place, EnumSet::only(Input::Cw));
            }
        }

        if !item.vertex.next_input.contains(Input::Ccw) {
            for place in un_move(board, item.vertex.place, Input::Ccw) {
                expand(place, EnumSet::only(Input::Ccw));
            }
        }

        if !item.vertex.next_input.is_empty() {
            expand(item.vertex.place, EnumSet::empty());
        }

        for place in un_move(board, item.vertex.place, Input::Softdrop) {
            let input = if item.inputs == 0 {
                Input::HardDrop
            } else {
                Input::Softdrop
            };
            expand(place, EnumSet::only(input));
        }
    }

    best
}

fn un_move(board: &Board, place: Placement, action: Input) -> ArrayVec<Placement, 5> {
    let mut places = ArrayVec::new();
    let mut reverse_rotate = |source: Rotation| {
        let kicks_back = place.kicks(source);
        for (dx, dy) in kicks_back {
            let start = Placement {
                x: place.x + dx,
                y: place.y + dy,
                rotation: source,
                piece: place.piece,
            };
            if start.obstructed(board) {
                continue;
            }
            for (dx, dy) in kicks_back {
                let result = Placement {
                    x: start.x - dx,
                    y: start.y - dy,
                    rotation: place.rotation,
                    piece: place.piece,
                };
                if !result.obstructed(board) {
                    if result == place {
                        places.push(start);
                    }
                    break;
                }
            }
        }
    };
    match action {
        Input::Left => {
            let place = Placement {
                x: place.x + 1,
                ..place
            };
            if !place.obstructed(board) {
                places.push(place);
            }
        }
        Input::Right => {
            let place = Placement {
                x: place.x - 1,
                ..place
            };
            if !place.obstructed(board) {
                places.push(place);
            }
        }
        Input::Softdrop => {
            let place = Placement {
                y: place.y + 1,
                ..place
            };
            if !place.obstructed(board) {
                places.push(place);
            }
        }
        Input::Cw => reverse_rotate(place.rotation.ccw()),
        Input::Ccw => reverse_rotate(place.rotation.cw()),
        _ => unreachable!(),
    };
    places
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Vertex {
    place: Placement,
    next_input: EnumSet<Input>,
}

#[derive(Clone, Copy, Debug)]
struct QueueItem {
    vertex: Vertex,
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

    let origin = match (placement.piece, placement.rotation) {
        (Piece::I, Rotation::South) => 5,
        (Piece::O, Rotation::West | Rotation::South) => 5,
        _ => 4,
    };

    let dx = placement.x - origin;
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

#[cfg(test)]
#[path = "pathfind-tests.rs"]
mod tests;
