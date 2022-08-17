use data::Piece;
use structopt::StructOpt;

mod archive;
mod data;
mod fourline;
mod pathfind;
mod placement_search;
mod sixline;
mod sixline2;
mod solve;
mod twoline;

#[derive(StructOpt)]
pub enum Command {
    /// TSD-Tetris PC database generation commands
    SixLine(sixline::Options),
    /// 6-line PC database generation commands
    SixLine2(sixline2::Options),
    /// 4-line PC database generation commands
    FourLine(fourline::Options),
    /// Solve an Ultra piece sequence
    Solve(solve::Options),
}

pub fn main() {
    match Command::from_args() {
        Command::SixLine(subcommand) => subcommand.run(),
        Command::SixLine2(subcommand) => subcommand.run(),
        Command::FourLine(subcommand) => subcommand.run(),
        Command::Solve(subcommand) => subcommand.run(),
    }
}

// stable polyfill for [T; N]::zip
trait ArrayExt<A, B> {
    type Map;
    fn azip(self, other: Self, f: impl FnMut(A, A) -> B) -> Self::Map;
}

impl<A, B, const N: usize> ArrayExt<A, B> for [A; N] {
    type Map = [B; N];

    fn azip(self, other: Self, mut f: impl FnMut(A, A) -> B) -> [B; N] {
        let mut iter_a = self.into_iter();
        let mut iter_b = other.into_iter();
        std::array::from_fn(|_| f(iter_a.next().unwrap(), iter_b.next().unwrap()))
    }
}

fn parse_seq(p: &str) -> Result<Vec<Piece>, &'static str> {
    p.chars().try_fold(vec![], |mut v, c| {
        v.push(parse_piece(c)?);
        Ok(v)
    })
}

fn parse_piece(c: char) -> Result<Piece, &'static str> {
    Ok(match c {
        'I' => Piece::I,
        'O' => Piece::O,
        'T' => Piece::T,
        'L' => Piece::L,
        'J' => Piece::J,
        'S' => Piece::S,
        'Z' => Piece::Z,
        _ => return Err("invalid piece"),
    })
}
