use arrayvec::ArrayVec;
use structopt::StructOpt;

mod data;
mod sixline;

#[derive(StructOpt)]
pub enum Command {
    /// TSD-Tetris PC database generation commands
    SixLine(sixline::Options),
}

pub fn main() {
    match Command::from_args() {
        Command::SixLine(subcommand) => subcommand.run(),
    }
}

// stable polyfill for [T; N]::map
trait ArrayExt<A, B> {
    type Map;
    fn amap(self, f: impl FnMut(A) -> B) -> Self::Map;
}

impl<A, B, const N: usize> ArrayExt<A, B> for [A; N] {
    type Map = [B; N];
    fn amap(self, f: impl FnMut(A) -> B) -> [B; N] {
        std::array::IntoIter::new(self)
            .map(f)
            .collect::<ArrayVec<_, N>>()
            .into_inner()
            .unwrap_or_else(|_| unreachable!())
    }
}
