use std::mem::MaybeUninit;

use structopt::StructOpt;

mod data;
mod fourline;
mod pathfind;
mod sixline;

#[derive(StructOpt)]
pub enum Command {
    /// TSD-Tetris PC database generation commands
    SixLine(sixline::Options),
    /// 4-line PC database generation commands
    FourLine(fourline::Options),
}

pub fn main() {
    match Command::from_args() {
        Command::SixLine(subcommand) => subcommand.run(),
        Command::FourLine(subcommand) => subcommand.run(),
    }
}

// stable polyfill for [T; N]::map
trait ArrayExt<A, B> {
    type Map;
    fn amap(self, f: impl FnMut(A) -> B) -> Self::Map;
    fn azip(self, other: Self, f: impl FnMut(A, A) -> B) -> Self::Map;
}

impl<A, B, const N: usize> ArrayExt<A, B> for [A; N] {
    type Map = [B; N];

    fn amap(self, mut f: impl FnMut(A) -> B) -> [B; N] {
        let mut result: [MaybeUninit<B>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for (i, a) in std::array::IntoIter::new(self).enumerate() {
            result[i] = MaybeUninit::new(f(a));
        }
        unsafe { std::mem::transmute_copy(&result) }
    }

    fn azip(self, other: Self, mut f: impl FnMut(A, A) -> B) -> [B; N] {
        let mut result: [MaybeUninit<B>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for (i, (a, b)) in std::array::IntoIter::new(self)
            .zip(std::array::IntoIter::new(other))
            .enumerate()
        {
            result[i] = MaybeUninit::new(f(a, b));
        }
        unsafe { std::mem::transmute_copy(&result) }
    }
}
