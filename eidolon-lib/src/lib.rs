#![feature(avx512_target_feature)]
#![feature(portable_simd)]

pub mod chess;
pub mod constants;
pub mod perft;
pub mod search;

mod transposition_hashmap;
