use crate::{
    chess_game::ChessGame,
    constants::TT_CAPACITY,
    move_struct::Move,
    search::{get_best_move_entry, TranspositionTable},
};

use std::{
    collections::HashMap, hash::BuildHasherDefault, sync::atomic::AtomicBool, time::Instant,
};

/// Source: https://lichess.org/study/rROPNxQX/NucjwPjN
///
/// This game does not have a drawn-out endgame \
/// It is a good test for the engine's opening and midgame play
pub const GAME: &str = "g1f3 g8f6 c2c4 g7g6 b1c3 f8g7 d2d4 e8g8 c1f4 d7d5 d1b3 d5c4
                b3c4 c7c6 e2e4 b8d7 a1d1 d7b6 c4c5 c8g4 f4g5 b6a4 c5a3 a4c3 
                b2c3 f6e4 g5e7 d8b6 f1c4 e4c3 e7c5 f8e8 e1f1 g4e6 c5b6 e6c4 
                f1g1 c3e2 g1f1 e2d4 f1g1 d4e2 g1f1 e2c3 f1g1 a7b6 a3b4 a8a4 
                b4b6 c3d1 h2h3 a4a2 g1h2 d1f2 h1e1 e8e1 b6d8 g7f8 f3e1 c4d5 
                e1f3 f2e4 d8b8 b7b5 h3h4 h7h5 f3e5 g8g7 h2g1 f8c5 g1f1 e4g3 
                f1e1 c5b4 e1d1 d5b3 d1c1 g3e2 c1b1 e2c3 b1c1 a2c2";

pub fn run_simple_benchmark(depth: u8, steps: u8) {
    let mut moves = GAME.split_ascii_whitespace();

    let mut game = ChessGame::default();
    let mut durations = vec![];

    let atomic_false = AtomicBool::new(false);

    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildHasherDefault::default());
    'outer: loop {
        let now = Instant::now();

        let mut history = [0; 64 * 12];

        get_best_move_entry(game.clone(), &atomic_false, depth, &mut cache, &mut history).unwrap();

        durations.push(now.elapsed());

        for _ in 0..steps {
            let Some(_move) = moves.next() else {
                break 'outer;
            };

            game.push(Move::from_uci_notation(_move, &game).unwrap());
        }
    }

    let product = durations.iter().fold(1.0, |acc, x| acc * x.as_secs_f64());
    let geo_mean = product.powf(1.0 / durations.len() as f64);

    println!(
        "Depth: {}, Steps: {}
Geometric Mean: {:.2} ms",
        depth,
        steps,
        geo_mean * 1000.0
    );
}

pub fn run_iterative_benchmark(depth: u8, steps: u8) {
    let mut moves = GAME.split_ascii_whitespace();

    let mut game = ChessGame::default();
    let mut durations = vec![];

    let atomic_false = AtomicBool::new(false);

    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildHasherDefault::default());
    'outer: loop {
        let now = Instant::now();

        let mut history = [0; 64 * 12];

        for iter_depth in 2..=depth {
            get_best_move_entry(
                game.clone(),
                &atomic_false,
                iter_depth,
                &mut cache,
                &mut history,
            )
            .unwrap();
        }

        durations.push(now.elapsed());

        for _ in 0..steps {
            let Some(_move) = moves.next() else {
                break 'outer;
            };

            game.push(Move::from_uci_notation(_move, &game).unwrap());
        }
    }

    let product = durations.iter().fold(1.0, |acc, x| acc * x.as_secs_f64());
    let geo_mean = product.powf(1.0 / durations.len() as f64);

    println!(
        "Depth: {}, Steps: {}
Geometric Mean: {:.2} ms",
        depth,
        steps,
        geo_mean * 1000.0
    );
}
