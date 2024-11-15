use crate::{
    chess::{move_struct::Move, Game},
    constants::{TESTING_GAME, TT_CAPACITY},
    search::{get_best_move_entry, TranspositionTable},
};
use std::{
    collections::HashMap, hash::BuildHasherDefault, sync::atomic::AtomicBool, time::Instant,
};

pub fn run_simple_benchmark(depth: u8, steps: u8) {
    let mut moves = TESTING_GAME.split_ascii_whitespace();

    let mut game = Game::default();
    let mut durations = vec![];

    let is_running = AtomicBool::new(true);

    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildHasherDefault::default());
    'outer: loop {
        let now = Instant::now();

        let mut history = [0; 64 * 12];

        get_best_move_entry(game.clone(), &is_running, depth, &mut cache, &mut history).unwrap();

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
    let mut moves = TESTING_GAME.split_ascii_whitespace();

    let mut game = Game::default();
    let mut durations = vec![];

    let is_running = AtomicBool::new(true);

    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildHasherDefault::default());
    'outer: loop {
        let now = Instant::now();

        let mut history = [0; 64 * 12];

        for iter_depth in 1..=depth {
            get_best_move_entry(
                game.clone(),
                &is_running,
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
