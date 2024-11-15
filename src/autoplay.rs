use crate::{
    chess::Game,
    constants::TT_CAPACITY,
    search::{get_best_move_until_stop, TranspositionTable},
};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, Ordering::Relaxed},
        Arc,
    },
    time::Duration,
};

pub fn autoplay(millis: u64) {
    let mut game = Game::default();
    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());

    loop {
        let mut moves = ArrayVec::new();
        game.get_moves(&mut moves, true);
        println!("{}", game.get_pgn());
        println!("{}", &game);

        let search_is_running = Arc::new(AtomicBool::new(true));

        std::thread::spawn({
            let search_is_running = search_is_running.clone();
            move || {
                std::thread::sleep(Duration::from_millis(millis));
                search_is_running.store(false, Relaxed);
            }
        });

        let next_move = match get_best_move_until_stop(&game, &mut cache, &search_is_running, None)
        {
            Some(_move) => _move,
            None => break,
        };
        game.push_history(next_move);
    }
}
