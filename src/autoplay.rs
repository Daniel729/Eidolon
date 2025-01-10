use crate::{
    chess::Game,
    constants::{TT_CAPACITY, TT_REAL_CAPACITY},
    search::{get_best_move_until_stop, HistoryData, TranspositionTable},
};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::time::Duration;

pub fn autoplay(millis: u64) {
    let mut game = Game::default();
    let mut cache =
        TranspositionTable::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());
    let mut history = HistoryData::default();

    TT_REAL_CAPACITY.set(cache.capacity() / 2).unwrap();

    loop {
        let mut moves = ArrayVec::new();
        game.get_moves_main(&mut moves);
        println!("{}", &game);

        let next_move = match get_best_move_until_stop(
            &game,
            &mut cache,
            &mut history,
            Duration::from_millis(millis),
            None,
            None,
        ) {
            Some(_move) => _move,
            None => {
                if moves.is_empty() {
                    println!("--- Game Over ---");
                } else if game.times_seen_position() >= 3 {
                    println!("--- Draw by Threefold Repetition ---");
                } else {
                    println!("--- ERROR ---");
                }

                break;
            }
        };

        game.push_history(next_move);
    }
}
