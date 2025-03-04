use arrayvec::ArrayVec;
use eidolon::{
    chess::Game,
    constants::DEFAULT_HASH_SIZE,
    search::{SearchTime, TranspositionTable, iterative_search_by_time},
};
use std::time::Duration;

pub fn autoplay(millis: u64) {
    let mut game = Game::default();
    let mut cache = TranspositionTable::new(DEFAULT_HASH_SIZE);

    loop {
        let mut moves = ArrayVec::new();
        game.get_moves_main(&mut moves);

        let next_move = match iterative_search_by_time(
            &mut game,
            &mut cache,
            SearchTime::BestEffort {
                total: Duration::from_millis(2 * millis),
                inc: Duration::from_millis(millis / 2),
                overhead: Duration::ZERO,
                moves_to_go: None,
            },
            None,
            None,
            |_| {},
        ) {
            Some(_move) => _move,
            None => {
                if moves.is_empty() {
                    println!("--- Game Over ---");
                } else if game.threefold_repetition() {
                    println!("--- Draw by Threefold Repetition ---");
                } else if game.length() >= 250 {
                    println!("--- Draw because the game is too long ---");
                } else {
                    println!("--- ERROR ---");
                }

                break;
            }
        };

        game.push_history(next_move);
    }
}
