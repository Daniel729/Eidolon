use std::{collections::HashMap, time::Duration};

use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;

use crate::{
    chess_game::ChessGame,
    constants::TT_CAPACITY,
    search::{get_best_move_in_time, TranspositionTable},
};

pub fn autoplay(millis: u64) {
    let mut game = ChessGame::default();
    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());

    loop {
        let mut moves = ArrayVec::new();
        game.get_moves(&mut moves, true);
        println!("{}", game.get_pgn());
        println!("{}", &game);
        let next_move =
            match get_best_move_in_time(&game, Duration::from_millis(millis), &mut cache, true) {
                Some(_move) => _move,
                None => break,
            };
        game.push_history(next_move);
    }
}
