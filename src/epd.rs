use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::Duration,
};

use nohash_hasher::BuildNoHashHasher;

use crate::{
    chess::Game,
    constants::TT_CAPACITY,
    search::{get_best_move_until_stop, HistoryData, TranspositionTable},
};

pub fn epd(filename: &str, millis: u64) {
    let file = File::open(filename).expect("File not found");
    let reader = BufReader::new(file);

    let mut correct = 0;
    let mut total = 0;

    let mut cache =
        TranspositionTable::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());

    for line in reader.lines() {
        let line = line.unwrap();
        let mut terms = line.split_ascii_whitespace().collect::<Vec<&str>>();
        let winning_move = terms.pop().unwrap();
        let fen = terms.join(" ");
        let game = Game::new(&fen).unwrap();

        cache.clear();

        let mut history = HistoryData::default();

        let best_move = get_best_move_until_stop(
            &game,
            &mut cache,
            &mut history,
            Duration::from_millis(millis),
            None,
            None,
        );

        total += 1;

        if best_move.is_some() && best_move.unwrap().uci_notation() == winning_move {
            correct += 1;
        }

        println!(
            "{:.0}% correct, {} / {}",
            (correct as f64 / total as f64) * 100.0,
            correct,
            total
        );
    }
}
