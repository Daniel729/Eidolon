use eidolon::{
    chess::Game,
    constants::DEFAULT_HASH_SIZE,
    search::{SearchTime, TranspositionTable, iterative_search_by_time},
};
use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::Duration,
};
pub fn epd(filename: &str, millis: u64) {
    let file = File::open(filename).expect("File not found");
    let reader = BufReader::new(file);

    let mut correct = 0;
    let mut total = 0;

    let mut cache = TranspositionTable::new(DEFAULT_HASH_SIZE);

    for line in reader.lines() {
        let line = line.unwrap();
        let mut terms = line.split_ascii_whitespace().collect::<Vec<&str>>();
        let winning_move = terms.pop().unwrap();
        let fen = terms.join(" ");
        let mut game = Game::new(&fen).unwrap();

        cache.clear();

        let best_move = iterative_search_by_time(
            &mut game,
            &mut cache,
            SearchTime::Fixed(Duration::from_millis(millis)),
            None,
            None,
            |_| {},
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
