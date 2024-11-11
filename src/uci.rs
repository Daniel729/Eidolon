use std::{collections::HashMap, io::stdin, time::Duration};

use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;

use crate::{
    chess_game::{ChessGame, Players},
    constants::TT_CAPACITY,
    move_struct::Move,
    search::{get_best_move_in_time, TranspositionTable},
};

pub fn uci_talk() {
    let mut game = ChessGame::default();
    let mut cache: TranspositionTable =
        HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());

    // Source: https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
    'main_loop: for line in stdin().lines() {
        let line = line.unwrap();
        let mut terms = line.split_ascii_whitespace();
        while let Some(term) = terms.next() {
            match term {
                "uci" => {
                    println!("id name daniel_chess");
                    println!("id author Malanca Daniel");
                    println!("uciok");
                    continue 'main_loop;
                }
                "isready" => {
                    println!("readyok");
                    continue 'main_loop;
                }
                "position" => {
                    if let Some(term) = terms.next() {
                        match term {
                            "startpos" => {
                                game = ChessGame::default();
                                if let Some(term) = terms.next() {
                                    if term == "moves" {
                                        for move_str in terms.by_ref() {
                                            let Some(_move) =
                                                Move::from_uci_notation(move_str, &game)
                                            else {
                                                continue 'main_loop;
                                            };

                                            let mut moves = ArrayVec::new();
                                            game.get_moves(&mut moves, true);
                                            if moves
                                                .iter()
                                                .any(|allowed_move| _move == *allowed_move)
                                            {
                                                game.push_history(_move);
                                                // Hard limit onto the number
                                                // of possible moves in a game;
                                                if game.len() >= 400 {
                                                    continue 'main_loop;
                                                }
                                            } else {
                                                continue 'main_loop;
                                            }
                                        }
                                    }
                                }
                            }
                            "fen" => {
                                // TODO: I think it's possible to also get moves
                                // starting from this position
                                // i.e. position fen <fen> moves <moves>
                                let last_terms: Vec<_> = terms.clone().collect();
                                let fen: String = last_terms.join(" ");
                                match ChessGame::new(&fen) {
                                    Ok(fen_game) => {
                                        game = fen_game;
                                    }
                                    Err(err) => {
                                        eprintln!("{:?}", err.context("invalid FEN string"));
                                        continue 'main_loop;
                                    }
                                }
                            }
                            _ => continue 'main_loop,
                        }
                    } else {
                        continue 'main_loop;
                    }
                }
                "go" => {
                    let mut wtime: Option<u64> = None;
                    let mut btime: Option<u64> = None;
                    let mut winc: Option<u64> = None;
                    let mut binc: Option<u64> = None;

                    while let Some(term) = terms.next() {
                        match term {
                            "wtime" => wtime = terms.next().and_then(|s| s.parse().ok()),
                            "btime" => btime = terms.next().and_then(|s| s.parse().ok()),
                            "winc" => winc = terms.next().and_then(|s| s.parse().ok()),
                            "binc" => binc = terms.next().and_then(|s| s.parse().ok()),
                            _ => continue,
                        }
                    }

                    const FRACTION_OF_TOTAL_TIME: f64 = 0.02;
                    let mut time = None;

                    if wtime.is_some() && btime.is_some() && winc.is_some() && binc.is_some() {
                        let wtime = wtime.unwrap();
                        let btime = btime.unwrap();
                        let winc = winc.unwrap();
                        let binc = binc.unwrap();

                        // We subtract 100ms from the time to make sure we don't run out of time
                        let white_time =
                            (wtime as f64 * FRACTION_OF_TOTAL_TIME) as u64 + winc - 100;
                        let black_time =
                            (btime as f64 * FRACTION_OF_TOTAL_TIME) as u64 + binc - 100;

                        time = if game.current_player == Players::White {
                            Some(Duration::from_millis(white_time))
                        } else {
                            Some(Duration::from_millis(black_time))
                        };
                    }

                    if let Some(env_time) = std::env::var("CHESS_TIME_PER_MOVE")
                        .ok()
                        .and_then(|s| s.parse::<u64>().ok())
                    {
                        time = Some(Duration::from_millis(env_time));
                    }

                    let time = time.unwrap_or(Duration::from_secs(2));

                    println!("info time {:?}", time.as_millis());

                    if let Some(best_move) = get_best_move_in_time(&game, time, &mut cache, true) {
                        println!("bestmove {}", best_move.uci_notation());
                        game.push_history(best_move);
                    }
                }
                "show" => {
                    println!("{}", game);
                }
                "quit" => {
                    return;
                }
                _ => continue,
            }
        }
    }
}
