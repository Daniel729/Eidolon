use crate::{
    chess::move_struct::Move,
    chess::{Game, Player},
    constants::TT_CAPACITY,
    search::{get_best_move_in_time, TranspositionTable},
};
use anyhow::{bail, Context};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{collections::HashMap, io::stdin, time::Duration};

struct Data {
    current_game: Option<Game>,
    cache: TranspositionTable,
}

/// Enter uci mode and wait for commands
///
/// Specification of UCI standard source
/// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
pub fn uci_talk() -> anyhow::Result<()> {
    let mut data = Data {
        current_game: None,
        cache: HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default()),
    };

    'main_loop: for line in stdin().lines() {
        let line = line.context("Failed to read line from stdin")?;

        let mut terms = line.split_ascii_whitespace();

        while let Some(term) = terms.next() {
            match term {
                "uci" => {
                    command_uci();
                }
                "ucinewgame" => {
                    command_ucinewgame(&mut data);
                }
                "isready" => {
                    command_isready();
                }
                "position" => {
                    if let Err(err) = command_position(&mut data, &mut terms) {
                        println!("error: {}", err);
                    };
                }
                "go" => {
                    if let Err(err) = command_go(&mut data, &mut terms) {
                        println!("error: {}", err);
                    };
                }
                "show" => {
                    if let Err(err) = command_show(&data) {
                        println!("error: {}", err);
                    };
                }
                "quit" => {
                    break 'main_loop;
                }
                _ => continue,
            }

            break;
        }
    }

    Ok(())
}

fn command_uci() {
    println!("id name rustybait");
    println!("id author Malanca Daniel");
    println!("uciok");
}

fn command_ucinewgame(data: &mut Data) {
    data.cache.clear();
    data.current_game = None;
}

fn command_isready() {
    println!("readyok");
}

fn command_show(data: &Data) -> anyhow::Result<()> {
    if let Some(game) = data.current_game.as_ref() {
        println!("{}", game);
    } else {
        bail!("No game to show, please set a position first");
    }

    Ok(())
}

fn command_go(
    data: &mut Data,
    terms: &mut std::str::SplitAsciiWhitespace<'_>,
) -> anyhow::Result<()> {
    let Some(game) = data.current_game.as_mut() else {
        bail!("No game to play, please set a position first");
    };

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
    const LATENCY_MS_COMPENSATE: u64 = 150;

    let mut time = None;

    if wtime.is_some() && btime.is_some() && winc.is_some() && binc.is_some() {
        let wtime = wtime.unwrap();
        let btime = btime.unwrap();
        let winc = winc.unwrap();
        let binc = binc.unwrap();

        // We decrease the time to make sure we never run out
        let white_time =
            (wtime as f64 * FRACTION_OF_TOTAL_TIME) as u64 + winc - LATENCY_MS_COMPENSATE;
        let black_time =
            (btime as f64 * FRACTION_OF_TOTAL_TIME) as u64 + binc - LATENCY_MS_COMPENSATE;

        time = if game.player() == Player::White {
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

    if let Some(best_move) = get_best_move_in_time(game, time, &mut data.cache, true) {
        println!("bestmove {}", best_move.uci_notation());
    }

    data.current_game = None;

    Ok(())
}

fn command_position(
    data: &mut Data,
    terms: &mut std::str::SplitAsciiWhitespace<'_>,
) -> anyhow::Result<()> {
    if let Some(term) = terms.next() {
        match term {
            "startpos" => {
                data.current_game = Some(Game::default());
                let game = data.current_game.as_mut().unwrap();
                if let Some(term) = terms.next() {
                    if term == "moves" {
                        for move_str in terms.by_ref() {
                            let Some(_move) = Move::from_uci_notation(move_str, game) else {
                                data.current_game = None;
                                bail!("Invalid move: {}", move_str);
                            };

                            let mut moves = ArrayVec::new();
                            game.get_moves(&mut moves, true);
                            if moves.iter().any(|allowed_move| _move == *allowed_move) {
                                game.push_history(_move);
                                if game.len() >= 400 {
                                    data.current_game = None;
                                    bail!("Game became too long, please try again");
                                }
                            } else {
                                bail!("Invalid move: {}", move_str);
                            }
                        }
                    }
                }
            }
            "fen" => {
                let mut terms = terms.clone();

                let fen: String = terms
                    .by_ref()
                    .take_while(|term| *term != "moves")
                    .flat_map(|term| [term, " "].into_iter())
                    .collect();

                let game = match Game::new(&fen) {
                    Ok(fen_game) => {
                        data.current_game = Some(fen_game);
                        data.current_game.as_mut().unwrap()
                    }
                    Err(err) => {
                        data.current_game = None;
                        bail!("{:?}", err.context("Invalid FEN string"));
                    }
                };

                for move_str in terms.by_ref() {
                    let Some(_move) = Move::from_uci_notation(move_str, game) else {
                        data.current_game = None;
                        bail!("Invalid move: {}", move_str);
                    };

                    let mut moves = ArrayVec::new();
                    game.get_moves(&mut moves, true);
                    if moves.iter().any(|allowed_move| _move == *allowed_move) {
                        game.push_history(_move);
                        if game.len() >= 400 {
                            data.current_game = None;
                            bail!("Game became too long, please try again");
                        }
                    } else {
                        bail!("Invalid move: {}", move_str);
                    }
                }
            }
            _ => bail!("Invalid position command"),
        }
    } else {
        bail!("Invalid position command");
    }

    Ok(())
}
