use crate::{
    chess::{move_struct::Move, Game, Player},
    constants::TT_CAPACITY,
    search::{get_best_move_until_stop, TranspositionTable},
};
use anyhow::{bail, Context};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{
    collections::HashMap,
    io::stdin,
    str::SplitAsciiWhitespace,
    sync::{
        atomic::{AtomicBool, Ordering::Relaxed},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

struct Data {
    current_game: Option<Game>,
    cache: TranspositionTable,
}

impl Data {
    fn mut_refs(&mut self) -> (&mut Option<Game>, &mut TranspositionTable) {
        (&mut self.current_game, &mut self.cache)
    }
}

/// Enter uci mode and wait for commands
///
/// Specification of UCI standard source
/// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
pub fn uci_talk() -> anyhow::Result<()> {
    let data = Arc::new(Mutex::new(Data {
        current_game: None,
        cache: HashMap::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default()),
    }));

    let mut search_thread: Option<JoinHandle<()>> = None;
    let mut search_is_running = Arc::new(AtomicBool::new(false));

    'main_loop: for line in stdin().lines() {
        let line = line.context("Failed to read line from stdin")?;

        let mut terms = line.split_ascii_whitespace();

        while let Some(term) = terms.next() {
            match term {
                "uci" => {
                    command_uci();
                }
                "ucinewgame" => {
                    if search_is_running.load(Relaxed) {
                        let thread =
                            search_thread.context("There should a search thread running")?;
                        search_is_running.store(false, Relaxed);
                        thread.join().unwrap();
                        search_thread = None;
                    }
                    let mut data = data.lock().unwrap();
                    command_ucinewgame(&mut data);
                }
                "isready" => {
                    command_isready();
                }
                "position" => {
                    if search_is_running.load(Relaxed) {
                        println!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let mut data = data.lock().unwrap();
                        if let Err(err) = command_position(&mut data, &mut terms) {
                            println!("error: {}", err);
                        };
                    }
                }
                "go" => {
                    if search_is_running.load(Relaxed) {
                        println!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        // Create new bool such that if the old sleep threaed is still runnning
                        // it won't affect this new search
                        search_is_running = Arc::new(AtomicBool::new(false));
                        match command_go(&data, &mut terms, &search_is_running) {
                            Ok(thread) => search_thread = Some(thread),
                            Err(err) => println!("error: {}", err),
                        }
                    }
                }
                "show" => {
                    if search_is_running.load(Relaxed) {
                        println!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let data = data.lock().unwrap();
                        if let Err(err) = command_show(&data) {
                            println!("error: {}", err);
                        };
                    }
                }
                "stop" => {
                    search_is_running.store(false, Relaxed);
                    if let Some(thread) = search_thread {
                        thread.join().unwrap();
                        search_thread = None;
                    }
                }
                "wait" => {
                    if let Some(thread) = search_thread {
                        thread.join().unwrap();
                        search_thread = None;
                        search_is_running.store(false, Relaxed);
                    }
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
    data_mutex: &Arc<Mutex<Data>>,
    terms: &mut SplitAsciiWhitespace<'_>,
    search_is_running: &Arc<AtomicBool>,
) -> anyhow::Result<JoinHandle<()>> {
    let mut data = data_mutex.lock().unwrap();
    let Some(game) = data.current_game.as_mut() else {
        bail!("No game to play, please set a position first");
    };

    let mut wtime: Option<u64> = None;
    let mut btime: Option<u64> = None;
    let mut winc: Option<u64> = None;
    let mut binc: Option<u64> = None;
    let mut depth: Option<u8> = None;
    let mut move_time: Option<u64> = None;
    let mut infinite = false;

    while let Some(term) = terms.next() {
        match term {
            "wtime" => wtime = terms.next().and_then(|s| s.parse().ok()),
            "btime" => btime = terms.next().and_then(|s| s.parse().ok()),
            "winc" => winc = terms.next().and_then(|s| s.parse().ok()),
            "binc" => binc = terms.next().and_then(|s| s.parse().ok()),
            "depth" => depth = terms.next().and_then(|s| s.parse().ok()),
            "movetime" => move_time = terms.next().and_then(|s| s.parse().ok()),
            "infinite" => infinite = true,
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

    if let Some(move_time) = move_time {
        time = Some(Duration::from_millis(move_time));
    }

    if let Some(time) = time {
        if !infinite {
            // Cut 5 ms from the time because sleep always takes more than given
            let time = time.saturating_sub(Duration::from_millis(5));

            println!("info time {:?}", time.as_millis());

            // This thread might stop a future search if the current one stops by itself
            // Thus when a new search is started, a new atomic bool is created
            thread::spawn({
                let search_is_running = search_is_running.clone();
                move || {
                    thread::sleep(time);
                    search_is_running.store(false, Relaxed);
                }
            });
        }
    }

    let thread = thread::spawn({
        search_is_running.store(true, Relaxed);
        let data_mutex = data_mutex.clone();
        let search_is_running = search_is_running.clone();
        move || {
            let mut data = data_mutex.lock().unwrap();
            let (current_game, cache) = data.mut_refs();
            let best_move = get_best_move_until_stop(
                current_game.as_mut().unwrap(),
                cache,
                &search_is_running,
                depth,
            );

            if let Some(best_move) = best_move {
                println!("bestmove {}", best_move.uci_notation());
            } else {
                println!("bestmove none");
            }

            search_is_running.store(false, Relaxed);
            *current_game = None;
        }
    });

    Ok(thread)
}

fn command_position(data: &mut Data, terms: &mut SplitAsciiWhitespace<'_>) -> anyhow::Result<()> {
    let mut add_moves = false;

    if let Some(term) = terms.next() {
        let game = match term {
            "startpos" => {
                data.current_game = Some(Game::default());

                if let Some(term) = terms.next() {
                    if term == "moves" {
                        add_moves = true;
                    }
                }

                data.current_game.as_mut().unwrap()
            }
            "fen" => {
                let fen: String = terms
                    .by_ref()
                    .take_while(|&term| {
                        if term == "moves" {
                            add_moves = true;
                            false
                        } else {
                            true
                        }
                    })
                    .flat_map(|term| [term, " "].into_iter())
                    .collect();

                match Game::new(&fen) {
                    Ok(fen_game) => {
                        data.current_game = Some(fen_game);
                        data.current_game.as_mut().unwrap()
                    }
                    Err(err) => {
                        data.current_game = None;
                        bail!("{:?}", err.context("Invalid FEN string"));
                    }
                }
            }
            _ => bail!("Invalid position command"),
        };

        if add_moves {
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
    } else {
        bail!("Invalid position command");
    }

    Ok(())
}
