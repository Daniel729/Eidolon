use crate::{
    chess::{move_struct::Move, Game, Player},
    constants::{TT_CAPACITY, TT_REAL_CAPACITY},
    performance_test,
    search::{get_best_move_until_stop, HistoryData, TranspositionTable},
};
use anyhow::{bail, Context};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{
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
    history: HistoryData,
}

impl Data {
    fn mut_refs(&mut self) -> (&mut Option<Game>, &mut TranspositionTable, &mut HistoryData) {
        (&mut self.current_game, &mut self.cache, &mut self.history)
    }
}

macro_rules! send_uci {
    ($($arg:tt)*) => {
        {
            let string = format!($($arg)*);

            println!("{}", string);

            #[cfg(feature = "log")]
            log::info!("[OUTPUT] {}", string);
        }
    };
}

pub(crate) use send_uci;

/// Enter uci mode and wait for commands
///
/// Specification of UCI standard source
/// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
pub fn uci_talk() -> anyhow::Result<()> {
    let cache =
        TranspositionTable::with_capacity_and_hasher(TT_CAPACITY, BuildNoHashHasher::default());

    TT_REAL_CAPACITY.set(cache.capacity() / 2).unwrap();

    let data = Arc::new(Mutex::new(Data {
        current_game: None,
        cache,
        history: HistoryData::default(),
    }));

    let mut search_thread: Option<JoinHandle<()>> = None;
    let mut search_is_running = Arc::new(AtomicBool::new(false));

    'main_loop: for line in stdin().lines() {
        let line = line.context("Failed to read line from stdin")?;

        #[cfg(feature = "log")]
        log::info!("[INPUT] {}", line);

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
                        send_uci!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let mut data = data.lock().unwrap();
                        if let Err(err) = command_position(&mut data, &mut terms) {
                            send_uci!("error: {}", err);
                        };
                    }
                }
                "go" => {
                    if search_is_running.load(Relaxed) {
                        send_uci!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        // Create new bool such that if the old sleep threaed is still runnning
                        // it won't affect this new search
                        search_is_running = Arc::new(AtomicBool::new(false));
                        match command_go(&data, &mut terms, &search_is_running) {
                            Ok(thread) => search_thread = Some(thread),
                            Err(err) => send_uci!("error: {}", err),
                        }
                    }
                }
                "show" | "d" => {
                    if search_is_running.load(Relaxed) {
                        send_uci!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let data = data.lock().unwrap();
                        if let Err(err) = command_show(&data) {
                            send_uci!("error: {}", err);
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
    send_uci!("id name Eidolon 0.1.0");
    send_uci!("id author Malanca Daniel");
    send_uci!("uciok");
}

fn command_ucinewgame(data: &mut Data) {
    data.cache.clear();
    data.current_game = None;
}

fn command_isready() {
    send_uci!("readyok");
}

fn command_show(data: &Data) -> anyhow::Result<()> {
    if let Some(game) = data.current_game.as_ref() {
        send_uci!("{}", game);
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
    let mut depth: Option<u16> = None;
    let mut move_time: Option<u64> = None;
    let mut moves_to_go: Option<u64> = None;
    let mut perft: Option<u16> = None;
    let mut infinite = false;

    while let Some(term) = terms.next() {
        match term {
            "wtime" => wtime = terms.next().and_then(|s| s.parse().ok()),
            "btime" => btime = terms.next().and_then(|s| s.parse().ok()),
            "winc" => winc = terms.next().and_then(|s| s.parse().ok()),
            "binc" => binc = terms.next().and_then(|s| s.parse().ok()),
            "depth" => depth = terms.next().and_then(|s| s.parse().ok()),
            "movetime" => move_time = terms.next().and_then(|s| s.parse().ok()),
            "movestogo" => moves_to_go = terms.next().and_then(|s| s.parse().ok()),
            "perft" => perft = terms.next().and_then(|s| s.parse().ok()),
            "infinite" => infinite = true,
            _ => continue,
        }
    }

    if let Some(depth) = perft {
        let mut game = game.clone();

        return Ok(thread::spawn(move || {
            // Generate perft test result
            let mut moves = ArrayVec::new();
            game.get_moves_main(&mut moves);

            moves.sort_by_cached_key(|_move| _move.uci_notation());

            let mut sum = 0;

            let now = std::time::Instant::now();

            for _move in moves {
                game.push(_move);
                let count = performance_test::perft(&mut game, depth - 1);
                game.pop(_move);
                sum += count;
                println!("{}: {}", _move.uci_notation(), count);
            }

            println!();
            println!("{}", sum);
            println!();
            println!("Time: {:?}", now.elapsed());
            println!("Nodes: {}", game.generated_moves());
            println!(
                "NPS: {:.0}k",
                game.generated_moves() as f64 / now.elapsed().as_secs_f64() / 1000.0
            );
        }));
    }

    const FRACTION_OF_TOTAL_TIME: f64 = 0.025;

    let mut time = None;

    if wtime.is_some() && btime.is_some() {
        let wtime = wtime.unwrap();
        let btime = btime.unwrap();
        let winc = winc.unwrap_or(0);
        let binc = binc.unwrap_or(0);

        let fraction = if let Some(moves_to_go) = moves_to_go {
            1.0 / moves_to_go as f64
        } else {
            FRACTION_OF_TOTAL_TIME
        };

        let white_time = ((wtime - winc) as f64 * fraction) as u64 + winc;
        let black_time = ((btime - binc) as f64 * fraction) as u64 + binc;

        time = if game.player() == Player::White {
            Some(Duration::from_millis(white_time))
        } else {
            Some(Duration::from_millis(black_time))
        };
    }

    if time.is_none() || infinite {
        time = Some(Duration::from_secs(u64::MAX));
    }

    if moves_to_go.is_some() && wtime.is_some() && btime.is_some() {
        let moves_to_go = moves_to_go.unwrap();
        let wtime = wtime.unwrap();
        let btime = btime.unwrap();

        let millis = if game.player() == Player::White {
            wtime / moves_to_go
        } else {
            btime / moves_to_go
        };

        time = Some(Duration::from_millis(millis));
    }

    if let Some(move_time) = move_time {
        time = Some(Duration::from_millis(move_time));
    }

    let thread = thread::spawn({
        search_is_running.store(true, Relaxed);
        let data_mutex = data_mutex.clone();
        let search_is_running = search_is_running.clone();
        move || {
            let mut data = data_mutex.lock().unwrap();
            let (current_game, cache, history) = data.mut_refs();
            let best_move = get_best_move_until_stop(
                current_game.as_mut().unwrap(),
                cache,
                history,
                time.unwrap(),
                Some(&search_is_running),
                depth,
            );

            send_uci!(
                "bestmove {}",
                best_move
                    .as_ref()
                    .map(Move::uci_notation)
                    .unwrap_or_else(|| "none".to_string()),
            );

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
                game.get_moves_main(&mut moves);
                if moves.iter().any(|&allowed_move| _move == allowed_move) {
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
