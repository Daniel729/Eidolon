use anyhow::{Context, Result, bail};
use arrayvec::ArrayVec;
use eidolon::{
    chess::{Game, move_struct::Move, player::Player},
    constants::{DEFAULT_HASH_SIZE, DEFAULT_MOVE_OVERHEAD, MAX_MOVES_GAME},
    perft::perft_uci,
    search::{SearchTime, TranspositionTable, iterative_search_by_time},
};
use std::{
    io::stdin,
    num::NonZeroUsize,
    str::SplitAsciiWhitespace,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering::Relaxed},
    },
    thread::{self, JoinHandle},
    time::Duration,
};

pub struct UciOptions {
    hashmap_size: NonZeroUsize,
    move_overhead: Duration,
}

impl Default for UciOptions {
    fn default() -> Self {
        Self {
            hashmap_size: DEFAULT_HASH_SIZE,
            move_overhead: DEFAULT_MOVE_OVERHEAD,
        }
    }
}

struct Data {
    current_game: Option<Game>,
    uci_options: UciOptions,
    cache: TranspositionTable,
}

impl Data {
    fn mut_refs(&mut self) -> (&mut Option<Game>, &mut TranspositionTable) {
        (&mut self.current_game, &mut self.cache)
    }
}

macro_rules! send_uci {
    ($($arg:tt)*) => {
        {
            let string = format!($($arg)*);

            println!("{}", string);
        }
    };
}

pub(crate) use send_uci;

/// Enter uci mode and wait for commands
///
/// Specification of UCI standard source
/// https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
pub fn uci_talk() -> Result<()> {
    println!("Eidolon 1.0.0");
    println!("Author: Malanca Daniel");
    println!(
        "Compiled using rustc {} ({} {})",
        rustc_version_runtime::version(),
        &rustc_version_runtime::version_meta().commit_hash.unwrap()[..9],
        rustc_version_runtime::version_meta().commit_date.unwrap()
    );
    println!("Type 'uci' to enter UCI mode");
    let cache = TranspositionTable::new(DEFAULT_HASH_SIZE);

    let data = Arc::new(Mutex::new(Data {
        current_game: None,
        cache,
        uci_options: UciOptions::default(),
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
                "setoption" => {
                    if search_is_running.load(Relaxed) {
                        send_uci!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let mut data = data.lock().unwrap();
                        if let Err(err) = command_setoption(&mut data, &mut terms) {
                            send_uci!("error: {}", err);
                        };
                    }
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
                "e" | "eval" => {
                    if search_is_running.load(Relaxed) {
                        send_uci!("error: search is still running, enter 'stop' to stop it");
                    } else {
                        let data = data.lock().unwrap();
                        if let Err(err) = command_eval(&data) {
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

fn command_setoption(data: &mut Data, terms: &mut SplitAsciiWhitespace<'_>) -> Result<()> {
    let Some("name") = terms.next() else {
        bail!("Missing 'name' keyword");
    };

    let remaining = terms.collect::<Vec<_>>().join(" ");
    let mut iter = remaining.trim().split(" value ");

    let name = iter.next().context("Missing option name")?;

    let value = iter.next().context("Missing option value")?;

    match name {
        "Hash" => {
            let value = value.parse().context("Invalid value for 'Hash' option")?;

            if !(1..=16384).contains(&value) {
                bail!("Hash value must be between 1 and 16384");
            }

            data.uci_options.hashmap_size = NonZeroUsize::new(value).unwrap();

            data.cache = TranspositionTable::new(data.uci_options.hashmap_size);
        }
        "Move Overhead" => {
            let value = value
                .parse()
                .context("Invalid value for 'Move Overhead' option")?;

            if !(1..=60000).contains(&value) {
                bail!("Move Overhead value must be between 1 and 60000");
            }

            data.uci_options.move_overhead = Duration::from_millis(value);
        }
        _ => bail!("Unknown option: {}", name),
    }

    Ok(())
}

fn command_uci() {
    send_uci!("id name Eidolon 1.0.0");
    send_uci!("id author Malanca Daniel");
    send_uci!(
        "option name Hash type spin default {} min 1 max 16384",
        DEFAULT_HASH_SIZE
    );
    send_uci!(
        "option name Move Overhead type spin default {} min 0 max 60000",
        DEFAULT_MOVE_OVERHEAD.as_millis()
    );
    send_uci!("option name Ponder type check default false");
    send_uci!("option name OwnBook type check default false");

    send_uci!("uciok");
}

fn command_ucinewgame(data: &mut Data) {
    data.cache.clear();
    data.current_game = None;
}

fn command_isready() {
    send_uci!("readyok");
}

fn command_show(data: &Data) -> Result<()> {
    if let Some(game) = data.current_game.as_ref() {
        send_uci!("{}", game);
    } else {
        bail!("No game to show, please set a position first");
    }

    Ok(())
}

fn command_eval(data: &Data) -> Result<()> {
    if let Some(game) = data.current_game.as_ref() {
        game.display_eval();
    } else {
        bail!("No game to show, please set a position first");
    }

    Ok(())
}

fn command_go(
    data_mutex: &Arc<Mutex<Data>>,
    terms: &mut SplitAsciiWhitespace<'_>,
    search_is_running: &Arc<AtomicBool>,
) -> Result<JoinHandle<()>> {
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
    let mut moves_to_go: Option<u16> = None;
    let mut perft: Option<u16> = None;

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
            _ => continue,
        }
    }

    if let Some(depth) = perft {
        return Ok(thread::spawn({
            let game = game.clone();

            move || perft_uci(game, depth)
        }));
    }

    let (total_time, inc) = if game.player() == Player::White {
        (wtime, winc.unwrap_or(0))
    } else {
        (btime, binc.unwrap_or(0))
    };

    let search_time = if let Some(fixed_time) = move_time {
        SearchTime::Fixed(Duration::from_millis(fixed_time))
    } else if let Some(total_time) = total_time {
        SearchTime::BestEffort {
            total: Duration::from_millis(total_time),
            inc: Duration::from_millis(inc),
            overhead: data.uci_options.move_overhead,
            moves_to_go,
        }
    } else {
        SearchTime::Infinite
    };

    let thread = thread::spawn({
        search_is_running.store(true, Relaxed);
        let data_mutex = data_mutex.clone();
        let search_is_running = search_is_running.clone();
        move || {
            let mut data = data_mutex.lock().unwrap();
            let (current_game, cache) = data.mut_refs();
            let best_move = iterative_search_by_time(
                current_game.as_mut().unwrap(),
                cache,
                search_time,
                Some(&search_is_running),
                depth,
                |msg| println!("{}", msg),
            );

            send_uci!(
                "bestmove {}",
                best_move
                    .as_ref()
                    .map(Move::uci_notation)
                    .unwrap_or_else(|| "none".to_string()),
            );

            search_is_running.store(false, Relaxed);
        }
    });

    Ok(thread)
}

fn command_position(data: &mut Data, terms: &mut SplitAsciiWhitespace<'_>) -> Result<()> {
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
                    if game.length() >= MAX_MOVES_GAME {
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
