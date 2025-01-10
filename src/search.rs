use crate::{
    chess::{move_struct::Move, Game, Score},
    constants::{
        CONTEMPT_FACTOR, HISTORY_SCALE, PVS, STARTING_DEPTH, THREEFOLD_NUM, TT_REAL_CAPACITY,
    },
    uci::send_uci,
};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{
    cmp::Reverse,
    panic::set_hook,
    process::exit,
    sync::atomic::{AtomicBool, Ordering::Relaxed},
    time::{Duration, Instant},
};

pub type TranspositionTable = std::collections::HashMap<u64, TableEntry, BuildNoHashHasher<u64>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NodeType {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone, Copy, Debug)]
pub struct TableEntry {
    score: Score,
    pvs: [Option<Move>; PVS],
    depth: u16,
    game_depth: u16,
    flag: NodeType,
}

#[derive(Clone, Debug, Copy)]
pub struct HaltInfo<'a> {
    start: Instant,
    duration: Duration,
    search_is_running: Option<&'a AtomicBool>,
}

pub type KillerMoves = [Option<Move>; 1000];

pub struct HistoryData {
    history: [u32; 64 * 12],
    seen_history: [u32; 64 * 12],
}

impl Default for HistoryData {
    fn default() -> Self {
        Self {
            history: [0; 64 * 12],
            seen_history: [0; 64 * 12],
        }
    }
}

/// This function repeatedly calls get_best_move with increasing depth,
/// until `continue_running` is set to false, at which point it returns the best move found so far
pub fn get_best_move_until_stop(
    game: &Game,
    table: &mut TranspositionTable,
    history: &mut HistoryData,
    duration: Duration,
    search_is_running: Option<&AtomicBool>,
    max_depth: Option<u16>,
) -> Option<Move> {
    set_hook(Box::new(|info| {
        send_uci!("info error {}", info);

        exit(1);
    }));

    // Restore history heuristic values while reatining a small part

    history.history.iter_mut().for_each(|h| *h /= 100);
    history.seen_history.iter_mut().for_each(|h| *h /= 100);

    let mut game = game.clone();

    // Handle base case (checkmate/stalemate/threefold repetition and a single move available)

    if game.times_seen_position() >= 3 {
        send_uci!("info error threefold repetition");
        return None;
    } else {
        // Mark all positions as been seen once
        game.set_seen_positions_once();
    }

    let mut moves = ArrayVec::new();
    game.get_moves_main(&mut moves);

    if moves.is_empty() {
        if game.in_check() {
            send_uci!("info error checkmate");
        } else {
            send_uci!("info error stalemate");
        }
        return None;
    } else if moves.len() == 1 {
        return moves.first().copied();
    }

    let mut killer_moves = [None; 1000];

    let halt_info = HaltInfo {
        start: Instant::now(),
        duration,
        search_is_running,
    };

    let initial_game_len = game.len();

    let mut depth = table.get(&game.hash()).map_or(STARTING_DEPTH, |entry| {
        if entry.flag == NodeType::Exact {
            entry.depth
        } else {
            STARTING_DEPTH
        }
    });

    loop {
        if depth >= 100 {
            send_uci!("info error search depth >= 100");

            let _move = table.get(&game.hash()).and_then(|entry| entry.pvs[0]);

            return _move;
        }

        let search_result = get_best_move_entry(
            &mut game,
            table,
            history,
            &mut killer_moves,
            halt_info,
            depth,
        );

        let (best_score, is_draw) = match search_result {
            SearchResult::Score(score) => (score, false),
            SearchResult::Draw => (0, true),
            SearchResult::HaltedEarly => {
                send_uci!(
                    "info depth {} hashfull {} nodes {} nps {:.0} time {}",
                    depth,
                    table.len() * 1000 / table.capacity(),
                    game.generated_moves(),
                    game.generated_moves() as f64 / halt_info.start.elapsed().as_secs_f64(),
                    halt_info.start.elapsed().as_millis()
                );

                let _move = table.get(&game.hash()).and_then(|entry| entry.pvs[0]);

                _move.unwrap();

                return _move;
            }
        };

        let mut hash = game.hash();

        let is_mate = is_mate(best_score);

        let mut info = format_args!(
            "info depth {} score {} hashfull {} nodes {} nps {:.0} time {} pv ",
            depth,
            if is_mate {
                format_args!(
                    "mate {}",
                    ((MAX_SCORE.unsigned_abs()
                        - best_score.unsigned_abs()
                        - initial_game_len as u16
                        + 1)
                        / 2) as Score
                        * best_score.signum(),
                )
                .to_string()
            } else {
                format_args!("cp {}", best_score).to_string()
            },
            table.len() * 1000 / table.capacity(),
            game.generated_moves(),
            game.generated_moves() as f64 / halt_info.start.elapsed().as_secs_f64(),
            halt_info.start.elapsed().as_millis()
        )
        .to_string();

        for _ in 0..depth {
            let Some(entry) = table.get(&hash) else {
                break;
            };

            let Some(pv) = entry.pvs[0] else {
                break;
            };

            game.push_history(pv);

            info.push_str(&pv.uci_notation());
            info.push(' ');

            if game.times_seen_position() >= THREEFOLD_NUM {
                break;
            }

            hash = game.hash();
        }

        send_uci!("{}", info);

        while game.len() > initial_game_len {
            game.pop_history();
        }

        // Conditions to stop the search beside the time limit
        if max_depth.is_some_and(|d| d == depth) || is_mate || is_draw {
            let _move = table.get(&game.hash()).and_then(|entry| entry.pvs[0]);

            return _move;
        }

        depth += 1;
    }
}

pub enum SearchResult {
    Score(Score),
    Draw,
    HaltedEarly,
}

/// This function is the entry point for the search algorithm
/// It returns the best move, the score of the best move
/// and a flag indicating if there is only one move available
pub fn get_best_move_entry(
    game: &mut Game,
    table: &mut TranspositionTable,
    history: &mut HistoryData,
    killer_moves: &mut KillerMoves,
    halt_info: HaltInfo,
    depth: u16,
) -> SearchResult {
    assert!(game.times_seen_position() < 3);

    let mut pv_moves = [None; PVS];

    if let Some(entry) = table.get(&game.hash()) {
        if entry.depth >= depth && entry.flag == NodeType::Exact && !is_mate(entry.score) {
            let _move = entry.pvs[0].unwrap();

            game.push_history(_move);

            let draw = game.times_seen_position() >= THREEFOLD_NUM;

            game.pop_history();

            // Only use the PV move if it doesn't result in a draw
            if draw {
                // If it does, we can still use it if our expected score is negative
                if entry.score < 0 {
                    return SearchResult::Draw;
                }
            } else {
                return SearchResult::Score(entry.score);
            }
        }

        pv_moves = entry.pvs;
    }

    let mut moves = ArrayVec::new();
    game.get_moves_main(&mut moves);

    // Already handled zero and one move available cases
    assert!(moves.len() > 1);

    let mut halted_early = false;

    macro_rules! search {
        ($depth:expr, $alpha:expr, $beta:expr) => {
            match get_best_move_score(
                game,
                killer_moves,
                table,
                history,
                halt_info,
                $depth,
                $alpha,
                $beta,
            ) {
                Some(score) => -score,
                None => {
                    halted_early = true;
                    break;
                }
            }
        };
    }

    let mut best_moves = [None; PVS];
    let mut best_scores = [MIN_SCORE; PVS];

    let mut alpha = MIN_SCORE;
    let beta = MAX_SCORE;

    moves.sort_by_cached_key(|&_move| move_score(_move, &pv_moves, None, history));

    for (index, &_move) in moves.iter().enumerate() {
        let depth_decrement = decrement(index, moves.len());

        game.push_history(_move);

        if index == 0 {
            let score = search!(depth - 1, -beta, -alpha);

            add_best_move(_move, score, &mut best_moves, &mut best_scores);

            alpha = alpha.max(score);
        } else {
            let test_score = search!(depth.saturating_sub(depth_decrement), -alpha - 1, -alpha);

            if test_score > best_scores[0] {
                let score = search!(depth - 1, -beta, -alpha);

                add_best_move(_move, score, &mut best_moves, &mut best_scores);

                alpha = alpha.max(score);
            }
        }

        game.pop_history();
    }

    if halted_early {
        game.pop_history();
    }

    if !halted_early && best_moves[0].is_some() {
        let new_entry = TableEntry {
            score: best_scores[0],
            pvs: best_moves,
            depth,
            game_depth: game.len() as u16,
            flag: NodeType::Exact,
        };

        free_transposition_table(table, game);

        table
            .entry(game.hash())
            .and_modify(|entry| {
                if entry.depth <= depth {
                    *entry = new_entry;
                }
            })
            .or_insert(new_entry);
    }

    if halted_early {
        SearchResult::HaltedEarly
    } else {
        SearchResult::Score(alpha)
    }
}

/// Core function of the alpha beta search algorithm
/// It halts early and returns None if the should_stop flag is set
/// Otherwise returns the best score for the current player
#[allow(clippy::too_many_arguments)]
fn get_best_move_score(
    game: &mut Game,
    killer_moves: &mut KillerMoves,
    table: &mut TranspositionTable,
    history: &mut HistoryData,
    halt_info: HaltInfo,
    mut remaining_depth: u16, // Moves left to search
    mut alpha: Score,
    beta: Score,
) -> Option<Score> {
    // Threefold repetition
    if game.times_seen_position() >= THREEFOLD_NUM {
        return Some(CONTEMPT_FACTOR);
    }

    let mut pv_moves = [None; PVS];

    if let Some(entry) = table.get(&game.hash()) {
        if entry.depth >= remaining_depth {
            let _move = entry.pvs[0].unwrap();

            game.push_history(_move);

            let draw = game.times_seen_position() >= THREEFOLD_NUM;

            game.pop_history();

            // Only use the PV move if it doesn't result in a draw
            if draw {
                // If it does, we can still use it if our expected score is negative
                if entry.score < CONTEMPT_FACTOR {
                    return Some(CONTEMPT_FACTOR);
                }
            } else {
                match entry.flag {
                    NodeType::Exact => {
                        return Some(entry.score);
                    }
                    NodeType::LowerBound => {
                        if entry.score >= beta {
                            return Some(entry.score);
                        }
                    }
                    NodeType::UpperBound => {
                        if entry.score <= alpha {
                            return Some(entry.score);
                        }
                    }
                }
            }
        }

        pv_moves = entry.pvs;
    }

    if remaining_depth == 0 {
        let score = quiescence_search(game, alpha, beta);

        return Some(score);
    }

    // 5 here seems to provide precision up to 1 ms
    if remaining_depth >= 5
        && (Instant::now().duration_since(halt_info.start) >= halt_info.duration
            || halt_info
                .search_is_running
                .is_some_and(|val| !val.load(Relaxed)))
    {
        // Halt the search early
        return None;
    }

    let mut halted_early = false;

    macro_rules! search {
        ($depth:expr, $alpha:expr, $beta:expr) => {
            match get_best_move_score(
                game,
                killer_moves,
                table,
                history,
                halt_info,
                $depth,
                $alpha,
                $beta,
            ) {
                Some(score) => -score,
                None => {
                    halted_early = true;
                    break;
                }
            }
        };
    }

    let initial_alpha = alpha;

    let mut moves = ArrayVec::new();

    if game.in_check() {
        game.get_moves_main(&mut moves);

        if moves.is_empty() {
            // Checkmate
            // The earlier the mate the worse the score for the losing player
            return Some(MIN_SCORE + game.len() as Score);
        }
    } else {
        // Null move pruning
        if remaining_depth > 3 {
            let mut score = None;

            game.push_null();

            // This is a fake loop which runs only once in order to use the same search macro
            for _ in 0..1 {
                score = Some(search!(remaining_depth.saturating_sub(3), -beta, -beta + 1));
            }

            game.pop_null();

            if halted_early {
                return None;
            }

            // Didn't halt early thus score exists
            let score = score.unwrap();

            if score >= beta {
                return Some(beta);
            }
        }

        game.get_moves_main(&mut moves);

        if moves.is_empty() {
            // Stalemate
            return Some(CONTEMPT_FACTOR);
        } else if moves.len() == 1 {
            // Only one move available, extending depth
            remaining_depth += 1;
        }
    }

    let mut best_moves = [None; PVS];
    let mut best_scores = [MIN_SCORE; PVS];

    moves.sort_by_cached_key(|&_move| {
        move_score(_move, &pv_moves, killer_moves[game.len()], history)
    });

    for (index, &_move) in moves.iter().enumerate() {
        let depth_decrement = decrement(index, moves.len());

        game.push_history(_move);

        if index == 0 {
            let score = search!(remaining_depth - 1, -beta, -alpha);

            add_best_move(_move, score, &mut best_moves, &mut best_scores);

            alpha = alpha.max(score);
        } else {
            let test_score = search!(
                remaining_depth.saturating_sub(depth_decrement),
                -alpha - 1,
                -alpha
            );

            if test_score > best_scores[0] {
                let score = search!(remaining_depth - 1, -beta, -alpha);

                add_best_move(_move, score, &mut best_moves, &mut best_scores);

                alpha = alpha.max(score);
            }
        }

        game.pop_history();

        if !_move.is_tactical_move() {
            let index = _move.index_history();

            history.seen_history[index] += 1;
        }

        if alpha >= beta {
            killer_moves[game.len()] = Some(_move);

            if !_move.is_tactical_move() {
                let index = _move.index_history();

                const MAX_HISTORY: f64 = (u32::MAX / 2) as f64;
                let bonus = (remaining_depth as f64).powf(1.3) * HISTORY_SCALE;
                let real_bonus =
                    bonus.min(MAX_HISTORY) * (1.0 - history.history[index] as f64 / MAX_HISTORY);

                history.history[index] += real_bonus as u32;
            }

            break;
        }
    }

    if halted_early {
        game.pop_history();
    }

    // If the first search didn't finish there is nothing to store
    best_moves[0]?;

    let new_entry = TableEntry {
        score: best_scores[0],
        pvs: best_moves,
        depth: remaining_depth,
        game_depth: game.len() as u16,
        flag: if best_scores[0] <= initial_alpha {
            NodeType::UpperBound
        } else if halted_early || best_scores[0] >= beta {
            NodeType::LowerBound
        } else {
            NodeType::Exact
        },
    };

    free_transposition_table(table, game);

    table
        .entry(game.hash())
        .and_modify(|entry| {
            if entry.depth < remaining_depth
                || (entry.depth == remaining_depth && new_entry.flag == NodeType::Exact)
            {
                *entry = new_entry;
            }
        })
        .or_insert(new_entry);

    if halted_early {
        None
    } else {
        Some(alpha)
    }
}

fn quiescence_search(game: &mut Game, mut alpha: Score, beta: Score) -> Score {
    let current_score = game.score() * (game.player() as Score);
    alpha = alpha.max(current_score);

    if alpha >= beta {
        return alpha;
    }

    let mut moves = ArrayVec::new();
    game.get_moves_quiescence(&mut moves);

    moves.sort_by_key(|&_move| match _move {
        Move::Normal {
            piece,
            captured_piece: capture,
            ..
        } => (
            Reverse(capture.unwrap().material_value()),
            piece.material_value(),
        ),
        Move::Promotion { .. } => (Reverse(u8::MAX), 0),
        Move::EnPassant { .. } => (Reverse(1), 0),
        _ => unreachable!(),
    });

    for &_move in &moves {
        game.push(_move);
        let score = -quiescence_search(game, -beta, -alpha);
        game.pop(_move);

        alpha = alpha.max(score);

        if alpha >= beta {
            break;
        }
    }

    alpha
}

/// Order:
///
/// 1. PV moves
/// 2. Killer move
/// 3. Promotion
/// 4. En passant
/// 5. Captures by material exchange
/// 6. Castling
/// 7. Quiet moves by history heuristic
fn move_score(
    _move: Move,
    pv_move: &[Option<Move>; PVS],
    killer_move: Option<Move>,
    history: &HistoryData,
) -> u32 {
    for (index, &pv) in pv_move.iter().enumerate() {
        if pv.is_some_and(|pv| pv == _move) {
            return 2 * index as u32;
        }
    }

    if let Some(killer_move) = killer_move {
        if killer_move == _move {
            return 1;
        }
    }

    match _move {
        Move::Promotion { new_piece, .. } => PVS as u32 + 9 - new_piece.material_value() as u32 + 2,
        Move::EnPassant { .. } => 900,
        Move::Normal {
            captured_piece: Some(captured_piece),
            piece,
            ..
        } => 1000 - (captured_piece.material_value() as u32) * 10 + piece.material_value() as u32,
        _ => {
            let index = _move.index_history();
            let history_score = history.history[index] / history.seen_history[index].max(1);

            u32::MAX - history_score
        }
    }
}

fn add_best_move(
    _move: Move,
    score: Score,
    best_moves: &mut [Option<Move>; PVS],
    best_scores: &mut [Score; PVS],
) {
    for (i, &best_score) in best_scores.iter().enumerate() {
        if score > best_score {
            for j in (i..PVS - 1).rev() {
                best_scores[j + 1] = best_scores[j];
                best_moves[j + 1] = best_moves[j];
            }

            best_scores[i] = score;
            best_moves[i] = Some(_move);
            break;
        }
    }
}

/// Partially empty transposition if is full, without reallocating.
fn free_transposition_table(table: &mut TranspositionTable, game: &Game) {
    if table.len() < table.capacity() {
        return;
    }

    table
        .try_reserve(TT_REAL_CAPACITY.get().unwrap().saturating_sub(table.len()))
        .unwrap();

    if table.len() < table.capacity() {
        return;
    }

    for min_depth in 1.. {
        table.retain(|_, entry| {
            entry.flag == NodeType::Exact
                || (entry.game_depth + entry.depth >= game.len() as u16 + min_depth)
        });

        table
            .try_reserve(TT_REAL_CAPACITY.get().unwrap().saturating_sub(table.len()))
            .unwrap();

        if table.len() as f64 <= table.capacity() as f64 * 0.75 {
            return;
        }
    }
}

fn decrement(index: usize, total: usize) -> u16 {
    if total < 10 {
        match index {
            ..2 => 1,
            2..5 => 2,
            5..10 => 3,
            10.. => 4,
        }
    } else if total < 20 {
        match index {
            ..2 => 1,
            2..10 => 2,
            10..15 => 3,
            15.. => 4,
        }
    } else {
        match index {
            ..2 => 1,
            2..15 => 2,
            15..20 => 3,
            20.. => 4,
        }
    }
}

const MIN_SCORE: Score = Score::MIN + 100;
const MAX_SCORE: Score = -MIN_SCORE;

const fn is_mate(score: Score) -> bool {
    (MAX_SCORE.unsigned_abs() - score.unsigned_abs()) < 1000
}
