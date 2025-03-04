use crate::{
    chess::{
        Game,
        move_struct::{Move, compact_move::CompactMove},
        scores::scale_to_cp,
    },
    constants::{MAX_SEARCH_DEPTH, MIN_SEARCH_TIME},
};
use arrayvec::ArrayVec;
use std::{
    cmp::Reverse,
    sync::atomic::{AtomicBool, Ordering::Relaxed},
    time::{Duration, Instant},
};

pub type TranspositionTable = crate::transposition_hashmap::TranspositionHashMap<TableEntry>;

#[derive(Clone, Copy, Debug)]
pub enum NodeType {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone, Copy, Debug)]
pub struct TableEntry {
    pv: CompactMove,
    score: i16,
    search_depth: u8,
    flag: NodeType,
}

const EXACT_BONUS_RELEVANCE: u16 = 100;

#[derive(Clone, Debug, Copy)]
pub struct HaltInfo<'a> {
    pub start: Instant,
    pub duration: Duration,
    pub search_is_running: Option<&'a AtomicBool>,
}

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

#[derive(Debug, Clone, Copy)]
pub struct SearchInfo {
    pub max_depth: u8,
    pub nodes: u64,
}

pub enum SearchTime {
    Infinite,
    Fixed(Duration),
    BestEffort {
        total: Duration,
        inc: Duration,
        overhead: Duration,
        moves_to_go: Option<u16>,
    },
}

pub const MIN_SCORE: i16 = i16::MIN + 100;
pub const MAX_SCORE: i16 = -MIN_SCORE;

const fn is_mate(score: i16) -> bool {
    (MAX_SCORE.unsigned_abs() - score.unsigned_abs()) < 1000
}

pub fn iterative_search_by_time(
    game: &mut Game,
    table: &mut TranspositionTable,
    time: SearchTime,
    search_is_running: Option<&AtomicBool>,
    max_depth: Option<u8>,
    info_closure: fn(&str),
) -> Option<Move> {
    // Handle base case (checkmate/stalemate/threefold repetition and a single move available)

    if game.threefold_repetition() {
        info_closure("info string threefold repetition");
        return None;
    }

    let mut moves = ArrayVec::new();
    game.get_moves_main(&mut moves);

    if moves.is_empty() {
        if game.in_check() {
            info_closure("info string checkmate");
        } else {
            info_closure("info string stalemate");
        }
        return None;
    } else if moves.len() == 1 {
        let best_score = table.get(game.hash()).map(|entry| entry.score).unwrap_or(0);

        let is_mate = is_mate(best_score);

        let info = format!(
            "info depth 1 seldepth 1 score {} hashfull {} nodes 1 nps 1 time 1 pv ",
            if is_mate {
                format!(
                    "mate {}",
                    (MAX_SCORE.unsigned_abs() - best_score.unsigned_abs()).div_ceil(2) as i16
                        * best_score.signum(),
                )
            } else {
                format!("cp {}", scale_to_cp(best_score))
            },
            table.len() * 1000 / table.capacity(),
        );

        info_closure(info.as_str());

        return moves.first().copied();
    }

    /// Maximum fraction of total time to be allocated for the search.
    const FRACTION_MAX_TOTAL_TIME: f64 = 1.0 / 8.0;

    /// Fraction of increment time to be used for a stable search (minimum).
    const FRACTION_INC_STABLE: f64 = 1.0 / 1.75;

    /// Fraction of increment time to be used for an unstable search (minimum).
    const FRACTION_INC_UNSTABLE: f64 = 1.0 / 1.1;

    /// Fraction of total time to be used for a stable search (minimum).
    const FRACTION_TOTAL_TIME_STABLE: f64 = 1.0 / 50.0;

    /// Fraction of total time to be used for an unstable search (minimum).
    const FRACTION_TOTAL_TIME_UNSTABLE: f64 = 1.0 / 20.0;

    let maximum_time = match time {
        SearchTime::Infinite => Duration::from_secs(u64::MAX),
        SearchTime::Fixed(time) => time,
        SearchTime::BestEffort {
            total,
            inc,
            overhead,
            moves_to_go: None,
        } => (inc + total.saturating_sub(inc).mul_f64(FRACTION_MAX_TOTAL_TIME))
            .saturating_sub(overhead)
            .max(MIN_SEARCH_TIME),
        SearchTime::BestEffort {
            total,
            inc,
            overhead,
            moves_to_go: Some(moves),
        } => {
            let time_per_move = total.div_f64(moves as f64) + inc;

            let max_time = total.saturating_sub(overhead);

            time_per_move
                .mul_f64(2.0)
                .saturating_sub(overhead)
                .min(max_time)
        }
    };

    let minimum_time_stable = match time {
        SearchTime::BestEffort {
            inc,
            total,
            overhead,
            moves_to_go: None,
        } => (inc.mul_f64(FRACTION_INC_STABLE) + total.mul_f64(FRACTION_TOTAL_TIME_STABLE))
            .saturating_sub(overhead)
            .max(MIN_SEARCH_TIME),
        SearchTime::BestEffort {
            total,
            inc,
            overhead,
            moves_to_go: Some(moves),
        } => {
            let time_per_move = total.div_f64(moves as f64) + inc;

            time_per_move.mul_f64(0.4).saturating_sub(overhead)
        }
        SearchTime::Infinite => Duration::from_secs(u64::MAX),
        SearchTime::Fixed(time) => time,
    };

    let minimum_time_unstable = match time {
        SearchTime::BestEffort {
            inc,
            total,
            overhead,
            moves_to_go: None,
        } => (inc.mul_f64(FRACTION_INC_UNSTABLE) + total.mul_f64(FRACTION_TOTAL_TIME_UNSTABLE))
            .saturating_sub(overhead)
            .max(MIN_SEARCH_TIME),
        SearchTime::BestEffort {
            total,
            inc,
            overhead,
            moves_to_go: Some(moves),
        } => {
            let time_per_move = total.div_f64(moves as f64) + inc;

            time_per_move.mul_f64(0.8).saturating_sub(overhead)
        }
        SearchTime::Infinite => Duration::from_secs(u64::MAX),
        SearchTime::Fixed(time) => time,
    };

    let halt_info = HaltInfo {
        start: Instant::now(),
        duration: maximum_time,
        search_is_running,
    };

    let mut best_moves: ArrayVec<Move, MAX_SEARCH_DEPTH> = ArrayVec::new();

    let mut search_info = SearchInfo {
        max_depth: 0,
        nodes: 0,
    };

    let mut history_data = HistoryData::default();

    let mut depth = 1;

    loop {
        if depth >= MAX_SEARCH_DEPTH as u8 {
            info_closure("info string search depth >= MAX_SEARCH_DEPTH");

            let _move = table.get(game.hash()).map(|entry| entry.pv.into());

            return _move;
        }

        search_info.max_depth = 0;

        let search_result = main_search(
            game,
            table,
            &mut search_info,
            &mut history_data,
            halt_info,
            0,
            depth,
            MIN_SCORE,
            MAX_SCORE,
        );

        let best_move = table.get(game.hash()).map(|entry| entry.pv.into()).unwrap();

        let Some(best_score) = search_result else {
            return Some(best_move);
        };

        let is_mate = is_mate(best_score);

        let mut info = format!(
            "info depth {} seldepth {} score {} hashfull {} nodes {} nps {:.0} time {} pv ",
            depth,
            search_info.max_depth,
            if is_mate {
                format!(
                    "mate {}",
                    (MAX_SCORE.unsigned_abs() - best_score.unsigned_abs()).div_ceil(2) as i16
                        * best_score.signum(),
                )
            } else {
                format!("cp {}", scale_to_cp(best_score))
            },
            table.len() * 1000 / table.capacity(),
            search_info.nodes,
            search_info.nodes as f64 / halt_info.start.elapsed().as_secs_f64(),
            halt_info.start.elapsed().as_millis()
        );

        fn get_pv(game: &mut Game, table: &mut TranspositionTable, info: &mut String, depth: u8) {
            if depth == 0 || game.threefold_repetition() {
                return;
            }

            let Some(entry) = table.get(game.hash()) else {
                return;
            };

            let _move = entry.pv.into();

            let mut game_with_move = game.push_threefold(_move);

            info.push_str(&_move.uci_notation());
            info.push(' ');

            get_pv(&mut game_with_move, table, info, depth - 1);
        }

        get_pv(game, table, &mut info, depth);

        info_closure(info.as_str());

        let same_best_move = best_moves.iter().rev().take(5).all(|&m| m == best_move);

        let elapsed = halt_info.start.elapsed();

        let time_limit_stop = if same_best_move {
            elapsed >= minimum_time_stable
        } else {
            elapsed >= minimum_time_unstable
        };

        // Conditions to stop the search
        if max_depth.is_some_and(|d| d == depth) || is_mate || time_limit_stop {
            return Some(best_move);
        }

        best_moves.push(best_move);

        depth += 1;
    }
}

/// Core function of the alpha beta search algorithm
#[allow(clippy::too_many_arguments)]
pub fn main_search(
    game: &mut Game,
    table: &mut TranspositionTable,
    search_info: &mut SearchInfo,
    history: &mut HistoryData,
    halt_info: HaltInfo,
    root_depth: u8,          // Depth since the root of the search
    mut remaining_depth: u8, // Moves left to search
    mut alpha: i16,
    beta: i16,
) -> Option<i16> {
    search_info.max_depth = search_info.max_depth.max(root_depth);

    // Avoid searching the same position multiple times
    if game.twofold_repetition() && root_depth > 0 {
        return Some(0);
    }

    let mut pv_move = None;

    if let Some(entry) = table.get(game.hash()) {
        // At low root-depths we can't use a previous mate since it could result in threefold-repetition
        if entry.search_depth >= remaining_depth && (!is_mate(entry.score) || root_depth > 6) {
            let _move = entry.pv.into();

            let game_with_move = game.push_threefold(_move);

            let draw = game_with_move.twofold_repetition();

            // Only use the PV move if it doesn't result in a draw
            if draw {
                // If it does, we can still use it if our expected score is negative
                // and it's not a lower bound
                if entry.score < 0 && !matches!(entry.flag, NodeType::LowerBound) {
                    return Some(0);
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

        pv_move = Some(entry.pv.into());
    }

    let in_check = game.in_check();

    if remaining_depth == 0 {
        if !in_check {
            let score = quiescence_search(game, search_info, alpha, beta, root_depth);

            return Some(score);
        }

        // Check extension before quiescence search
        remaining_depth = 1;
    }

    // 4 here seems to provide precision up to 1 ms
    if remaining_depth >= 4
        && (Instant::now().duration_since(halt_info.start) >= halt_info.duration
            || halt_info
                .search_is_running
                .is_some_and(|val| !val.load(Relaxed)))
    {
        // Halt the search early
        return None;
    }

    let mut best_move = None;
    let mut best_score = MIN_SCORE;

    // Macros to deduplicate code

    macro_rules! search {
        ($game:expr, $depth:expr, $alpha:expr, $beta:expr) => {{
            main_search(
                &mut $game,
                table,
                search_info,
                history,
                halt_info,
                root_depth + 1,
                $depth,
                -$beta,
                -$alpha,
            )
            .map(|score| -score)
        }};
    }

    macro_rules! increase_history {
        ($_move:expr) => {
            if !$_move.is_tactical_move() {
                let index = $_move.index_history();

                history.seen_history[index] += 1;

                if alpha >= beta {
                    const MAX_HISTORY: f64 = (u32::MAX / 2) as f64;
                    let bonus = (remaining_depth as f64).powf(1.5) * 1000.0;
                    let real_bonus = bonus.min(MAX_HISTORY)
                        * (1.0 - history.history[index] as f64 / MAX_HISTORY);

                    history.history[index] += real_bonus as u32;
                }
            }
        };
    }

    macro_rules! insert_in_table {
        ($flag:expr) => {
            let new_entry = TableEntry {
                score: best_score,
                pv: best_move.unwrap().into(),
                search_depth: remaining_depth,
                flag: $flag,
            };

            table.insert(
                new_entry,
                game.hash(),
                (game.length() as u16
                    + remaining_depth as u16
                    + if matches!($flag, NodeType::Exact) {
                        EXACT_BONUS_RELEVANCE
                    } else {
                        0
                    })
                .try_into()
                .unwrap(),
            );
        };
    }

    let initial_alpha = alpha;

    // Null move pruning
    if !in_check && remaining_depth > 3 {
        let mut game_with_move = game.push_null();

        search_info.nodes += 1;

        let search_depth = remaining_depth - 3;

        let score = search!(game_with_move, search_depth, beta - 1, beta)?;

        if score >= beta {
            return Some(score);
        }
    }

    // Testing PV move first
    if let Some(_move) = pv_move {
        let mut game_with_move = game.push_threefold(_move);

        search_info.nodes += 1;

        let score = search!(game_with_move, remaining_depth - 1, alpha, beta)?;

        // restore game state before inserting the move in the table
        drop(game_with_move);

        best_score = score;
        best_move = Some(_move);

        alpha = alpha.max(score);

        increase_history!(_move);

        if alpha >= beta {
            insert_in_table!(NodeType::LowerBound);

            return Some(alpha);
        }
    }

    let mut moves = ArrayVec::new();
    game.get_moves_main(&mut moves);

    if moves.is_empty() {
        if in_check {
            // Checkmate
            // The earlier the mate the worse the score for the losing player
            return Some(MIN_SCORE + root_depth as i16);
        } else {
            // Stalemate
            return Some(0);
        }
    } else if moves.len() == 1 && !in_check {
        // Only one move available, extending depth
        remaining_depth += 1;
    }

    // Order:
    //
    // 0. PV moves - is skipped since it was already tested
    // 1. Promotions
    // 2. Captures by:
    //    1. MVV-LVA - Most Valuable Victim - Least Valuable Aggressor
    //    2. History heuristic if victim and aggressor have the same value
    // 3. Quiet moves by history heuristic
    moves.sort_by_cached_key(|&_move| {
        if pv_move.is_some_and(|pv| pv == _move) {
            return 0;
        }

        let quiet_score = {
            let index = _move.index_history();
            history.history[index] / history.seen_history[index].max(1)
        };

        const SCALE: u32 = 10000;

        match _move {
            Move::PromotionCapture { new_piece, .. } => 1 + 6 - new_piece.value() as u32,
            Move::PromotionQuiet { new_piece, .. } => 2 + 6 - new_piece.value() as u32,
            Move::EnPassant { .. } => {
                let capture_score = 100;

                capture_score * SCALE - quiet_score.min(SCALE)
            }
            Move::Capture { capture, piece, .. } => {
                let capture_score = 100 - (capture.value() as u32) * 6 + piece.value() as u32;

                capture_score * SCALE - quiet_score.min(SCALE)
            }
            _ => u32::MAX - quiet_score,
        }
    });

    for (index, &_move) in moves.iter().enumerate() {
        if index == 0 && pv_move.is_some() {
            // Skip PV move since it was already searched
            continue;
        }

        let mut game_with_move = game.push_threefold(_move);

        search_info.nodes += 1;

        let do_full_search;

        if index == 0 {
            // At least one move has to be fully searched
            do_full_search = true;
        } else {
            // Reduction of the remaining search depth, used for pruning uninteresting moves.
            // - Don't prune tactical moves unless they are losing
            // - Prune late quiet moves more (LMR - late move reduction)
            let decrement = match _move {
                Move::PromotionQuiet { .. }
                | Move::PromotionCapture { .. }
                | Move::EnPassant { .. } => 1,
                Move::Capture { capture, piece, .. } => {
                    if capture.value() >= piece.value() {
                        1
                    } else {
                        2
                    }
                }
                _ => (1 + (index as u8 - 1) / 3).min(4),
            };

            let search_depth = remaining_depth.saturating_sub(decrement);

            let score = search!(game_with_move, search_depth, best_score, best_score + 1)?;

            // Only do a full search if the move seems good with a lower depth null window search
            do_full_search = score > best_score;
        }

        if do_full_search {
            let score = search!(game_with_move, remaining_depth - 1, alpha, beta)?;

            if score > best_score {
                best_score = score;
                best_move = Some(_move);
            }

            alpha = alpha.max(score);
        }

        increase_history!(_move);

        if alpha >= beta {
            break;
        }
    }

    assert!(best_move.is_some());

    let flag = if best_score <= initial_alpha {
        NodeType::UpperBound
    } else if best_score >= beta {
        NodeType::LowerBound
    } else {
        NodeType::Exact
    };

    insert_in_table!(flag);

    Some(alpha)
}

fn quiescence_search(
    game: &mut Game,
    search_info: &mut SearchInfo,
    mut alpha: i16,
    beta: i16,
    root_depth: u8,
) -> i16 {
    search_info.max_depth = search_info.max_depth.max(root_depth);

    let current_score = game.score(true);
    alpha = alpha.max(current_score);

    if alpha >= beta {
        return alpha;
    }

    let mut moves = ArrayVec::new();
    game.get_moves_quiescence(&mut moves);

    moves.sort_by_key(|&_move| match _move {
        Move::Capture { piece, capture, .. } => (Reverse(capture.value()), piece.value()),
        Move::PromotionQuiet { new_piece, .. } => (Reverse(u8::MAX), 6 - new_piece.value()),
        Move::PromotionCapture { new_piece, .. } => (Reverse(u8::MAX), 6 - new_piece.value()),
        Move::EnPassant { .. } => (Reverse(1), 0),
        _ => unreachable!(),
    });

    for &_move in &moves {
        let mut game_with_move = game.push::<true>(_move);
        search_info.nodes += 1;

        let score = -quiescence_search(
            &mut game_with_move,
            search_info,
            -beta,
            -alpha,
            root_depth + 1,
        );

        alpha = alpha.max(score);

        if alpha >= beta {
            break;
        }
    }

    alpha
}
