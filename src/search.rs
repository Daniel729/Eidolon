use crate::chess::{move_struct::Move, ChessGame, Score};
use arrayvec::ArrayVec;
use nohash_hasher::BuildNoHashHasher;
use std::{
    collections::HashMap,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
    thread,
    time::Duration,
};

pub type TranspositionTable = HashMap<u64, TableEntry, BuildNoHashHasher<u64>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeType {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone, Copy, Debug)]
pub struct TableEntry {
    score: Score,
    pv: Option<Move>,
    depth: u8,
    flag: NodeType,
}

/// Order:
///
/// 1. PV move
/// 2. Killer move
/// 3. Promotion
/// 4. En passant
/// 5. Captures by material exchange
/// 6. Castling
/// 7. Quiet moves by history heuristic
fn move_score(
    _move: Move,
    pv_move: Option<Move>,
    killer_move: Option<Move>,
    history: &[u16; 64 * 12],
) -> u32 {
    if pv_move.is_some_and(|pv_move| pv_move == _move) {
        return 0;
    }

    if killer_move.is_some_and(|killer_move| killer_move == _move) {
        return 1;
    }

    match _move {
        Move::Promotion { new_piece, .. } => 9 - new_piece.material_value() as u32 + 2,
        Move::EnPassant { .. } => 12,
        Move::CastlingLong { .. } => 100000,
        Move::CastlingShort { .. } => 100000,
        Move::Normal {
            captured_piece: capture,
            piece,
            ..
        } => {
            if let Some(captured_piece) = capture {
                1000 + piece.material_value() as u32 - captured_piece.material_value() as u32
            } else {
                10000000 - history[_move.index_history().unwrap()] as u32
            }
        }
    }
}

fn quiescence_search(game: &mut ChessGame, mut alpha: Score, beta: Score, real_depth: u8) -> Score {
    let current_score = game.score * (game.current_player as Score);
    alpha = alpha.max(current_score);

    if alpha >= beta {
        return beta;
    }

    let player = game.current_player;
    let mut moves = ArrayVec::new();

    // It is possible for the game to be a stalemate, but be recognized as a checkmate
    // Because we don't validate the king's moves there due to performance reasons
    game.get_moves(&mut moves, false);
    if moves.is_empty() {
        if game.king_exists(player) && !game.is_targeted(game.get_king_position(player), player) {
            return 0;
        } else {
            // The earlier the mate the worse the score for the losing player
            // This is not a real mate, so it's score reflects that
            return Score::MIN + 3000 + real_depth as Score;
        }
    }

    for _move in &moves {
        let _move = *_move;

        if !_move.is_tactical_move() {
            continue;
        }

        game.push(_move);
        let score = -quiescence_search(game, -beta, -alpha, real_depth + 1);
        game.pop(_move);

        if score > alpha {
            alpha = score;
        }

        if alpha >= beta {
            return beta;
        }
    }

    alpha
}

/// This function exists in order to improve the performance of the search algorithm
/// It's the same as get_best_move_score but with a depth always equal to 1
///
/// Explanation: due to the nature of the search tree (exponential growth), the majority
/// of the time is spent in this function, so it's eliminating unnecessary branches
fn get_best_move_score_depth_1(
    game: &mut ChessGame,
    mut alpha: Score,
    beta: Score,
    real_depth: u8,
) -> Score {
    let player = game.current_player;
    let mut moves = ArrayVec::new();
    game.get_moves(&mut moves, false);

    if moves.is_empty() {
        if game.king_exists(player) && !game.is_targeted(game.get_king_position(player), player) {
            return 0;
        } else {
            // The earlier the mate the worse the score for the losing player
            // This is not a real mate, so it's score reflects that
            return Score::MIN + 2000 + real_depth as Score;
        }
    }

    for _move in &moves {
        let _move = *_move;
        game.push(_move);
        let score = -quiescence_search(game, -beta, -alpha, real_depth + 1);
        game.pop(_move);

        if score > alpha {
            alpha = score;
        }

        if alpha >= beta {
            break;
        }
    }

    alpha
}

/// Core function of the alpha beta search algorithm
/// It halts early and returns None if the should_stop flag is set
/// Otherwise returns the best score for the current player
fn get_best_move_score(
    game: &mut ChessGame,
    table: &mut TranspositionTable,
    should_stop: &AtomicBool, // Flag to stop the search early
    remaining_depth: u8,      // Moves left to search
    real_depth: u8,           // Moves made since root of the search tree
    mut alpha: Score,
    beta: Score,
    killer_moves: &mut [Option<Move>],
    history: &mut [u16; 64 * 12],
) -> Option<Score> {
    if should_stop.load(atomic::Ordering::Relaxed) {
        // Halt the search early
        return None;
    }

    let initial_alpha = alpha;

    let mut pv_move = None;

    if let Some(entry) = table.get(&game.hash()) {
        if entry.depth >= remaining_depth {
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
        pv_move = entry.pv;
    }

    if remaining_depth == 1 {
        return Some(get_best_move_score_depth_1(game, alpha, beta, real_depth));
    } else if remaining_depth == 0 {
        return Some(quiescence_search(game, alpha, beta, real_depth));
    }

    let player = game.current_player;
    let mut moves = ArrayVec::new();
    game.get_moves(&mut moves, true);

    if moves.is_empty() {
        if game.king_exists(player) && !game.is_targeted(game.get_king_position(player), player) {
            return Some(0);
        } else {
            // The earlier the mate the worse the score for the losing player
            return Some(Score::MIN + 100 + real_depth as Score);
        }
    }

    moves.sort_by_cached_key(|a| {
        move_score(*a, pv_move, killer_moves[real_depth as usize], history)
    });

    let mut best_move = None;
    let mut best_score = Score::MIN;

    for (index, _move) in moves.iter().enumerate() {
        let _move = *_move;

        if index <= 2 {
            game.push(_move);
            let score = -get_best_move_score(
                game,
                table,
                should_stop,
                remaining_depth - 1,
                real_depth + 1,
                -beta,
                -alpha,
                killer_moves,
                history,
            )?;
            game.pop(_move);

            if score > best_score {
                best_score = score;
                best_move = Some(_move);
            }

            alpha = alpha.max(score);
        } else {
            game.push(_move);

            let test_score = -get_best_move_score(
                game,
                table,
                should_stop,
                remaining_depth - 1,
                real_depth + 1,
                -alpha - 1,
                -alpha,
                killer_moves,
                history,
            )?;
            game.pop(_move);

            if test_score > best_score {
                game.push(_move);
                let score = -get_best_move_score(
                    game,
                    table,
                    should_stop,
                    remaining_depth - 1,
                    real_depth + 1,
                    -beta,
                    -test_score,
                    killer_moves,
                    history,
                )?;
                game.pop(_move);

                best_move = Some(_move);
                best_score = score;
                alpha = alpha.max(score);
            }
        }

        if alpha >= beta {
            killer_moves[real_depth as usize] = Some(_move);
            if let Some(index) = _move.index_history() {
                let bonus = (remaining_depth as f64).powf(3.0);
                let real_bonus = bonus * (1.0 - history[index] as f64 / 10000.0);
                history[index] += real_bonus as u16;
            }
            break;
        }
    }

    let new_entry = TableEntry {
        score: best_score,
        pv: best_move,
        depth: remaining_depth,
        flag: if best_score <= initial_alpha {
            NodeType::UpperBound
        } else if best_score >= beta {
            NodeType::LowerBound
        } else {
            NodeType::Exact
        },
    };

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

    Some(alpha)
}

/// This function is the entry point for the search algorithm
/// It returns the best move, the score of the best move
/// and a flag indicating if there is only one move available
pub fn get_best_move_entry(
    mut game: ChessGame,
    should_stop: &AtomicBool,
    depth: u8,
    table: &mut TranspositionTable,
    history: &mut [u16; 64 * 12],
) -> Option<(Option<Move>, Score, bool)> {
    let mut moves = ArrayVec::new();
    game.get_moves(&mut moves, true);

    // If there is only one move available don't bother searching
    if moves.len() == 1 {
        return Some((moves.first().copied(), 0, true));
    }

    let mut killer_moves = [None; 32];
    let mut best_move = None;
    let mut best_score = Score::MIN + 1;

    // Prevent threefold repetition
    if game.move_stack.len() >= 5
        && game.move_stack[game.move_stack.len() - 1] == game.move_stack[game.move_stack.len() - 5]
    {
        let repetition_move = game.move_stack[game.move_stack.len() - 4];

        for (index, _move) in moves.iter().enumerate() {
            if repetition_move == *_move {
                moves.swap_remove(index);
                break;
            }
        }
    }
    if let Some(entry) = table.get(&game.hash()) {
        if entry.depth >= depth && entry.flag == NodeType::Exact {
            return Some((entry.pv, entry.score, false));
        }
    }

    let pv_move = table.get(&game.hash()).and_then(|entry| entry.pv);
    moves.sort_by_cached_key(|a| move_score(*a, pv_move, None, history));

    for (index, _move) in moves.iter().enumerate() {
        let _move = *_move;
        if index <= 2 {
            game.push(_move);
            let score = -get_best_move_score(
                &mut game,
                table,
                should_stop,
                depth - 1,
                1,
                Score::MIN + 1,
                -best_score,
                &mut killer_moves,
                history,
            )?;
            game.pop(_move);

            if score > best_score {
                best_score = score;
                best_move = Some(_move);
            }
        } else {
            game.push(_move);

            let score = -get_best_move_score(
                &mut game,
                table,
                should_stop,
                depth - 1,
                1,
                -best_score - 1,
                -best_score,
                &mut killer_moves,
                history,
            )?;
            game.pop(_move);

            if score > best_score {
                game.push(_move);
                let score2 = -get_best_move_score(
                    &mut game,
                    table,
                    should_stop,
                    depth - 1,
                    1,
                    Score::MIN + 1,
                    -score,
                    &mut killer_moves,
                    history,
                )?;
                game.pop(_move);

                best_score = score2;
                best_move = Some(_move);
            }
        }
    }

    let new_entry = TableEntry {
        score: best_score,
        pv: best_move,
        depth,
        flag: NodeType::Exact,
    };

    table
        .entry(game.hash())
        .and_modify(|entry| {
            if entry.depth <= depth {
                *entry = new_entry;
            }
        })
        .or_insert(new_entry);

    Some((best_move, best_score, false))
}

/// This function repeatedly calls get_best_move with increasing depth,
/// until the time limit is reached, at which point it returns the best move found so far
pub fn get_best_move_in_time(
    game: &ChessGame,
    duration: Duration,
    table: &mut TranspositionTable,
    log: bool,
) -> Option<Move> {
    let mut found_move = None;

    // Stop searching after the duration has passed
    let should_stop = Arc::new(AtomicBool::new(false));
    thread::spawn({
        let should_stop = should_stop.clone();
        move || {
            thread::sleep(duration);
            should_stop.store(true, atomic::Ordering::Relaxed);
        }
    });

    let mut history = [0; 64 * 12];

    let starting_depth = table
        .get(&game.hash())
        .map(|entry| {
            if entry.flag == NodeType::Exact {
                entry.depth
            } else {
                5
            }
        })
        .unwrap_or(5);

    for depth in starting_depth.. {
        let Some((best_move, best_score, is_only_move)) = get_best_move_entry(
            game.clone(),
            should_stop.as_ref(),
            depth,
            table,
            &mut history,
        ) else {
            return found_move;
        };

        let mut hash = game.hash();
        let mut game_clone = game.clone();

        found_move = best_move;
        if log {
            print!("info pv ");
            for _ in 0..depth {
                if let Some(entry) = table.get(&hash) {
                    if let Some(pv) = entry.pv {
                        game_clone.push(pv);
                        print!("{} ", pv.uci_notation());
                        hash = game_clone.hash();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            println!();
            println!("info depth {}", depth);
            println!("info score cp {}", best_score);
            println!("info nodes {}", table.len());
        }

        // If mate can be forced, or there is only a single move available, stop searching
        if is_only_move || best_score > Score::MAX - 1000 || best_score < Score::MIN + 1000 {
            return found_move;
        }
    }

    unreachable!()
}
