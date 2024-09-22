// #![feature(str_split_whitespace_remainder)]

mod autoplay;
mod benchmark;
mod chess_game;
mod gamestate;
mod move_struct;
mod performance_test;
mod piece;
mod position;
mod scores;
mod search;
mod uci;

use arrayvec::ArrayVec;
use chess_game::ChessGame;
use move_struct::Move;

fn get_parameter<T>(args: &mut std::env::Args, default: T) -> T
where
    T: std::str::FromStr + std::string::ToString,
{
    args.next()
        .unwrap_or_else(|| default.to_string())
        .parse()
        .unwrap_or(default)
}

fn main() {
    let mut args = std::env::args();
    args.next();

    if let Some(arg) = args.next() {
        if arg == "bench" {
            // Generate best moves for a couple different positions
            // This is used for benchmarking and PGO optimization
            let depth = get_parameter(&mut args, 7);
            let steps = get_parameter(&mut args, 5);

            benchmark::run_simple_benchmark(depth, steps);
        } else if arg == "iterbench" {
            // Generate best moves for a couple different positions
            // This is used for benchmarking and PGO optimization
            let depth = get_parameter(&mut args, 7);
            let steps = get_parameter(&mut args, 5);

            benchmark::run_iterative_benchmark(depth, steps);
        } else if arg == "perft" {
            // Generate perft test result
            let depth = get_parameter(&mut args, 7);
            let fen = args.next().unwrap_or_default();
            let mut game = ChessGame::new(&fen).unwrap_or_default();
            while let Some(move_str) = &args.next() {
                let _move = Move::from_uci_notation(move_str, &game).unwrap();
                game.push(_move);
                println!("{}", &game);
            }

            let mut moves = ArrayVec::new();
            game.get_moves(&mut moves, true);

            moves.sort_by_cached_key(|_move| _move.uci_notation());

            let mut sum = 0;
            for _move in moves {
                game.push(_move);
                let count = performance_test::perft(&mut game, depth - 1);
                game.pop(_move);
                sum += count;
                println!("{}: {}", _move.uci_notation(), count);
            }
            println!("");
            println!("{}", sum);
        } else if arg == "auto" {
            // Auto play in terminal
            let millis = get_parameter(&mut args, 1000);
            autoplay::autoplay(millis);
        }
    } else {
        // Enter UCI mode
        uci::uci_talk();
    }
}
