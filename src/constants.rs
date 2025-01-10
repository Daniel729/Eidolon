use std::sync::OnceLock;

use crate::chess::Score;

/// Size of the transposition table in megabytes.
pub const TT_MEGABYTES: usize = 64;

/// Real capacity obtained at runtime
/// At the moment of writing this, this value should be set to half of the total capacity of the hashtable,
/// which makes sure that no reallocation takes place. This is not a guarantee of the API.
pub static TT_REAL_CAPACITY: OnceLock<usize> = OnceLock::new();

/// Transposition table capacity. This formula is valid at the time of writing this on my machine.
pub const TT_CAPACITY: usize = TT_MEGABYTES * 1024 * 1024 * 7
    / 8
    / size_of::<crate::search::TableEntry>().next_multiple_of(16);

/// Starting depth for iterative deepening.
pub const STARTING_DEPTH: u16 = 1;

/// Number of stored PV moves in each entry of the transposition table.
pub const PVS: usize = 6;

/// Threshold for drawing a game by repetition.
pub const THREEFOLD_NUM: u8 = 2;

/// Centipawn value of threefold repetition draw.
pub const CONTEMPT_FACTOR: Score = 0;

/// Scaling factor for history heuristic values.
pub const HISTORY_SCALE: f64 = 1000.0;

/// Source: https://lichess.org/study/rROPNxQX/NucjwPjN
///
/// Used for unit tests
#[allow(unused)]
pub const TESTING_GAME: &str = "g1f3 g8f6 c2c4 g7g6 b1c3 f8g7 d2d4 e8g8 c1f4 d7d5 d1b3 d5c4
                b3c4 c7c6 e2e4 b8d7 a1d1 d7b6 c4c5 c8g4 f4g5 b6a4 c5a3 a4c3 
                b2c3 f6e4 g5e7 d8b6 f1c4 e4c3 e7c5 f8e8 e1f1 g4e6 c5b6 e6c4 
                f1g1 c3e2 g1f1 e2d4 f1g1 d4e2 g1f1 e2c3 f1g1 a7b6 a3b4 a8a4 
                b4b6 c3d1 h2h3 a4a2 g1h2 d1f2 h1e1 e8e1 b6d8 g7f8 f3e1 c4d5 
                e1f3 f2e4 d8b8 b7b5 h3h4 h7h5 f3e5 g8g7 h2g1 f8c5 g1f1 e4g3 
                f1e1 c5b4 e1d1 d5b3 d1c1 g3e2 c1b1 e2c3 b1c1 a2c2";
