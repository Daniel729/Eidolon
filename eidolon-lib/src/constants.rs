use std::{num::NonZeroUsize, time::Duration};

/// Default size of the transposition table in MiB.
pub const DEFAULT_HASH_SIZE: NonZeroUsize = NonZeroUsize::new(16).unwrap();

/// Default move overhead.
pub const DEFAULT_MOVE_OVERHEAD: Duration = Duration::from_millis(10);

/// Minumum search time
pub const MIN_SEARCH_TIME: Duration = Duration::from_millis(0);

/// Maximum number of moves in a game.
pub const MAX_MOVES_GAME: usize = 500;

/// Maximum search depth.
pub const MAX_SEARCH_DEPTH: usize = 100;

/// Maximum number of pseudo-legal moves in any given position.
pub const MAX_MOVES_POSITION: usize = 256;
