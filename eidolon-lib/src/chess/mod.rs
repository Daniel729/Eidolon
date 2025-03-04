pub mod move_struct;
pub mod player;
pub mod position;
pub mod scores;
pub mod zobrist;

mod bitboard;
mod deltas;
mod gamestate;
mod move_generator;
mod piece;
mod seen_hashmap;
mod weights;

use anyhow::{Context, bail};
use arrayvec::ArrayVec;
use gamestate::GameState;
use move_generator::get_moves;
use move_struct::Move;
use multiversion::multiversion;
use piece::{Piece, PieceType};
use player::Player;
use position::{Column, Position};
use scopeguard::{Always, ScopeGuard};
use scores::{ACC_SCALE, ACCS, NNUE_1_LEN, occupancy_to_bucket, scale_to_cp};
use seen_hashmap::SeenHashMap;
use std::{
    ops::DerefMut,
    simd::{Simd, cmp::SimdOrd, num::SimdInt},
};
use weights::{NNUE_1_BIAS, NNUE_2_WEIGHTS};

use crate::constants::{MAX_MOVES_GAME, MAX_MOVES_POSITION, MAX_SEARCH_DEPTH};

#[derive(Clone, Debug)]
// AVX-512 requires 64-byte alignment
#[repr(align(64))]
struct Accumulator([i16; ACCS]);

// These traits make the wrapper invisible
impl std::ops::Deref for Accumulator {
    type Target = [i16; ACCS];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Accumulator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Debug)]
pub struct BitBoards {
    pieces: [u64; 12],
    colors: [u64; 2],
}

#[derive(Clone)]
pub struct Game {
    /// Player to move
    current_player: Player,

    /// Zobrist hash of current position
    hash: u64,

    /// NNUE accumulator for main perspective
    accumulator_main: Accumulator,

    /// NNUE accumulator for mirrored perspective
    accumulator_mirrored: Accumulator,

    /// Piece-centric representation of the board using 12 + 2 bitboards
    bitboard: BitBoards,

    /// Position-centric representation of the board using a 64-element array
    board: [Option<Piece>; 64],

    /// Game state history, used for castling rights and en passant
    state: ArrayVec<GameState, { MAX_MOVES_GAME + MAX_SEARCH_DEPTH }>,

    /// List of made moves, used for generating PGNs.
    moves: Vec<Move>,

    /// Custom hashmap which stores the times a position was seen
    seen_count: SeenHashMap,

    /// Starting fen, for generating PGN
    starting_fen: String,
}

impl Default for Game {
    fn default() -> Self {
        Game::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

impl Game {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let fen = fen.trim();

        let mut terms = fen.split_ascii_whitespace();

        let mut game = Self {
            current_player: Player::White,
            hash: 0,
            accumulator_main: Accumulator([0; ACCS]),
            accumulator_mirrored: Accumulator([0; ACCS]),
            bitboard: BitBoards {
                pieces: [0; 12],
                colors: [0; 2],
            },
            board: [None; 64],
            state: ArrayVec::new(),
            moves: Vec::new(),
            seen_count: SeenHashMap::new(),
            starting_fen: fen.to_string(),
        };

        // Initialize biases
        game.accumulator_main[..NNUE_1_LEN].copy_from_slice(&NNUE_1_BIAS[..NNUE_1_LEN]);
        game.accumulator_mirrored[..NNUE_1_LEN].copy_from_slice(&NNUE_1_BIAS[..NNUE_1_LEN]);

        let Some(pieces) = terms.next() else {
            bail!("Missing board");
        };

        let mut row = 7;
        let mut col = 0;

        for character in pieces.chars() {
            match character {
                '/' => {
                    if row == 0 {
                        bail!("Too many rows");
                    }
                    col = 0;
                    row -= 1;
                }
                piece if piece.is_ascii_alphabetic() => {
                    if col == 8 {
                        bail!("Too many columns");
                    }

                    let position = Position::new_assert(row, col);
                    let piece = Piece::from_char_ascii(piece).with_context(|| "Invalid piece")?;

                    game.set_position::<true>(position, None, Some(piece));

                    col += 1;
                }

                empty_count if character.is_ascii_digit() => {
                    let count = (empty_count as u8 - b'0') as i8;

                    for i in 0..count {
                        let position = Position::new_assert(row, col + i);
                        game.set_position::<true>(position, None, None);
                    }

                    col += count;
                }
                _ => bail!("Unknown character met"),
            }
        }

        if row != 0 || col != 8 {
            bail!("Invalid board size");
        }

        let Some(next_player) = terms.next() else {
            bail!("Missing player");
        };

        let current_player = match next_player.chars().next().unwrap() {
            'w' => Player::White,
            'b' => Player::Black,
            _ => bail!("Invalid player"),
        };

        game.current_player = current_player;

        if current_player == Player::Black {
            game.hash ^= zobrist::BLACK_TO_MOVE;
        }

        let mut state = GameState::default();

        let Some(castling_rights) = terms.next() else {
            bail!("Missing castling rights");
        };

        for right in castling_rights.chars() {
            match right {
                'K' => state.set_white_king_castling_true(),
                'Q' => state.set_white_queen_castling_true(),
                'k' => state.set_black_king_castling_true(),
                'q' => state.set_black_queen_castling_true(),
                '-' => continue,
                _ => bail!("Invalid castling right"),
            }
        }

        let Some(en_passant) = terms.next() else {
            bail!("Missing en passant");
        };

        if en_passant != "-" {
            let col_char = en_passant.chars().nth(0).unwrap();
            let Some(column) = Column::new(((col_char as u8) - b'a') as i8) else {
                bail!("Invalid en passant square");
            };

            state.set_en_passant(Some(column));
            if !(0..8).contains(&state.en_passant()) {
                bail!("Invalid en passant square");
            }
        }

        game.state.push(state);
        game.hash ^= state.hash();

        game.inc_pos_count();

        Ok(game)
    }

    pub fn length(&self) -> usize {
        self.state.len()
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn player(&self) -> Player {
        self.current_player
    }

    fn bitboard_all(&self) -> u64 {
        self.bitboard.colors[0] | self.bitboard.colors[1]
    }

    fn bitboard_opp(&self) -> u64 {
        self.bitboard.colors[self.current_player.opposite().as_index()]
    }

    fn bitboard_piece(&self, piece: PieceType, player: Player) -> u64 {
        self.bitboard.pieces[Piece::new(piece, player).as_index()]
    }

    fn is_empty(&self, position: Position) -> bool {
        let bitboard = 1 << position.as_index();

        self.bitboard_all() & bitboard == 0
    }

    fn bucket(&self) -> usize {
        let bitboard = self.bitboard_all();

        assert!(bitboard != 0);

        occupancy_to_bucket(bitboard.count_ones())
    }

    fn get_position(&self, position: Position) -> Option<Piece> {
        self.board[position.as_index()]
    }

    fn state(&self) -> GameState {
        // SAFETY: There will always be a valid last state
        unsafe { *self.state.last().unwrap_unchecked() }
    }

    pub fn in_check(&self) -> bool {
        let player = self.current_player;

        self.is_targeted(self.get_king_position(player), player)
    }

    fn get_king_position(&self, player: Player) -> Position {
        let bitboard = self.bitboard_piece(PieceType::King, player);

        assert!(bitboard != 0);

        Position::from_bitboard(bitboard)
    }

    fn king_exists(&self, player: Player) -> bool {
        self.bitboard_piece(PieceType::King, player) != 0
    }

    pub fn twofold_repetition(&self) -> bool {
        self.times_seen_pos() >= 2
    }

    pub fn threefold_repetition(&self) -> bool {
        self.times_seen_pos() >= 3
    }

    // Evaluation with runtime CPU features detection
    pub fn score(&self, relative: bool) -> i16 {
        #[multiversion(targets(
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw+avx512ifma+avx512vbmi+avx512vbmi2+avx512vnni+avx512bitalg+avx512vpopcntdq+avx512vp2intersect+gfni+vaes",
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw",
            "x86_64+avx512f+avx512cd",
            "x86_64+avx2+fma+bmi2",
            "x86_64+avx2",
            "x86_64+sse4.2",
            "aarch64+neon",
        ))]
        fn score_multiversion(game: &Game, relative: bool) -> i16 {
            game._score(relative)
        }

        score_multiversion(self, relative)
    }

    #[inline(always)]
    fn _score(&self, relative: bool) -> i16 {
        let (acc_a, acc_b) = if self.current_player == Player::White {
            (&self.accumulator_main, &self.accumulator_mirrored)
        } else {
            (&self.accumulator_mirrored, &self.accumulator_main)
        };

        let bucket = self.bucket();

        let psqt_score = (acc_a[NNUE_1_LEN + bucket] - acc_b[NNUE_1_LEN + bucket]) / 2;

        let mut collector = 0;

        const VEC_LEN: usize = 64;

        // make sure all weights are used
        const { assert!(NNUE_1_LEN % VEC_LEN == 0) };

        for i in 0..(NNUE_1_LEN / VEC_LEN) {
            let start = i * VEC_LEN;
            let end = (i + 1) * VEC_LEN;

            let acc = Simd::<i16, VEC_LEN>::from_slice(&acc_a[start..end])
                .simd_clamp(Simd::splat(0), Simd::splat(ACC_SCALE as i16))
                .cast::<i32>();

            let w = Simd::<i16, VEC_LEN>::from_slice(&NNUE_2_WEIGHTS[start..end]).cast::<i32>();

            collector += (acc * acc * w).reduce_sum();
        }

        for i in 0..(NNUE_1_LEN / VEC_LEN) {
            let start = i * VEC_LEN;
            let end = (i + 1) * VEC_LEN;

            let acc = Simd::<i16, VEC_LEN>::from_slice(&acc_b[start..end])
                .simd_clamp(Simd::splat(0), Simd::splat(ACC_SCALE as i16))
                .cast::<i32>();

            let w = Simd::<i16, VEC_LEN>::from_slice(
                &NNUE_2_WEIGHTS[NNUE_1_LEN + start..NNUE_1_LEN + end],
            )
            .cast::<i32>();

            collector += (acc * acc * w).reduce_sum();
        }

        let relative_score = psqt_score + (collector / ACC_SCALE / ACC_SCALE) as i16;

        if relative {
            relative_score
        } else {
            relative_score * self.current_player.as_score()
        }
    }

    pub fn display_eval(&self) {
        let (acc_a, acc_b) = if self.current_player == Player::White {
            (&self.accumulator_main, &self.accumulator_mirrored)
        } else {
            (&self.accumulator_mirrored, &self.accumulator_main)
        };

        let bucket = self.bucket();

        let psqt_score = (acc_a[NNUE_1_LEN + bucket] - acc_b[NNUE_1_LEN + bucket]) / 2;

        for i in 0..8 {
            let psqt_score = (acc_a[NNUE_1_LEN + i] - acc_b[NNUE_1_LEN + i]) / 2;
            let scaled_score = scale_to_cp(psqt_score * self.current_player.as_score());

            if i == bucket {
                println!(
                    "PSQT bucket {}:  {: >4}  <-- this bucket is used",
                    i, scaled_score
                );
            } else {
                println!("PSQT bucket {}:  {: >4}", i, scaled_score);
            }
        }

        println!();

        let mut collector = 0;

        for i in 0..NNUE_1_LEN {
            let val_a = acc_a[i].clamp(0, ACC_SCALE as i16) as i32;
            let val_b = acc_b[i].clamp(0, ACC_SCALE as i16) as i32;

            collector += NNUE_2_WEIGHTS[i] as i32 * val_a * val_a;
            collector += NNUE_2_WEIGHTS[NNUE_1_LEN + i] as i32 * val_b * val_b;
        }

        let nnue_score = (collector / ACC_SCALE / ACC_SCALE) as i16;

        let nnue = scale_to_cp(nnue_score * self.current_player.as_score());

        let total = scale_to_cp((psqt_score + nnue_score) * self.current_player.as_score());

        println!("NNUE:           {: >4}", nnue);
        println!("Total:          {: >4}", total);
        println!();
    }

    #[inline(always)]
    fn set_position<const INC_UPDATES: bool>(
        &mut self,
        position: Position,
        old_place: Option<Piece>,
        new_place: Option<Piece>,
    ) {
        let position_index = position.as_index();
        let mirror_position_index = position_index ^ 56;

        if let Some(piece) = old_place {
            let piece_index = piece.as_index();
            let owner_index = piece.owner.as_index();

            let position_bitboard = !(1 << position_index);

            self.bitboard.pieces[piece_index] &= position_bitboard;
            self.bitboard.colors[owner_index] &= position_bitboard;

            self.board[position_index] = None;

            if INC_UPDATES {
                let opposite_piece_index = piece.opposite().as_index();

                self.hash ^= zobrist::PIECE[position_index][piece_index];

                for i in 0..ACCS {
                    self.accumulator_main[i] -=
                        scores::ACCUMULATORS[piece_index][position_index][i];
                }

                for i in 0..ACCS {
                    self.accumulator_mirrored[i] -=
                        scores::ACCUMULATORS[opposite_piece_index][mirror_position_index][i];
                }
            }
        }

        if let Some(piece) = new_place {
            let piece_index = piece.as_index();
            let owner_index = piece.owner.as_index();

            let position_bitboard = 1 << position_index;

            self.bitboard.pieces[piece_index] |= position_bitboard;
            self.bitboard.colors[owner_index] |= position_bitboard;

            self.board[position_index] = Some(piece);

            if INC_UPDATES {
                let opposite_piece_index = piece.opposite().as_index();

                self.hash ^= zobrist::PIECE[position_index][piece_index];

                for i in 0..ACCS {
                    self.accumulator_main[i] +=
                        scores::ACCUMULATORS[piece_index][position_index][i];
                }

                for i in 0..ACCS {
                    self.accumulator_mirrored[i] +=
                        scores::ACCUMULATORS[opposite_piece_index][mirror_position_index][i];
                }
            }
        }
    }

    fn times_seen_pos(&self) -> u8 {
        self.seen_count.get(self.hash)
    }

    fn inc_pos_count(&mut self) {
        self.seen_count.increase(self.hash);
    }

    fn dec_pos_count(&mut self) {
        self.seen_count.decrease(self.hash);
    }

    pub fn push_history(&mut self, _move: Move) {
        self.moves.push(_move);

        self._push_threefold(_move);
    }

    fn _push_threefold(&mut self, _move: Move) {
        self._push::<true>(_move);

        self.inc_pos_count();
    }

    fn _pop_threefold(&mut self, _move: Move) {
        self.dec_pos_count();

        self._pop::<true>(_move);
    }

    fn _push_null(&mut self) {
        self.current_player = self.current_player.opposite();

        let mut state = self.state();
        state.set_en_passant(None);

        self.hash ^= self.state().hash();

        self.state.push(state);

        self.hash ^= self.state().hash();
        self.hash ^= zobrist::BLACK_TO_MOVE;

        self.inc_pos_count();
    }

    fn _pop_null(&mut self) {
        self.dec_pos_count();

        self.hash ^= self.state().hash();
        self.hash ^= zobrist::BLACK_TO_MOVE;

        self.state.pop();

        self.hash ^= self.state().hash();

        self.current_player = self.current_player.opposite();
    }

    #[must_use]
    /// This function returns a wrapper to the game with a move made
    ///
    /// When it is dropped the move is unmade and the game's state completely restored
    pub fn push<const INC_UPDATES: bool>(
        &mut self,
        _move: Move,
    ) -> impl DerefMut<Target = &mut Game> {
        self._push::<INC_UPDATES>(_move);

        ScopeGuard::<_, _, Always>::with_strategy(self, move |game| {
            game._pop::<INC_UPDATES>(_move);
        })
    }

    #[must_use]
    /// This function returns a wrapper to the game with a null move made
    ///
    /// When it is dropped the move is unmade and the game's state completely restored
    pub fn push_null(&mut self) -> impl DerefMut<Target = &mut Game> {
        self._push_null();

        ScopeGuard::<_, _, Always>::with_strategy(self, |game| {
            game._pop_null();
        })
    }

    #[must_use]
    /// This function returns a wrapper to the game with a move made,
    /// and which counts the position in order to detect twofold/threefold repetitions
    ///
    /// When it is dropped the move is unmade and the game's state completely restored
    pub fn push_threefold(&mut self, _move: Move) -> impl DerefMut<Target = &mut Game> {
        self._push_threefold(_move);

        ScopeGuard::<_, _, Always>::with_strategy(self, move |game| {
            game._pop_threefold(_move);
        })
    }

    // Push with runtime CPU features detection
    fn _push<const INC_UPDATES: bool>(&mut self, _move: Move) {
        #[multiversion(targets(
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw+avx512ifma+avx512vbmi+avx512vbmi2+avx512vnni+avx512bitalg+avx512vpopcntdq+avx512vp2intersect+gfni+vaes",
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw",
            "x86_64+avx512f+avx512cd",
            "x86_64+avx2+fma+bmi2",
            "x86_64+avx2",
            "x86_64+sse4.2",
            "aarch64+neon",
        ))]
        fn push_multiversion(game: &mut Game, _move: Move) {
            game.__push::<true>(_move);
        }

        if INC_UPDATES {
            push_multiversion(self, _move);
        } else {
            self.__push::<false>(_move);
        }
    }

    #[inline(always)]
    fn __push<const INC_UPDATES: bool>(&mut self, _move: Move) {
        let mut state = self.state();

        state.set_en_passant(None);

        match _move {
            Move::Quiet { piece, start, end } => {
                self.set_position::<INC_UPDATES>(start, Some(piece), None);
                self.set_position::<INC_UPDATES>(end, None, Some(piece));

                if piece.piece_type == PieceType::King {
                    match self.current_player {
                        Player::White => {
                            state.set_white_king_castling_false();
                            state.set_white_queen_castling_false();
                        }
                        Player::Black => {
                            state.set_black_king_castling_false();
                            state.set_black_queen_castling_false();
                        }
                    }
                } else if piece.piece_type == PieceType::Rook {
                    match start {
                        Position::WHITE_QUEEN_ROOK => state.set_white_queen_castling_false(),
                        Position::WHITE_KING_ROOK => state.set_white_king_castling_false(),
                        Position::BLACK_QUEEN_ROOK => state.set_black_queen_castling_false(),
                        Position::BLACK_KING_ROOK => state.set_black_king_castling_false(),
                        _ => (),
                    }
                }

                if piece.piece_type == PieceType::Pawn && i8::abs(end.row() - start.row()) == 2 {
                    // Check if there are enemy pawns that could capture en passant
                    let mut enemy_pawns_exist = false;

                    if end.col() > 0 {
                        let position = Position::new_assert(end.row(), end.col() - 1);
                        let bitboard = 1 << position.as_index();

                        enemy_pawns_exist |=
                            self.bitboard_piece(PieceType::Pawn, piece.owner.opposite()) & bitboard
                                != 0;
                    }

                    if end.col() < 7 {
                        let position = Position::new_assert(end.row(), end.col() + 1);
                        let bitboard = 1 << position.as_index();

                        enemy_pawns_exist |=
                            self.bitboard_piece(PieceType::Pawn, piece.owner.opposite()) & bitboard
                                != 0;
                    }

                    let end_col = Column::new(end.col()).unwrap();

                    if enemy_pawns_exist {
                        state.set_en_passant(Some(end_col));
                    }
                }
            }
            Move::Capture {
                piece,
                start,
                end,
                capture,
            } => {
                self.set_position::<INC_UPDATES>(start, Some(piece), None);
                self.set_position::<INC_UPDATES>(end, Some(capture), Some(piece));

                if piece.piece_type == PieceType::King {
                    match self.current_player {
                        Player::White => {
                            state.set_white_king_castling_false();
                            state.set_white_queen_castling_false();
                        }
                        Player::Black => {
                            state.set_black_king_castling_false();
                            state.set_black_queen_castling_false();
                        }
                    }
                } else if piece.piece_type == PieceType::Rook {
                    match start {
                        Position::WHITE_QUEEN_ROOK => state.set_white_queen_castling_false(),
                        Position::WHITE_KING_ROOK => state.set_white_king_castling_false(),
                        Position::BLACK_QUEEN_ROOK => state.set_black_queen_castling_false(),
                        Position::BLACK_KING_ROOK => state.set_black_king_castling_false(),
                        _ => (),
                    }
                }

                if capture.piece_type == PieceType::Rook {
                    match capture.owner {
                        Player::White => match end {
                            Position::WHITE_QUEEN_ROOK => state.set_white_queen_castling_false(),
                            Position::WHITE_KING_ROOK => state.set_white_king_castling_false(),
                            _ => (),
                        },
                        Player::Black => match end {
                            Position::BLACK_QUEEN_ROOK => state.set_black_queen_castling_false(),
                            Position::BLACK_KING_ROOK => state.set_black_king_castling_false(),
                            _ => (),
                        },
                    }
                }
            }
            Move::PromotionQuiet {
                new_piece,
                start,
                end,
            } => {
                self.set_position::<INC_UPDATES>(
                    start,
                    Some(Piece::new(PieceType::Pawn, new_piece.owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(end, None, Some(new_piece));
            }
            Move::PromotionCapture {
                new_piece,
                capture,
                start,
                end,
            } => {
                self.set_position::<INC_UPDATES>(
                    start,
                    Some(Piece::new(PieceType::Pawn, new_piece.owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(end, Some(capture), Some(new_piece));

                if capture.piece_type == PieceType::Rook && capture.owner == Player::White {
                    match end {
                        Position::WHITE_QUEEN_ROOK => state.set_white_queen_castling_false(),
                        Position::WHITE_KING_ROOK => state.set_white_king_castling_false(),
                        _ => (),
                    }
                }

                if capture.piece_type == PieceType::Rook && capture.owner == Player::Black {
                    match end {
                        Position::BLACK_QUEEN_ROOK => state.set_black_queen_castling_false(),
                        Position::BLACK_KING_ROOK => state.set_black_king_castling_false(),
                        _ => (),
                    }
                }
            }
            Move::EnPassant {
                owner,
                start_col,
                end_col,
            } => {
                let (old_pawn, new_pawn, taken_pawn) = match owner {
                    Player::White => (
                        Position::new_assert(4, start_col.get()),
                        Position::new_assert(5, end_col.get()),
                        Position::new_assert(4, end_col.get()),
                    ),
                    Player::Black => (
                        Position::new_assert(3, start_col.get()),
                        Position::new_assert(2, end_col.get()),
                        Position::new_assert(3, end_col.get()),
                    ),
                };
                self.set_position::<INC_UPDATES>(
                    taken_pawn,
                    Some(Piece::new(PieceType::Pawn, owner.opposite())),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    old_pawn,
                    Some(Piece::new(PieceType::Pawn, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    new_pawn,
                    None,
                    Some(Piece::new(PieceType::Pawn, owner)),
                );
            }
            Move::CastlingLong { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_LONG_OLD_KING[owner_index],
                    Position::CASTLING_LONG_NEW_KING[owner_index],
                    Position::CASTLING_LONG_OLD_ROOK[owner_index],
                    Position::CASTLING_LONG_NEW_ROOK[owner_index],
                );

                self.set_position::<INC_UPDATES>(
                    old_rook,
                    Some(Piece::new(PieceType::Rook, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    old_king,
                    Some(Piece::new(PieceType::King, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    new_rook,
                    None,
                    Some(Piece::new(PieceType::Rook, owner)),
                );
                self.set_position::<INC_UPDATES>(
                    new_king,
                    None,
                    Some(Piece::new(PieceType::King, owner)),
                );

                match self.current_player {
                    Player::White => {
                        state.set_white_king_castling_false();
                        state.set_white_queen_castling_false();
                    }
                    Player::Black => {
                        state.set_black_king_castling_false();
                        state.set_black_queen_castling_false();
                    }
                }
            }
            Move::CastlingShort { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_SHORT_OLD_KING[owner_index],
                    Position::CASTLING_SHORT_NEW_KING[owner_index],
                    Position::CASTLING_SHORT_OLD_ROOK[owner_index],
                    Position::CASTLING_SHORT_NEW_ROOK[owner_index],
                );

                self.set_position::<INC_UPDATES>(
                    old_rook,
                    Some(Piece::new(PieceType::Rook, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    old_king,
                    Some(Piece::new(PieceType::King, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    new_rook,
                    None,
                    Some(Piece::new(PieceType::Rook, owner)),
                );
                self.set_position::<INC_UPDATES>(
                    new_king,
                    None,
                    Some(Piece::new(PieceType::King, owner)),
                );

                match self.current_player {
                    Player::White => {
                        state.set_white_king_castling_false();
                        state.set_white_queen_castling_false();
                    }
                    Player::Black => {
                        state.set_black_king_castling_false();
                        state.set_black_queen_castling_false();
                    }
                }
            }
        };

        self.current_player = self.current_player.opposite();

        if INC_UPDATES {
            self.hash ^= zobrist::BLACK_TO_MOVE;
            self.hash ^= self.state().hash();
        }

        // SAFETY: The engine will refuse to allow positions with more than MAX_MOVES moves
        unsafe {
            self.state.push_unchecked(state);
        }

        if INC_UPDATES {
            self.hash ^= self.state().hash();
        }
    }

    // Push with runtime CPU features detection
    fn _pop<const INC_UPDATES: bool>(&mut self, _move: Move) {
        #[multiversion(targets(
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw+avx512ifma+avx512vbmi+avx512vbmi2+avx512vnni+avx512bitalg+avx512vpopcntdq+avx512vp2intersect+gfni+vaes",
            "x86_64+avx512f+avx512cd+avx512dq+avx512vl+avx512bw",
            "x86_64+avx512f+avx512cd",
            "x86_64+avx2+fma+bmi2",
            "x86_64+avx2",
            "x86_64+sse4.2",
            "aarch64+neon",
        ))]
        fn pop_multiversion(game: &mut Game, _move: Move) {
            game.__pop::<true>(_move);
        }

        if INC_UPDATES {
            pop_multiversion(self, _move);
        } else {
            self.__pop::<false>(_move);
        }
    }

    #[inline(always)]
    fn __pop<const INC_UPDATES: bool>(&mut self, _move: Move) {
        if INC_UPDATES {
            self.hash ^= self.state().hash();
        }

        self.state.pop();

        if INC_UPDATES {
            self.hash ^= self.state().hash();
            self.hash ^= zobrist::BLACK_TO_MOVE;
        }

        self.current_player = self.current_player.opposite();

        match _move {
            Move::Quiet { piece, start, end } => {
                self.set_position::<INC_UPDATES>(start, None, Some(piece));
                self.set_position::<INC_UPDATES>(end, Some(piece), None);
            }
            Move::Capture {
                piece,
                start,
                end,
                capture,
            } => {
                self.set_position::<INC_UPDATES>(start, None, Some(piece));
                self.set_position::<INC_UPDATES>(end, Some(piece), Some(capture));
            }
            Move::PromotionQuiet {
                new_piece,
                start,
                end,
            } => {
                self.set_position::<INC_UPDATES>(
                    start,
                    None,
                    Some(Piece::new(PieceType::Pawn, new_piece.owner)),
                );
                self.set_position::<INC_UPDATES>(end, Some(new_piece), None);
            }
            Move::PromotionCapture {
                new_piece,
                capture,
                start,
                end,
            } => {
                self.set_position::<INC_UPDATES>(
                    start,
                    None,
                    Some(Piece::new(PieceType::Pawn, new_piece.owner)),
                );
                self.set_position::<INC_UPDATES>(end, Some(new_piece), Some(capture));
            }
            Move::EnPassant {
                owner,
                start_col,
                end_col,
            } => {
                let (old_pawn, new_pawn, taken_pawn) = match owner {
                    Player::White => (
                        Position::new_assert(4, start_col.get()),
                        Position::new_assert(5, end_col.get()),
                        Position::new_assert(4, end_col.get()),
                    ),
                    Player::Black => (
                        Position::new_assert(3, start_col.get()),
                        Position::new_assert(2, end_col.get()),
                        Position::new_assert(3, end_col.get()),
                    ),
                };

                self.set_position::<INC_UPDATES>(
                    new_pawn,
                    Some(Piece::new(PieceType::Pawn, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    taken_pawn,
                    None,
                    Some(Piece::new(PieceType::Pawn, owner.opposite())),
                );
                self.set_position::<INC_UPDATES>(
                    old_pawn,
                    None,
                    Some(Piece::new(PieceType::Pawn, owner)),
                );
            }
            Move::CastlingLong { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_LONG_OLD_KING[owner_index],
                    Position::CASTLING_LONG_NEW_KING[owner_index],
                    Position::CASTLING_LONG_OLD_ROOK[owner_index],
                    Position::CASTLING_LONG_NEW_ROOK[owner_index],
                );

                self.set_position::<INC_UPDATES>(
                    new_rook,
                    Some(Piece::new(PieceType::Rook, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    new_king,
                    Some(Piece::new(PieceType::King, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    old_rook,
                    None,
                    Some(Piece::new(PieceType::Rook, owner)),
                );
                self.set_position::<INC_UPDATES>(
                    old_king,
                    None,
                    Some(Piece::new(PieceType::King, owner)),
                );
            }
            Move::CastlingShort { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_SHORT_OLD_KING[owner_index],
                    Position::CASTLING_SHORT_NEW_KING[owner_index],
                    Position::CASTLING_SHORT_OLD_ROOK[owner_index],
                    Position::CASTLING_SHORT_NEW_ROOK[owner_index],
                );

                self.set_position::<INC_UPDATES>(
                    new_rook,
                    Some(Piece::new(PieceType::Rook, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    new_king,
                    Some(Piece::new(PieceType::King, owner)),
                    None,
                );
                self.set_position::<INC_UPDATES>(
                    old_rook,
                    None,
                    Some(Piece::new(PieceType::Rook, owner)),
                );
                self.set_position::<INC_UPDATES>(
                    old_king,
                    None,
                    Some(Piece::new(PieceType::King, owner)),
                );
            }
        };
    }

    pub fn get_moves_main(&mut self, moves: &mut ArrayVec<Move, MAX_MOVES_POSITION>) {
        assert!(moves.is_empty());

        if !self.king_exists(self.current_player) {
            // no available moves;
            return;
        }

        let mut push = |_move| {
            // SAFETY: The number of possible moves on the board at any given time
            // will never exceed the arrays capacity (MAX_MOVES_POSITION)
            unsafe {
                moves.push_unchecked(_move);
            }
        };

        for piece in PieceType::ALL {
            let mut bitboard = self.bitboard_piece(piece, self.current_player);

            while bitboard != 0 {
                let position = Position::from_bitboard(bitboard);

                get_moves(
                    &mut push,
                    self,
                    position,
                    Piece::new(piece, self.current_player),
                );

                bitboard &= bitboard - 1;
            }
        }

        // Remove moves which put the king in check (invalid moves)
        // We remove invalid moves by overwriting them with the following valid moves
        let player = self.current_player;
        let king_position = self.get_king_position(player);
        let is_king_targeted = self.is_targeted(king_position, player);
        let mut keep_index = 0;

        for index in 0..moves.len() {
            let _move = moves[index];

            if !is_king_targeted {
                match _move {
                    Move::Quiet { start, .. } | Move::Capture { start, .. } => {
                        let delta_col = start.col() - king_position.col();
                        let delta_row = start.row() - king_position.row();

                        // If the start is not aligned at all to the king,
                        // then it cannot be pinned and it's a valid move
                        if delta_col != 0 && delta_row != 0 && delta_col.abs() != delta_row.abs() {
                            moves[keep_index] = _move;
                            keep_index += 1;
                            continue;
                        }
                    }
                    Move::CastlingShort { .. } | Move::CastlingLong { .. } => {
                        // Castling moves have already been validated
                        moves[keep_index] = _move;
                        keep_index += 1;
                        continue;
                    }
                    _ => {}
                }
            }

            let game_with_move = self.push::<false>(_move);
            let condition =
                !game_with_move.is_targeted(game_with_move.get_king_position(player), player);

            if condition {
                moves[keep_index] = _move;
                keep_index += 1;
            }
        }

        moves.truncate(keep_index);
    }

    pub fn get_moves_quiescence(&mut self, moves: &mut ArrayVec<Move, MAX_MOVES_POSITION>) {
        assert!(moves.is_empty());

        if !self.king_exists(self.current_player) {
            // no available moves;
            return;
        }

        let mut push = |_move: Move| {
            // SAFETY: The number of possible moves on the board at any given time
            // will never exceed the arrays capacity (MAX_MOVES_POSITION)
            unsafe {
                if _move.is_tactical_move() {
                    moves.push_unchecked(_move);
                }
            }
        };

        for piece in PieceType::ALL {
            let mut bitboard = self.bitboard_piece(piece, self.current_player);

            while bitboard != 0 {
                let position = Position::from_bitboard(bitboard);

                get_moves(
                    &mut push,
                    self,
                    position,
                    Piece::new(piece, self.current_player),
                );

                bitboard &= bitboard - 1;
            }
        }
    }

    /// Returns if player's position is targeted by enemy pieces
    ///
    /// This function is ONLY used for testing castling rights and if a king is in check
    ///
    /// Thus I considered it unnecessary to verify if the square is targeted by a king,
    /// since I already verify that moves don't put kings near each other and a king blocking
    /// a castling move is so unlikely I don't want to waste time on it.
    fn is_targeted(&self, position: Position, player: Player) -> bool {
        // Verifiy for kings seems unnecessary

        // Verify for knights
        let opp_knight = self.bitboard_piece(PieceType::Knight, player.opposite());

        if bitboard::KNIGHT_ATTACK[position.as_index()] & opp_knight != 0 {
            return true;
        }

        // Verify for pawns
        let opp_pawn = self.bitboard_piece(PieceType::Pawn, player.opposite());

        match player {
            Player::White => {
                if bitboard::PAWN_ATTACK_WHITE[position.as_index()] & opp_pawn != 0 {
                    return true;
                }
            }
            Player::Black => {
                if bitboard::PAWN_ATTACK_BLACK[position.as_index()] & opp_pawn != 0 {
                    return true;
                }
            }
        };

        // Helpful macro
        macro_rules! search_enemies_loops {
            ( $piece_type1:expr, $piece_type2:expr, $( $x:expr ),* ) => {
                $(

                for delta in $x {
                    if let Some(new_pos) = position.add(delta) {
                        if let Some(piece) = self.get_position(new_pos)  {
                            if piece.owner != player &&
                                (piece.piece_type == $piece_type1 || piece.piece_type == $piece_type2) {
                                return true
                            }
                            break;
                        }
                    } else {
                        break;
                    }
                }
                )*
            };
        }

        let opp_pieces = self.bitboard_piece(PieceType::Rook, player.opposite())
            | self.bitboard_piece(PieceType::Queen, player.opposite());

        let attack = bitboard::ROOK_ATTACK[position.as_index()];

        if attack & opp_pieces != 0 {
            // Verify lines for rooks/queens
            search_enemies_loops![
                PieceType::Rook,
                PieceType::Queen,
                deltas::DELTA_ROOK_1,
                deltas::DELTA_ROOK_2,
                deltas::DELTA_ROOK_3,
                deltas::DELTA_ROOK_4
            ];
        }

        let opp_pieces = self.bitboard_piece(PieceType::Bishop, player.opposite())
            | self.bitboard_piece(PieceType::Queen, player.opposite());

        let attack = bitboard::BISHOP_ATTACK[position.as_index()];

        if attack & opp_pieces != 0 {
            // Verify diagonals for bishops/queens
            search_enemies_loops![
                PieceType::Bishop,
                PieceType::Queen,
                deltas::DELTA_BISHOP_1,
                deltas::DELTA_BISHOP_2,
                deltas::DELTA_BISHOP_3,
                deltas::DELTA_BISHOP_4
            ];
        }

        false
    }

    pub fn get_pgn(&self) -> String {
        let moves: Vec<_> = self.moves.iter().map(Move::pgn_notation).collect();

        let mut s = String::new();

        for (i, _move) in moves.iter().enumerate() {
            if i % 2 == 0 {
                s.push_str((i / 2 + 1).to_string().as_str());
                s.push('.');
                s.push(' ');
            }
            s.push_str(_move.as_str());
            s.push(' ');
        }

        s
    }

    pub fn fen(&self) -> String {
        let mut result = String::new();

        // Add board state
        for row in (0..8).rev() {
            let mut empty_count = 0;

            for col in 0..8 {
                let position = Position::new_assert(row, col);
                match self.get_position(position) {
                    None => empty_count += 1,
                    Some(piece) => {
                        if empty_count > 0 {
                            result.push_str(&empty_count.to_string());
                            empty_count = 0;
                        }
                        result.push(piece.as_char());
                    }
                }
            }

            if empty_count > 0 {
                result.push_str(&empty_count.to_string());
            }

            if row > 0 {
                result.push('/');
            }
        }

        // Add current player
        result.push(' ');
        result.push(match self.current_player {
            Player::White => 'w',
            Player::Black => 'b',
        });

        // Add castling rights
        result.push(' ');
        let state = self.state();
        let mut has_castling = false;

        if state.white_king_castling() {
            result.push('K');
            has_castling = true;
        }
        if state.white_queen_castling() {
            result.push('Q');
            has_castling = true;
        }
        if state.black_king_castling() {
            result.push('k');
            has_castling = true;
        }
        if state.black_queen_castling() {
            result.push('q');
            has_castling = true;
        }
        if !has_castling {
            result.push('-');
        }

        // Add en passant square
        result.push(' ');
        if state.en_passant() < 8 {
            let row = match self.current_player {
                Player::White => '6',
                Player::Black => '3',
            };
            result.push((b'a' + state.en_passant() as u8) as char);
            result.push(row);
        } else {
            result.push('-');
        }

        // TODO: Track halfmove clock
        result.push(' ');
        result.push('0');

        // Add fullmove number
        result.push(' ');
        result.push_str(&((self.moves.len() / 2) + 1).to_string());

        result
    }
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f)?;

        writeln!(f, "[FEN \"{}\"]", self.starting_fen)?;
        writeln!(f, "{}", self.get_pgn())?;

        writeln!(f)?;

        writeln!(f, "Hash: {:X}", self.hash)?;
        writeln!(
            f,
            "Final eval (White's POV) {}",
            scale_to_cp(self.score(false))
        )?;
        writeln!(f, "Current FEN: {}", self.fen())?;

        writeln!(f)?;

        for i in (0..8).rev() {
            write!(f, "{} ", i + 1)?;
            for j in 0..8 {
                let position = Position::new_assert(i, j);
                write!(
                    f,
                    "|{}",
                    self.get_position(position)
                        .map(|piece| piece.as_char())
                        .unwrap_or(' ')
                )?;
            }
            writeln!(f, "|")?;
        }
        writeln!(f, "\n   a b c d e f g h")
    }
}

#[cfg(test)]

mod tests {
    use super::*;

    /// Source: https://lichess.org/study/rROPNxQX/NucjwPjN
    const TESTING_GAME: &str = "g1f3 g8f6 c2c4 g7g6 b1c3 f8g7 d2d4 e8g8 c1f4 d7d5 d1b3 d5c4
                b3c4 c7c6 e2e4 b8d7 a1d1 d7b6 c4c5 c8g4 f4g5 b6a4 c5a3 a4c3 
                b2c3 f6e4 g5e7 d8b6 f1c4 e4c3 e7c5 f8e8 e1f1 g4e6 c5b6 e6c4 
                f1g1 c3e2 g1f1 e2d4 f1g1 d4e2 g1f1 e2c3 f1g1 a7b6 a3b4 a8a4 
                b4b6 c3d1 h2h3 a4a2 g1h2 d1f2 h1e1 e8e1 b6d8 g7f8 f3e1 c4d5 
                e1f3 f2e4 d8b8 b7b5 h3h4 h7h5 f3e5 g8g7 h2g1 f8c5 g1f1 e4g3 
                f1e1 c5b4 e1d1 d5b3 d1c1 g3e2 c1b1 e2c3 b1c1 a2c2";

    #[test]
    fn fen_startpos() {
        let game = Game::default();

        assert_eq!(
            game.fen(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        );
    }

    #[test]
    fn fen_whole_game() {
        let mut game = Game::default();

        for _move in TESTING_GAME.split_ascii_whitespace() {
            let _move = Move::from_uci_notation(_move, &game).unwrap();
            game._push::<true>(_move);

            let fen = game.fen();

            let game2 = Game::new(&fen).unwrap();

            let fen2 = game2.fen();

            assert_eq!(fen, fen2);
        }
    }

    #[test]
    fn fen_en_passant() {
        let mut game = Game::default();

        let _move = Move::from_uci_notation("e2e4", &game).unwrap();
        game._push::<true>(_move);

        let _move = Move::from_uci_notation("a7a5", &game).unwrap();
        game._push::<true>(_move);

        let _move = Move::from_uci_notation("e4e5", &game).unwrap();
        game._push::<true>(_move);

        let _move = Move::from_uci_notation("d7d5", &game).unwrap();
        game._push::<true>(_move);

        let fen = game.fen();

        let game2 = Game::new(&fen).unwrap();

        let fen2 = game2.fen();

        assert_eq!(fen, fen2);
    }

    #[test]
    fn check_hashing_consistency() {
        let mut game = Game::default();

        for _move in TESTING_GAME.split_ascii_whitespace() {
            let _move = Move::from_uci_notation(_move, &game).unwrap();
            game._push::<true>(_move);

            let game2 = Game::new(&game.fen()).unwrap();

            assert_eq!(game.hash(), game2.hash());
        }
    }
}
