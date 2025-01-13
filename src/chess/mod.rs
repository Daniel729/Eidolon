pub mod move_struct;
pub mod zobrist;

mod bitboard;
mod deltas;
mod gamestate;
mod move_generator;
mod piece;
mod position;
mod scores;

use std::{collections::HashMap, hash::BuildHasherDefault};

use anyhow::{bail, Context};
use arrayvec::ArrayVec;
use gamestate::GameState;
use move_generator::get_moves;
use move_struct::Move;
use nohash_hasher::BuildNoHashHasher;
use piece::{Piece, PieceType};
use position::Position;

pub type Score = i16;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Player {
    White = 1,
    Black = -1,
}

#[derive(Clone, Debug)]
struct GameScores {
    score_mg: Score,
    score_eg: Score,
    game_phase: u8,
}

#[derive(Clone, Debug)]
pub struct BitBoards {
    pieces: [u64; 12],
    colors: [u64; 2],
}

#[derive(Clone)]
pub struct Game {
    scores: GameScores,
    current_player: Player,
    move_stack: ArrayVec<Move, 400>,
    hashes: HashMap<u64, u8, BuildNoHashHasher<u64>>,
    hash: u64,
    board: [Option<Piece>; 64],
    bitboard: BitBoards,
    state: ArrayVec<GameState, 512>,
    generated_moves: u64,
}

impl Player {
    pub fn as_index(&self) -> usize {
        match self {
            Self::White => 0,
            Self::Black => 1,
        }
    }

    pub fn the_other(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

impl Default for Game {
    fn default() -> Self {
        Game::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

impl Game {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut terms = fen.split_ascii_whitespace();

        let mut game = Self {
            scores: GameScores {
                score_mg: 0,
                score_eg: 0,
                game_phase: 0,
            },
            current_player: Player::White,
            board: [None; 64],
            bitboard: BitBoards {
                pieces: [0; 12],
                colors: [0; 2],
            },
            move_stack: ArrayVec::new(),
            hashes: HashMap::with_hasher(BuildHasherDefault::default()),
            hash: 0,
            state: ArrayVec::new(),
            generated_moves: 0,
        };

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

                    game.set_position(position, None, Some(piece));

                    col += 1;
                }

                empty_count if character.is_ascii_digit() => {
                    let count = (empty_count as u8 - b'0') as i8;

                    for i in 0..count {
                        let position = Position::new_assert(row, col + i);
                        game.set_position(position, None, None);
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
            let col = en_passant.chars().nth(0).unwrap();
            state.set_en_passant(((col as u8) - b'a') as i8);
            if !(0..8).contains(&state.en_passant()) {
                bail!("Invalid en passant square");
            }
        }

        game.state.push(state);
        game.hash ^= state.hash();

        game.hashes.insert(game.hash, 1);

        Ok(game)
    }

    pub fn len(&self) -> usize {
        self.state.len()
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn player(&self) -> Player {
        self.current_player
    }

    pub fn bitboard_all(&self) -> u64 {
        self.bitboard.colors[0] | self.bitboard.colors[1]
    }

    pub fn bitboard_opp(&self) -> u64 {
        self.bitboard.colors[self.current_player.the_other().as_index()]
    }

    pub fn bitboard_own(&self) -> u64 {
        self.bitboard.colors[self.current_player.as_index()]
    }

    pub fn bitboard_piece(&self, piece: PieceType, player: Player) -> u64 {
        self.bitboard.pieces[Piece::new(piece, player).as_index()]
    }

    pub fn score(&self) -> Score {
        let mg_score = self.scores.score_mg;
        let eg_score = self.scores.score_eg;

        let mg_phase = self.scores.game_phase.min(24); // in case of early promotion

        let eg_phase = 24 - mg_phase;

        let sum = mg_score as i32 * mg_phase as i32 + eg_score as i32 * eg_phase as i32;

        (sum / 24) as Score
    }

    pub fn get_position(&self, position: Position) -> Option<Piece> {
        self.board[position.as_index()]
    }

    pub fn state(&self) -> GameState {
        // SAFETY: There should always be a valid state
        unsafe { *self.state.last().unwrap_unchecked() }
    }

    pub fn in_check(&self) -> bool {
        let player = self.current_player;

        self.is_targeted(self.get_king_position(player), player)
    }

    pub fn get_king_position(&self, player: Player) -> Position {
        let bitboard = self.bitboard_piece(PieceType::King, player);
        Position::from_bitboard(bitboard)
    }

    pub fn king_exists(&self, player: Player) -> bool {
        self.bitboard_piece(PieceType::King, player) != 0
    }

    pub fn times_seen_position(&self) -> u8 {
        self.hashes.get(&self.hash).copied().unwrap_or(0)
    }

    pub fn set_seen_positions_once(&mut self) {
        self.hashes.iter_mut().for_each(|(_, v)| {
            if *v > 1 {
                *v = 1;
            }
        });
    }

    pub fn generated_moves(&self) -> u64 {
        self.generated_moves
    }

    #[allow(unused)]
    pub fn move_stack(&self) -> &[Move] {
        &self.move_stack
    }

    #[inline(always)]
    fn set_position(
        &mut self,
        position: Position,
        old_place: Option<Piece>,
        new_place: Option<Piece>,
    ) {
        let position_index = position.as_index();

        if let Some(piece) = old_place {
            let piece_index = piece.as_index();
            let owner_index = piece.owner.as_index();

            self.hash ^= zobrist::PIECE[position_index][piece_index];

            self.scores.score_mg -= scores::MG_TABLE[piece_index][position_index];
            self.scores.score_eg -= scores::EG_TABLE[piece_index][position_index];
            self.scores.game_phase -= scores::GAMEPHASE_INC[piece_index];

            self.bitboard.pieces[piece_index] &= !(1 << position_index);
            self.bitboard.colors[owner_index] &= !(1 << position_index);
        }

        if let Some(piece) = new_place {
            let piece_index = piece.as_index();
            let owner_index = piece.owner.as_index();

            self.hash ^= zobrist::PIECE[position_index][piece_index];

            self.scores.score_mg += scores::MG_TABLE[piece_index][position_index];
            self.scores.score_eg += scores::EG_TABLE[piece_index][position_index];
            self.scores.game_phase += scores::GAMEPHASE_INC[piece_index];

            self.bitboard.pieces[piece_index] |= 1 << position_index;
            self.bitboard.colors[owner_index] |= 1 << position_index;
        }

        self.board[position_index] = new_place;
    }

    pub fn push_history(&mut self, _move: Move) {
        self.move_stack.push(_move);
        self.push(_move);

        self.hashes
            .entry(self.hash)
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    pub fn pop_history(&mut self) {
        let hash = self.hash;

        self.hashes.entry(hash).and_modify(|e| *e -= 1);

        if self.hashes[&hash] == 0 {
            self.hashes.remove(&hash);
        }

        let _move = self.move_stack.pop().unwrap();
        self.pop(_move);
    }

    pub fn push_null(&mut self) {
        self.current_player = self.current_player.the_other();

        let mut state = self.state();
        state.set_en_passant(8);

        self.state.push(state);

        self.hash ^= zobrist::BLACK_TO_MOVE;
    }

    pub fn pop_null(&mut self) {
        self.hash ^= zobrist::BLACK_TO_MOVE;

        self.state.pop();

        self.current_player = self.current_player.the_other();
    }

    pub fn push(&mut self, _move: Move) {
        let mut state = self.state();
        state.set_en_passant(8);
        match _move {
            Move::Quiet { piece, start, end } => {
                self.set_position(start, Some(piece), None);
                self.set_position(end, None, Some(piece));

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

                        enemy_pawns_exist |= self
                            .bitboard_piece(PieceType::Pawn, piece.owner.the_other())
                            & bitboard
                            != 0;
                    }

                    if end.col() < 7 {
                        let position = Position::new_assert(end.row(), end.col() + 1);
                        let bitboard = 1 << position.as_index();

                        enemy_pawns_exist |= self
                            .bitboard_piece(PieceType::Pawn, piece.owner.the_other())
                            & bitboard
                            != 0;
                    }

                    if enemy_pawns_exist {
                        state.set_en_passant(start.col());
                    }
                }
            }
            Move::Capture {
                piece,
                start,
                end,
                capture,
            } => {
                self.set_position(start, Some(piece), None);
                self.set_position(end, Some(capture), Some(piece));

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
            Move::Promotion {
                owner,
                start,
                end,
                new_piece,
                captured_piece,
            } => {
                self.set_position(start, Some(Piece::new(PieceType::Pawn, owner)), None);
                self.set_position(end, captured_piece, Some(Piece::new(new_piece, owner)));

                if captured_piece.is_some_and(|piece| {
                    piece.piece_type == PieceType::Rook && piece.owner == Player::White
                }) {
                    match end {
                        Position::WHITE_QUEEN_ROOK => state.set_white_queen_castling_false(),
                        Position::WHITE_KING_ROOK => state.set_white_king_castling_false(),
                        _ => (),
                    }
                }
                if captured_piece.is_some_and(|piece| {
                    piece.piece_type == PieceType::Rook && piece.owner == Player::Black
                }) {
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
                assert!(start_col >= 0 && start_col <= 7 && end_col >= 0 && end_col <= 7);

                let (old_pawn, new_pawn, taken_pawn) = match owner {
                    Player::White => (
                        Position::new_assert(4, start_col),
                        Position::new_assert(5, end_col),
                        Position::new_assert(4, end_col),
                    ),
                    Player::Black => (
                        Position::new_assert(3, start_col),
                        Position::new_assert(2, end_col),
                        Position::new_assert(3, end_col),
                    ),
                };
                self.set_position(
                    taken_pawn,
                    Some(Piece::new(PieceType::Pawn, owner.the_other())),
                    None,
                );
                self.set_position(old_pawn, Some(Piece::new(PieceType::Pawn, owner)), None);
                self.set_position(new_pawn, None, Some(Piece::new(PieceType::Pawn, owner)));
            }
            Move::CastlingLong { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_LONG_OLD_KING[owner_index],
                    Position::CASTLING_LONG_NEW_KING[owner_index],
                    Position::CASTLING_LONG_OLD_ROOK[owner_index],
                    Position::CASTLING_LONG_NEW_ROOK[owner_index],
                );

                self.set_position(old_rook, Some(Piece::new(PieceType::Rook, owner)), None);
                self.set_position(old_king, Some(Piece::new(PieceType::King, owner)), None);
                self.set_position(new_rook, None, Some(Piece::new(PieceType::Rook, owner)));
                self.set_position(new_king, None, Some(Piece::new(PieceType::King, owner)));

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

                self.set_position(old_rook, Some(Piece::new(PieceType::Rook, owner)), None);
                self.set_position(old_king, Some(Piece::new(PieceType::King, owner)), None);
                self.set_position(new_rook, None, Some(Piece::new(PieceType::Rook, owner)));
                self.set_position(new_king, None, Some(Piece::new(PieceType::King, owner)));

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
        self.current_player = self.current_player.the_other();
        self.hash ^= zobrist::BLACK_TO_MOVE;
        self.hash ^= self.state().hash();
        // SAFETY: The game will not be longer than 512 moves
        unsafe {
            self.state.push_unchecked(state);
        }
        self.hash ^= self.state().hash();
    }

    pub fn pop(&mut self, _move: Move) {
        self.hash ^= self.state().hash();
        self.state.pop();
        self.hash ^= self.state().hash();
        self.hash ^= zobrist::BLACK_TO_MOVE;
        self.current_player = self.current_player.the_other();

        match _move {
            Move::Quiet { piece, start, end } => {
                self.set_position(start, None, Some(piece));
                self.set_position(end, Some(piece), None);
            }
            Move::Capture {
                piece,
                start,
                end,
                capture,
            } => {
                self.set_position(start, None, Some(piece));
                self.set_position(end, Some(piece), Some(capture));
            }
            Move::Promotion {
                owner,
                start,
                end,
                captured_piece,
                new_piece,
            } => {
                self.set_position(start, None, Some(Piece::new(PieceType::Pawn, owner)));
                self.set_position(end, Some(Piece::new(new_piece, owner)), captured_piece);
            }
            Move::EnPassant {
                owner,
                start_col,
                end_col,
            } => {
                assert!(start_col >= 0 && start_col <= 7 && end_col >= 0 && end_col <= 7);

                let (old_pawn, new_pawn, taken_pawn) = match owner {
                    Player::White => (
                        Position::new_assert(4, start_col),
                        Position::new_assert(5, end_col),
                        Position::new_assert(4, end_col),
                    ),
                    Player::Black => (
                        Position::new_assert(3, start_col),
                        Position::new_assert(2, end_col),
                        Position::new_assert(3, end_col),
                    ),
                };

                self.set_position(new_pawn, Some(Piece::new(PieceType::Pawn, owner)), None);
                self.set_position(
                    taken_pawn,
                    None,
                    Some(Piece::new(PieceType::Pawn, owner.the_other())),
                );
                self.set_position(old_pawn, None, Some(Piece::new(PieceType::Pawn, owner)));
            }
            Move::CastlingLong { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_LONG_OLD_KING[owner_index],
                    Position::CASTLING_LONG_NEW_KING[owner_index],
                    Position::CASTLING_LONG_OLD_ROOK[owner_index],
                    Position::CASTLING_LONG_NEW_ROOK[owner_index],
                );

                self.set_position(new_rook, Some(Piece::new(PieceType::Rook, owner)), None);
                self.set_position(new_king, Some(Piece::new(PieceType::King, owner)), None);
                self.set_position(old_rook, None, Some(Piece::new(PieceType::Rook, owner)));
                self.set_position(old_king, None, Some(Piece::new(PieceType::King, owner)));
            }
            Move::CastlingShort { owner } => {
                let owner_index = owner.as_index();

                let (old_king, new_king, old_rook, new_rook) = (
                    Position::CASTLING_SHORT_OLD_KING[owner_index],
                    Position::CASTLING_SHORT_NEW_KING[owner_index],
                    Position::CASTLING_SHORT_OLD_ROOK[owner_index],
                    Position::CASTLING_SHORT_NEW_ROOK[owner_index],
                );

                self.set_position(new_rook, Some(Piece::new(PieceType::Rook, owner)), None);
                self.set_position(new_king, Some(Piece::new(PieceType::King, owner)), None);
                self.set_position(old_rook, None, Some(Piece::new(PieceType::Rook, owner)));
                self.set_position(old_king, None, Some(Piece::new(PieceType::King, owner)));
            }
        };
    }

    /// `moves` must be empty in order to be filled with moves
    pub fn get_moves_main(&mut self, moves: &mut ArrayVec<Move, 256>) {
        assert!(moves.is_empty());

        self.generated_moves += 1;

        if !self.king_exists(self.current_player) {
            // no available moves;
            return;
        }

        let mut push = |_move| {
            // SAFETY: The number of possible moves on the board at any given time
            // will never exceed the arrays capacity (256)
            unsafe {
                moves.push_unchecked(_move);
            }
        };

        for piece in PieceType::ALL {
            let mut bitboard = self.bitboard_piece(piece, self.current_player);

            while bitboard != 0 {
                let position = Position::from_bitboard(bitboard);

                debug_assert!(self
                    .get_position(position)
                    .is_some_and(|p| p.piece_type == piece && p.owner == self.current_player));

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
                if let Move::Quiet { start, .. } = _move {
                    let delta_col = start.col() - king_position.col();
                    let delta_row = start.row() - king_position.row();
                    if delta_col != 0 && delta_row != 0 && delta_col.abs() != delta_row.abs() {
                        moves[keep_index] = _move;
                        keep_index += 1;
                        continue;
                    }
                }

                if let Move::Capture { start, .. } = _move {
                    let delta_col = start.col() - king_position.col();
                    let delta_row = start.row() - king_position.row();
                    if delta_col != 0 && delta_row != 0 && delta_col.abs() != delta_row.abs() {
                        moves[keep_index] = _move;
                        keep_index += 1;
                        continue;
                    }
                }
            }

            self.push(_move);
            let condition = !self.is_targeted(self.get_king_position(player), player);
            self.pop(_move);
            if condition {
                moves[keep_index] = _move;
                keep_index += 1;
            }
        }

        moves.truncate(keep_index);
    }
    /// `moves` must be empty in order to be filled with moves
    pub fn get_moves_quiescence(&mut self, moves: &mut ArrayVec<Move, 256>) {
        assert!(moves.is_empty());

        self.generated_moves += 1;

        if !self.king_exists(self.current_player) {
            // no available moves;
            return;
        }

        let mut push = |_move: Move| {
            // SAFETY: The number of possible moves on the board at any given time
            // will never exceed the arrays capacity (256)
            // TODO valgrind error ?
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

                debug_assert!(self
                    .get_position(position)
                    .is_some_and(|p| p.piece_type == piece && p.owner == self.current_player));

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
        let opp_knight = self.bitboard_piece(PieceType::Knight, player.the_other());

        if bitboard::KNIGHT_ATTACK[position.as_index()] & opp_knight != 0 {
            return true;
        }

        // Verify for pawns
        let opp_pawn = self.bitboard_piece(PieceType::Pawn, player.the_other());

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

        let opp_pieces = self.bitboard_piece(PieceType::Rook, player.the_other())
            | self.bitboard_piece(PieceType::Queen, player.the_other());

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

        let opp_pieces = self.bitboard_piece(PieceType::Bishop, player.the_other())
            | self.bitboard_piece(PieceType::Queen, player.the_other());

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
        let moves: Vec<_> = self.move_stack.iter().map(Move::pgn_notation).collect();

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
        result.push_str(&((self.move_stack.len() / 2) + 1).to_string());

        result
    }
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f)?;

        writeln!(f, "Hash: {:X}", self.hash)?;
        writeln!(f, "Fen: {}", self.fen())?;
        writeln!(f, "PGN: {}", self.get_pgn())?;
        writeln!(
            f,
            "Eval: mg {}, eg {}, phase {} | final eval {}",
            self.scores.score_mg,
            self.scores.score_eg,
            self.scores.game_phase,
            self.score()
        )?;

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
    use crate::constants::TESTING_GAME;
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
            game.push(_move);

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
        game.push(_move);

        let _move = Move::from_uci_notation("a7a5", &game).unwrap();
        game.push(_move);

        let _move = Move::from_uci_notation("e4e5", &game).unwrap();
        game.push(_move);

        let _move = Move::from_uci_notation("d7d5", &game).unwrap();
        game.push(_move);

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
            game.push(_move);

            let game2 = Game::new(&game.fen()).unwrap();

            assert_eq!(game.hash(), game2.hash());
        }
    }
}
