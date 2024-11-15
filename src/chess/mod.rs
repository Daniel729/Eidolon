pub mod move_struct;
pub mod zobrist;

mod gamestate;
mod piece;
mod position;
mod scores;

use anyhow::{bail, Context};
use arrayvec::ArrayVec;
use gamestate::GameState;
use move_struct::Move;
use piece::{Piece, PieceType};
use position::Position;
use scores::ENDGAME_THRESHOLD;
use seq_macro::seq;
use std::cell::Cell;

pub type Score = i16;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum GamePhase {
    Opening,
    // Middle,
    Endgame,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Player {
    White = 1,
    Black = -1,
}

#[derive(Clone)]
pub struct Game {
    score: Score,
    current_player: Player,
    move_stack: Vec<Move>,
    phase: GamePhase,
    hash: u64,
    board: [Option<Piece>; 64],
    past_scores: [Score; 64],
    past_hashes: [u64; 64],
    /// Cells are used here in order to allow the changing of the scores
    /// depending on the game's state, e.g. for the endgame
    ///
    /// WARNING: The order of the scores must match the order of the pieces
    piece_scores: [Cell<&'static [i16; 64]>; 6],
    king_positions: [Position; 2],
    state: ArrayVec<GameState, 512>,
}

impl Player {
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

        let mut hash = 0;
        let mut score = 0;

        let mut board = [None; 64];
        let mut past_scores = [0; 64];
        let mut past_hashes = [0; 64];
        let mut white_king_pos = None;
        let mut black_king_pos = None;
        let piece_scores: [Cell<&[i16; 64]>; 6] = [
            Cell::new(&scores::QUEEN_SCORES),
            Cell::new(&scores::ROOK_SCORES),
            Cell::new(&scores::BISHOP_SCORES),
            Cell::new(&scores::KNIGHT_SCORES),
            Cell::new(&scores::PAWN_SCORES),
            Cell::new(&scores::KING_SCORES_MIDDLE),
        ];

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
                    let piece = Piece::from_char_ascii(piece).with_context(|| "Invalid piece")?;
                    if piece.piece_type == PieceType::King {
                        match piece.owner {
                            Player::White => white_king_pos = Some(Position::new_assert(row, col)),
                            Player::Black => black_king_pos = Some(Position::new_assert(row, col)),
                        }
                    }
                    let position = Position::new_assert(row, col);
                    board[position.as_usize()] = Some(piece);
                    past_scores[position.as_usize()] = piece.score(position, &piece_scores);
                    score += past_scores[position.as_usize()];
                    past_hashes[position.as_usize()] = piece.hash(position);
                    hash ^= past_hashes[position.as_usize()];

                    col += 1;
                }
                empty_count if character.is_ascii_digit() => {
                    let count = (empty_count as u8 - b'0') as i8;
                    for i in 0..count {
                        let position = Position::new_assert(row, col + i);
                        past_hashes[position.as_usize()] = zobrist::EMPTY_PLACE;
                        hash ^= past_hashes[position.as_usize()];
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

        if current_player == Player::Black {
            hash ^= zobrist::BLACK_TO_MOVE;
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

        let Some(white_king_pos) = white_king_pos else {
            bail!("White king not found");
        };

        let Some(black_king_pos) = black_king_pos else {
            bail!("Black king not found");
        };

        let mut game = Self {
            board,
            move_stack: Vec::with_capacity(1000),
            king_positions: [white_king_pos, black_king_pos],
            current_player,
            score,
            hash,
            state: ArrayVec::new(),
            past_scores,
            past_hashes,
            piece_scores,
            phase: GamePhase::Opening,
        };

        game.state.push(state);
        game.hash ^= state.hash();
        game.update_phase();

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

    pub fn score(&self) -> Score {
        self.score
    }

    pub fn move_stack(&self) -> &[Move] {
        &self.move_stack
    }

    pub fn get_position(&self, position: Position) -> Option<Piece> {
        // SAFETY: position is always valid
        unsafe { *self.board.get_unchecked(position.as_usize()) }
    }

    pub fn state(&self) -> GameState {
        // SAFETY: There should always be a valid state
        unsafe { *self.state.last().unwrap_unchecked() }
    }

    fn set_position(&mut self, position: Position, new_place: Option<Piece>) {
        // SAFETY: position is always valid
        let (place, place_score, place_hash) = unsafe {
            (
                self.board.get_unchecked_mut(position.as_usize()),
                self.past_scores.get_unchecked_mut(position.as_usize()),
                self.past_hashes.get_unchecked_mut(position.as_usize()),
            )
        };

        self.hash ^= *place_hash;
        self.score -= *place_score;

        *place = new_place;

        *place_score = place
            .map(|piece| piece.score(position, &self.piece_scores))
            .unwrap_or(0);
        *place_hash = place
            .map(|piece| piece.hash(position))
            .unwrap_or(zobrist::EMPTY_PLACE);

        self.hash ^= *place_hash;
        self.score += *place_score;
    }

    pub fn get_king_position(&self, player: Player) -> Position {
        match player {
            Player::White => self.king_positions[0],
            Player::Black => self.king_positions[1],
        }
    }

    fn set_king_position(&mut self, player: Player, position: Position) {
        match player {
            Player::White => self.king_positions[0] = position,
            Player::Black => self.king_positions[1] = position,
        }
    }

    pub fn push_history(&mut self, _move: Move) {
        self.move_stack.push(_move);
        self.update_phase();
        self.push(_move);
    }

    pub fn push(&mut self, _move: Move) {
        let mut state = self.state();
        state.set_en_passant(8);
        match _move {
            Move::Normal {
                piece,
                start,
                end,
                captured_piece,
            } => {
                self.set_position(start, None);
                self.set_position(end, Some(piece));

                if piece.piece_type == PieceType::King {
                    self.set_king_position(self.current_player, end);
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
                if piece.piece_type == PieceType::Pawn && i8::abs(end.row() - start.row()) == 2 {
                    // Check if there are enemy pawns that could capture en passant
                    let mut enemy_pawns_exist = false;

                    if end.col() > 0 {
                        enemy_pawns_exist |= self
                            .get_position(Position::new_assert(end.row(), end.col() - 1))
                            .is_some_and(|p| {
                                p.piece_type == PieceType::Pawn && p.owner != piece.owner
                            });
                    }

                    if end.col() < 7 {
                        enemy_pawns_exist |= self
                            .get_position(Position::new_assert(end.row(), end.col() + 1))
                            .is_some_and(|p| {
                                p.piece_type == PieceType::Pawn && p.owner != piece.owner
                            });
                    }

                    if enemy_pawns_exist {
                        state.set_en_passant(start.col());
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
                self.set_position(start, None);
                self.set_position(
                    end,
                    Some(Piece {
                        owner,
                        piece_type: new_piece,
                    }),
                );

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
                self.set_position(taken_pawn, None);
                self.set_position(old_pawn, None);
                self.set_position(
                    new_pawn,
                    Some(Piece {
                        piece_type: PieceType::Pawn,
                        owner,
                    }),
                );
            }
            Move::CastlingLong { owner } => {
                let row = match owner {
                    Player::White => 0,
                    Player::Black => 7,
                };
                let (old_king, new_king, old_rook, new_rook) = (
                    Position::new_assert(row, 4),
                    Position::new_assert(row, 2),
                    Position::new_assert(row, 0),
                    Position::new_assert(row, 3),
                );

                self.set_position(old_rook, None);
                self.set_position(old_king, None);
                self.set_position(
                    new_rook,
                    Some(Piece {
                        piece_type: PieceType::Rook,
                        owner,
                    }),
                );

                self.set_position(
                    new_king,
                    Some(Piece {
                        piece_type: PieceType::King,
                        owner,
                    }),
                );
                self.set_king_position(self.current_player, new_king);
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
                let row = match owner {
                    Player::White => 0,
                    Player::Black => 7,
                };
                let (old_king, new_king, old_rook, new_rook) = (
                    Position::new_assert(row, 4),
                    Position::new_assert(row, 6),
                    Position::new_assert(row, 7),
                    Position::new_assert(row, 5),
                );

                self.set_position(old_rook, None);
                self.set_position(old_king, None);
                self.set_position(
                    new_rook,
                    Some(Piece {
                        piece_type: PieceType::Rook,
                        owner,
                    }),
                );

                self.set_position(
                    new_king,
                    Some(Piece {
                        piece_type: PieceType::King,
                        owner,
                    }),
                );

                self.set_king_position(self.current_player, new_king);
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
        self.hash ^= self.state().hash(); // SAFETY: The game will not be longer than 512 moves
        unsafe {
            self.state.push_unchecked(state);
        }
        self.hash ^= self.state().hash();
    }

    pub fn pop(&mut self, _move: Move) {
        self.hash ^= self.state().hash();
        // SAFETY: There is always a previous state
        unsafe {
            // self.state.pop() without verification for being empty
            self.state.set_len(self.len() - 1);
        }
        self.hash ^= self.state().hash();
        self.hash ^= zobrist::BLACK_TO_MOVE;
        self.current_player = self.current_player.the_other();

        match _move {
            Move::Normal {
                piece,
                start,
                end,
                captured_piece,
            } => {
                self.set_position(start, Some(piece));
                self.set_position(end, captured_piece);

                if piece.piece_type == PieceType::King {
                    self.set_king_position(self.current_player, start);
                }
            }
            Move::Promotion {
                owner,
                start,
                end,
                captured_piece,
                ..
            } => {
                self.set_position(
                    start,
                    Some(Piece {
                        piece_type: PieceType::Pawn,
                        owner,
                    }),
                );
                self.set_position(end, captured_piece);
            }
            Move::EnPassant {
                owner,
                start_col,
                end_col,
            } => {
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

                self.set_position(new_pawn, None);
                self.set_position(
                    taken_pawn,
                    Some(Piece {
                        piece_type: PieceType::Pawn,
                        owner: owner.the_other(),
                    }),
                );
                self.set_position(
                    old_pawn,
                    Some(Piece {
                        piece_type: PieceType::Pawn,
                        owner,
                    }),
                );
            }
            Move::CastlingLong { owner } => {
                let row = match owner {
                    Player::White => 0,
                    Player::Black => 7,
                };
                let (old_king, new_king, old_rook, new_rook) = (
                    Position::new_assert(row, 4),
                    Position::new_assert(row, 2),
                    Position::new_assert(row, 0),
                    Position::new_assert(row, 3),
                );

                self.set_position(new_rook, None);
                self.set_position(new_king, None);
                self.set_position(
                    old_rook,
                    Some(Piece {
                        piece_type: PieceType::Rook,
                        owner,
                    }),
                );

                self.set_position(
                    old_king,
                    Some(Piece {
                        piece_type: PieceType::King,
                        owner,
                    }),
                );

                self.set_king_position(owner, old_king);
            }
            Move::CastlingShort { owner } => {
                let row = match owner {
                    Player::White => 0,
                    Player::Black => 7,
                };
                let (old_king, new_king, old_rook, new_rook) = (
                    Position::new_assert(row, 4),
                    Position::new_assert(row, 6),
                    Position::new_assert(row, 7),
                    Position::new_assert(row, 5),
                );

                self.set_position(new_rook, None);
                self.set_position(new_king, None);
                self.set_position(
                    old_rook,
                    Some(Piece {
                        piece_type: PieceType::Rook,
                        owner,
                    }),
                );

                self.set_position(
                    old_king,
                    Some(Piece {
                        piece_type: PieceType::King,
                        owner,
                    }),
                );

                self.set_king_position(owner, old_king);
            }
        };
    }

    fn is_endgame(&self) -> bool {
        let mut total_piece_score: u32 = 0;

        for row in 0..8 {
            for col in 0..8 {
                let position = Position::new_assert(row, col);
                if let Some(piece) = self.get_position(position) {
                    total_piece_score +=
                        piece.score(position, &self.piece_scores).unsigned_abs() as u32;
                }
            }
        }

        total_piece_score < 2 * ENDGAME_THRESHOLD // because we are counting both sides
    }

    pub fn update_phase(&mut self) {
        if self.is_endgame() {
            self.piece_scores[PieceType::King as usize].set(&scores::KING_SCORES_END);
            self.phase = GamePhase::Endgame;
        }
    }

    pub fn king_exists(&self, player: Player) -> bool {
        self.get_position(self.get_king_position(player))
            .is_some_and(|piece| piece.piece_type == PieceType::King)
    }

    /// `moves` will be cleared by this function to be sure it has room for all moves
    pub fn get_moves(&mut self, moves: &mut ArrayVec<Move, 256>, verify_king: bool) {
        moves.clear();
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

        seq!(row in 0..8 {
            seq!(col in 0..8 {
                let pos = Position::new_assert(row, col);
                if let Some(piece) = self.get_position(pos) {
                    if piece.owner == self.current_player {
                        piece.get_moves(&mut push, self, pos);
                    }
                }
            });
        });

        // If verify_king then remove moves which put the king in check (invalid moves)
        // We remove invalid moves by overwriting them with the following valid moves
        if verify_king {
            let player = self.current_player;
            let king_position = self.get_king_position(player);
            let is_king_targeted = self.is_targeted(king_position, player);
            let mut keep_index = 0;
            for index in 0..moves.len() {
                let _move = moves[index];

                if !is_king_targeted {
                    if let Move::Normal { start, .. } = _move {
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
            unsafe { moves.set_len(keep_index) };
        }
    }

    /// Returns if player's position is targeted by enemy pieces
    ///
    /// This function is ONLY used for testing castling rights and if a king is in check
    ///
    /// Thus I considered it unnecessary to verify if the square is targeted by a king,
    /// since I already verify that moves don't put kings near each other and a king blocking
    /// a castling move is so unlikely I don't want to waste time on it.
    pub fn is_targeted(&self, position: Position, player: Player) -> bool {
        // Verifiy for kings
        for delta in [
            (1, 0),
            (0, 1),
            (-1, 0),
            (0, -1),
            (1, 1),
            (-1, 1),
            (1, -1),
            (-1, -1),
        ] {
            if let Some(new_pos) = position.add(delta) {
                if self.get_position(new_pos).is_some_and(|piece| {
                    piece.owner != player && piece.piece_type == PieceType::King
                }) {
                    return true;
                }
            }
        }

        // Verify for knights
        for delta in [
            (1, 2),
            (2, 1),
            (-1, -2),
            (-2, -1),
            (1, -2),
            (-2, 1),
            (-1, 2),
            (2, -1),
        ] {
            if let Some(new_pos) = position.add(delta) {
                if self.get_position(new_pos).is_some_and(|piece| {
                    piece.owner != player && piece.piece_type == PieceType::Knight
                }) {
                    return true;
                }
            }
        }

        // Verify for pawns
        match player {
            Player::White => {
                if let Some(new_pos) = position.add((1, 1)) {
                    if self.get_position(new_pos).is_some_and(|piece| {
                        piece.owner != player && piece.piece_type == PieceType::Pawn
                    }) {
                        return true;
                    }
                }
                if let Some(new_pos) = position.add((1, -1)) {
                    if self.get_position(new_pos).is_some_and(|piece| {
                        piece.owner != player && piece.piece_type == PieceType::Pawn
                    }) {
                        return true;
                    }
                }
            }
            Player::Black => {
                if let Some(new_pos) = position.add((-1, 1)) {
                    if self.get_position(new_pos).is_some_and(|piece| {
                        piece.owner != player && piece.piece_type == PieceType::Pawn
                    }) {
                        return true;
                    }
                }
                if let Some(new_pos) = position.add((-1, -1)) {
                    if self.get_position(new_pos).is_some_and(|piece| {
                        piece.owner != player && piece.piece_type == PieceType::Pawn
                    }) {
                        return true;
                    }
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

        // Verify lines for rooks/queens
        search_enemies_loops![
            PieceType::Rook,
            PieceType::Queen,
            (1..).map(|x| (0, x)),
            (1..).map(|x| (0, -x)),
            (1..).map(|x| (x, 0)),
            (1..).map(|x| (-x, 0))
        ];

        // Verify diagonals for bishops/queens
        search_enemies_loops![
            PieceType::Bishop,
            PieceType::Queen,
            (1..).map(|x| (x, x)),
            (1..).map(|x| (-x, -x)),
            (1..).map(|x| (x, -x)),
            (1..).map(|x| (-x, x))
        ];

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
                        result.push(piece.as_char_ascii());
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

        result
    }
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f)?;

        writeln!(f, "Hash: {:x}", self.hash)?;
        writeln!(f, "Fen: {}", self.fen())?;
        writeln!(f, "PGN: {}", self.get_pgn())?;
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
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"
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
