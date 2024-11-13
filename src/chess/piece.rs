use super::move_struct::Move;
use super::position::Position;
use super::zobrist;
use super::Score;
use super::{ChessGame, Players};
use std::cell::{Cell, OnceCell};

#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
pub enum PieceTypes {
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
    King,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    pub piece_type: PieceTypes,
    pub owner: Players,
}

impl PieceTypes {
    pub fn material_value(self) -> u8 {
        match self {
            PieceTypes::Pawn => 1,
            PieceTypes::Bishop => 3,
            PieceTypes::Knight => 3,
            PieceTypes::Rook => 5,
            PieceTypes::Queen => 9,
            PieceTypes::King => 100,
        }
    }
}

impl Piece {
    pub fn score(self, pos: Position, scores: &[Cell<&[i16; 64]>; 6]) -> Score {
        let piece_score_array = scores[self.piece_type as usize].get();

        let row = match self.owner {
            Players::White => 7 - pos.row(),
            Players::Black => pos.row(),
        };

        // SAFETY: Position is always valid
        let piece_score = unsafe {
            let position = Position::new_unsafe(row, pos.col());
            *piece_score_array.get_unchecked(position.as_usize())
        };

        piece_score * self.owner as Score
    }
    pub fn material_value(self) -> u8 {
        self.piece_type.material_value()
    }

    #[inline]
    pub fn as_index(self) -> usize {
        // This function is used to index zobrist::PIECE array
        // The return values are between 0 and 11
        let mut index = self.piece_type as usize;
        if self.owner == Players::Black {
            index += 6;
        }
        index
    }

    #[inline]
    pub fn hash(self, pos: Position) -> u64 {
        // SAFETY: self.as_usize() is always in 0..12 and pos.as_usize() is always in 0..64
        unsafe {
            *zobrist::PIECE
                .get_unchecked(pos.as_usize())
                .get_unchecked(self.as_index())
        }
    }

    pub fn get_moves(self, mut push: impl FnMut(Move), game: &ChessGame, pos: Position) {
        macro_rules! search_deltas {
            ( $( $deltas:expr ),* ) => { $ (
                for delta in $deltas {
                    if let Some(new_pos) = pos.add(delta) {
                        let place = game.get_position(new_pos);
                        let _move = Move::Normal {
                            piece: self,
                            start: pos,
                            end: new_pos,
                            captured_piece: place,
                        };

                        if let Some(piece) = place  {
                            if piece.owner != game.current_player {
                                push(_move);
                            }
                            break;
                        }

                        push(_move);
                    } else {
                        break;
                    }
                }
            )* };
        }

        match self.piece_type {
            PieceTypes::Pawn => self.get_pawn_moves(push, game, pos),
            PieceTypes::King => self.get_king_moves(push, game, pos),
            PieceTypes::Knight => self.get_knight_moves(push, game, pos),
            PieceTypes::Rook => {
                search_deltas![
                    (1..).map(|x| (0, x)),
                    (1..).map(|x| (0, -x)),
                    (1..).map(|x| (x, 0)),
                    (1..).map(|x| (-x, 0))
                ];
            }
            PieceTypes::Bishop => {
                search_deltas![
                    (1..).map(|x| (x, x)),
                    (1..).map(|x| (-x, -x)),
                    (1..).map(|x| (x, -x)),
                    (1..).map(|x| (-x, x))
                ];
            }

            PieceTypes::Queen => {
                search_deltas![
                    (1..).map(|x| (0, x)),
                    (1..).map(|x| (0, -x)),
                    (1..).map(|x| (x, 0)),
                    (1..).map(|x| (-x, 0)),
                    (1..).map(|x| (x, x)),
                    (1..).map(|x| (-x, -x)),
                    (1..).map(|x| (x, -x)),
                    (1..).map(|x| (-x, x))
                ];
            }
        }
    }

    fn get_pawn_moves(self, mut push: impl FnMut(Move), game: &ChessGame, pos: Position) {
        let first_row = match self.owner {
            Players::White => 1,
            Players::Black => 6,
        };

        let last_row = match self.owner {
            Players::White => 7,
            Players::Black => 0,
        };

        let en_passant_row = match self.owner {
            Players::White => 4,
            Players::Black => 3,
        };

        let normal_delta = match self.owner {
            Players::White => (1, 0),
            Players::Black => (-1, 0),
        };

        let first_row_delta = match self.owner {
            Players::White => (2, 0),
            Players::Black => (-2, 0),
        };

        // SAFETY: First moves for pawns always exist
        unsafe {
            if pos.row() == first_row
                && game.get_position(pos.add_unsafe(normal_delta)).is_none()
                && game.get_position(pos.add_unsafe(first_row_delta)).is_none()
            {
                push(Move::Normal {
                    piece: self,
                    start: pos,
                    end: pos.add_unsafe(first_row_delta),
                    captured_piece: None,
                });
            }
        }

        let side_deltas = match self.owner {
            Players::White => [(1, 1), (1, -1)],
            Players::Black => [(-1, 1), (-1, -1)],
        };

        if let Some(new_pos) = pos.add(normal_delta) {
            if game.get_position(new_pos).is_none() {
                if last_row == new_pos.row() {
                    for new_piece in [
                        PieceTypes::Queen,
                        PieceTypes::Rook,
                        PieceTypes::Bishop,
                        PieceTypes::Knight,
                    ] {
                        let _move = Move::Promotion {
                            owner: game.current_player,
                            start: pos,
                            end: new_pos,
                            captured_piece: None,
                            new_piece,
                        };
                        push(_move);
                    }
                } else {
                    let _move = Move::Normal {
                        piece: self,
                        start: pos,
                        end: new_pos,
                        captured_piece: None,
                    };
                    push(_move);
                };
            }
        }

        for delta in side_deltas {
            if let Some(new_pos) = pos.add(delta) {
                let place = game.get_position(new_pos);
                if place.is_some_and(|piece| piece.owner != self.owner) {
                    if last_row == new_pos.row() {
                        for new_piece in [
                            PieceTypes::Queen,
                            PieceTypes::Rook,
                            PieceTypes::Bishop,
                            PieceTypes::Knight,
                        ] {
                            let _move = Move::Promotion {
                                owner: game.current_player,
                                start: pos,
                                end: new_pos,
                                captured_piece: place,
                                new_piece,
                            };
                            push(_move);
                        }
                    } else {
                        let _move = Move::Normal {
                            piece: self,
                            start: pos,
                            end: new_pos,
                            captured_piece: place,
                        };
                        push(_move);
                    };
                }
            }
        }

        let valid_en_passant = game.state().en_passant();
        if pos.row() == en_passant_row
            && valid_en_passant < 8
            && i8::abs(valid_en_passant - pos.col()) == 1
        {
            let _move = Move::EnPassant {
                owner: game.current_player,
                start_col: pos.col(),
                end_col: valid_en_passant,
            };
            push(_move);
        }
    }

    fn get_king_moves(self, mut push: impl FnMut(Move), game: &ChessGame, pos: Position) {
        let other_king_pos = game.get_king_position(game.current_player.the_other());
        for delta in [
            (0, 1),
            (0, -1),
            (1, 0),
            (-1, 0),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ] {
            if let Some(new_pos) = pos.add(delta) {
                let place = game.get_position(new_pos);
                if !place.is_some_and(|piece| piece.owner == game.current_player) {
                    // Kings can't move into each other
                    if i8::abs(new_pos.row() - other_king_pos.row()) <= 1
                        && i8::abs(new_pos.col() - other_king_pos.col()) <= 1
                    {
                        continue;
                    }
                    push(Move::Normal {
                        piece: self,
                        start: pos,
                        end: new_pos,
                        captured_piece: place,
                    });
                }
            }
        }
        let state = game.state();
        let (king_side_castling, queen_side_castling) = match game.current_player {
            Players::White => (state.white_king_castling(), state.white_queen_castling()),
            Players::Black => (state.black_king_castling(), state.black_queen_castling()),
        };
        let row = match game.current_player {
            Players::White => 0,
            Players::Black => 7,
        };
        // We may need this value 0, 1, or 2 times so we lazy-initialize it.
        let is_king_targeted = OnceCell::new();
        let king = Position::new_assert(row, 4);
        if king_side_castling {
            let (pos1, pos2) = (Position::new_assert(row, 5), Position::new_assert(row, 6));
            if game.get_position(pos1).is_none()
                && game.get_position(pos2).is_none()
                && !*is_king_targeted.get_or_init(|| game.is_targeted(king, game.current_player))
                && !game.is_targeted(pos1, game.current_player)
                && !game.is_targeted(pos2, game.current_player)
            {
                push(Move::CastlingShort {
                    owner: game.current_player,
                });
            }
        }
        if queen_side_castling {
            let (pos1, pos2, pos3) = (
                Position::new_assert(row, 1),
                Position::new_assert(row, 2),
                Position::new_assert(row, 3),
            );

            if game.get_position(pos1).is_none()
                && game.get_position(pos2).is_none()
                && game.get_position(pos3).is_none()
                && !*is_king_targeted.get_or_init(|| game.is_targeted(king, game.current_player))
                && !game.is_targeted(pos2, game.current_player)
                && !game.is_targeted(pos3, game.current_player)
            {
                push(Move::CastlingLong {
                    owner: game.current_player,
                });
            }
        }
    }

    fn get_knight_moves(self, mut push: impl FnMut(Move), game: &ChessGame, pos: Position) {
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
            if let Some(new_pos) = pos.add(delta) {
                let place = game.get_position(new_pos);
                if !place.is_some_and(|piece| piece.owner == game.current_player) {
                    push(Move::Normal {
                        piece: self,
                        start: pos,
                        end: new_pos,
                        captured_piece: place,
                    });
                }
            }
        }
    }

    pub fn as_char(self) -> char {
        match self.owner {
            Players::White => match self.piece_type {
                PieceTypes::King => '♔',
                PieceTypes::Queen => '♕',
                PieceTypes::Rook => '♖',
                PieceTypes::Bishop => '♗',
                PieceTypes::Knight => '♘',
                PieceTypes::Pawn => '♙',
            },
            Players::Black => match self.piece_type {
                PieceTypes::King => '♚',
                PieceTypes::Queen => '♛',
                PieceTypes::Rook => '♜',
                PieceTypes::Bishop => '♝',
                PieceTypes::Knight => '♞',
                PieceTypes::Pawn => '♟',
            },
        }
    }

    pub fn as_str_pgn(self) -> &'static str {
        match self.piece_type {
            PieceTypes::King => "K",
            PieceTypes::Queen => "Q",
            PieceTypes::Rook => "R",
            PieceTypes::Bishop => "B",
            PieceTypes::Knight => "N",
            PieceTypes::Pawn => "",
        }
    }

    pub fn as_char_ascii(self) -> char {
        let piece = match self.piece_type {
            PieceTypes::King => 'K',
            PieceTypes::Queen => 'Q',
            PieceTypes::Rook => 'R',
            PieceTypes::Bishop => 'B',
            PieceTypes::Knight => 'N',
            PieceTypes::Pawn => 'P',
        };

        match self.owner {
            Players::White => piece,
            Players::Black => piece.to_ascii_lowercase(),
        }
    }

    pub fn from_char_ascii(piece: char) -> Option<Self> {
        let owner = if piece.is_ascii_lowercase() {
            Players::Black
        } else {
            Players::White
        };

        let piece_type = match piece.to_ascii_uppercase() {
            'K' => PieceTypes::King,
            'Q' => PieceTypes::Queen,
            'R' => PieceTypes::Rook,
            'B' => PieceTypes::Bishop,
            'N' => PieceTypes::Knight,
            'P' => PieceTypes::Pawn,
            _ => return None,
        };

        Some(Self { piece_type, owner })
    }
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}
