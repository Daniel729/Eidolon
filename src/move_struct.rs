use std::str::FromStr;

use crate::chess_game::{ChessGame, Players};
use crate::piece::{Piece, PieceTypes};
use crate::position::Position;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Move {
    Normal {
        piece: Piece,
        start: Position,
        end: Position,
        captured_piece: Option<Piece>,
    },
    Promotion {
        owner: Players,
        new_piece: PieceTypes,
        start: Position,
        end: Position,
        captured_piece: Option<Piece>,
    },
    CastlingShort {
        owner: Players,
    },
    CastlingLong {
        owner: Players,
    },
    EnPassant {
        owner: Players,
        start_col: i8,
        end_col: i8,
    },
}

impl Move {
    pub fn is_tactical_move(&self) -> bool {
        match self {
            Self::Normal {
                piece,
                captured_piece,
                ..
            } => captured_piece.is_some_and(|captured_piece| {
                piece.material_value() <= captured_piece.material_value()
            }),
            Self::Promotion { .. } => true,
            Self::EnPassant { .. } => true,
            _ => false,
        }
    }

    // Return the moves index inside history, if it is a quiet move
    pub fn index_history(&self) -> Option<usize> {
        match self {
            Move::Normal {
                piece,
                end,
                captured_piece,
                ..
            } => match captured_piece {
                Some(_) => None,
                None => Some(piece.as_index() * 64 + end.as_usize()),
            },
            _ => None,
        }
    }

    pub fn uci_notation(&self) -> String {
        let mut s = String::new();
        match self {
            Self::Normal { start, end, .. } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
            }
            Self::Promotion {
                start,
                end,
                new_piece,
                ..
            } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
                s.push(match new_piece {
                    PieceTypes::Queen => 'q',
                    PieceTypes::Rook => 'r',
                    PieceTypes::Bishop => 'b',
                    PieceTypes::Knight => 'n',
                    _ => unreachable!(),
                });
            }
            Self::CastlingShort { owner } => {
                let row = match owner {
                    Players::White => '1',
                    Players::Black => '8',
                };
                s.push('e');
                s.push(row);
                s.push('g');
                s.push(row);
            }
            Self::CastlingLong { owner } => {
                let row = match owner {
                    Players::White => '1',
                    Players::Black => '8',
                };
                s.push('e');
                s.push(row);
                s.push('c');
                s.push(row);
            }
            Self::EnPassant {
                owner,
                start_col,
                end_col,
            } => {
                let (start_row, end_row) = match owner {
                    Players::White => ('5', '6'),
                    Players::Black => ('4', '3'),
                };
                s.push((*start_col as u8 + b'a') as char);
                s.push(start_row);
                s.push((*end_col as u8 + b'a') as char);
                s.push(end_row);
            }
        }
        s
    }

    pub fn pgn_notation(&self) -> String {
        match self {
            Self::Normal {
                piece,
                start,
                end,
                captured_piece,
            } => {
                let mut s = String::new();
                s.push_str(piece.as_char_ascii());
                s.push(((start.col()) as u8 + b'a') as char);
                if captured_piece.is_some() {
                    s.push('x');
                }
                s.push(((end.col()) as u8 + b'a') as char);
                s.push_str((end.row() + 1).to_string().as_str());
                s
            }
            Self::CastlingShort { .. } => String::from_str("O-O").unwrap(),
            Self::CastlingLong { .. } => String::from_str("O-O-O").unwrap(),
            Self::EnPassant {
                start_col,
                end_col,
                owner,
            } => {
                let mut s = String::new();
                s.push((*start_col as u8 + b'a') as char);
                s.push('x');
                s.push((*end_col as u8 + b'a') as char);
                match owner {
                    Players::White => s.push('6'),
                    Players::Black => s.push('3'),
                };
                s
            }
            Self::Promotion {
                end,
                captured_piece,
                new_piece,
                ..
            } => {
                let mut s = String::new();
                if captured_piece.is_some() {
                    s.push('x');
                }
                s.push(((end.col()) as u8 + b'a') as char);
                s.push_str((end.row() + 1).to_string().as_str());
                s.push('=');
                s.push(match new_piece {
                    PieceTypes::Queen => 'Q',
                    PieceTypes::Rook => 'R',
                    PieceTypes::Bishop => 'K',
                    PieceTypes::Knight => 'B',
                    _ => unreachable!(),
                });
                s
            }
        }
    }

    pub fn from_uci_notation(s: &str, game: &ChessGame) -> Option<Self> {
        if s == "e1g1" && game.get_king_position(Players::White) == Position::new_assert(0, 4) {
            Some(Self::CastlingShort {
                owner: Players::White,
            })
        } else if s == "e8g8"
            && game.get_king_position(Players::Black) == Position::new_assert(7, 4)
        {
            Some(Self::CastlingShort {
                owner: Players::Black,
            })
        } else if s == "e1c1"
            && game.get_king_position(Players::White) == Position::new_assert(0, 4)
        {
            Some(Self::CastlingLong {
                owner: Players::White,
            })
        } else if s == "e8c8"
            && game.get_king_position(Players::Black) == Position::new_assert(7, 4)
        {
            Some(Self::CastlingLong {
                owner: Players::Black,
            })
        } else {
            let mut chars = s.bytes();
            let start_col = chars.next()?.wrapping_sub(b'a') as i8;
            let start_row = chars.next()?.wrapping_sub(b'1') as i8;
            let end_col = chars.next()?.wrapping_sub(b'a') as i8;
            let end_row = chars.next()?.wrapping_sub(b'1') as i8;

            let start = Position::new(start_row, start_col)?;
            let end = Position::new(end_row, end_col)?;

            if let Some(new_piece) = s.chars().nth(4) {
                let new_piece = match new_piece {
                    'q' | 'Q' => PieceTypes::Queen,
                    'r' | 'R' => PieceTypes::Rook,
                    'n' | 'N' => PieceTypes::Knight,
                    'b' | 'B' => PieceTypes::Bishop,
                    _ => return None,
                };

                return Some(Self::Promotion {
                    owner: game.current_player,
                    start,
                    end,
                    new_piece,
                    captured_piece: game.get_position(end),
                });
            }

            if let Some(piece) = game.get_position(start) {
                // This move is either en passant or normal
                return if piece.piece_type == PieceTypes::Pawn
                    && game.get_position(end).is_none()
                    && i8::abs(start.col() - end.col()) == 1
                {
                    Some(Self::EnPassant {
                        owner: game.current_player,
                        start_col: start.col(),
                        end_col: end.col(),
                    })
                } else {
                    Some(Self::Normal {
                        piece,
                        start,
                        end,
                        captured_piece: game.get_position(end),
                    })
                };
            }

            None
        }
    }
}

impl std::fmt::Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Normal {
                piece,
                start,
                end,
                captured_piece,
            } => write!(
                f,
                "{:?} {:?} from {} {} to {} {}, captured {:?} ",
                piece.owner,
                piece.piece_type,
                start.row(),
                start.col(),
                end.row(),
                end.col(),
                captured_piece.map(|piece| format!("{:?} {:?}", piece.owner, piece.piece_type))
            ),
            Self::CastlingLong { owner } => write!(f, "castling long {:?} ", *owner),
            Self::CastlingShort { owner } => write!(f, "castling short {:?} ", *owner),
            _ => write!(f, "not supported"),
        }
    }
}
