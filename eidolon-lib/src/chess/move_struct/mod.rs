pub mod compact_move;

use super::piece::{Piece, PieceType};
use super::position::{Column, Position};
use super::{Game, Player};
use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Copy)]
// aligning here leads to performance gains
#[repr(align(8))]
pub enum Move {
    Quiet {
        piece: Piece,
        start: Position,
        end: Position,
    },
    Capture {
        piece: Piece,
        capture: Piece,
        start: Position,
        end: Position,
    },
    PromotionQuiet {
        new_piece: Piece,
        start: Position,
        end: Position,
    },
    PromotionCapture {
        new_piece: Piece,
        capture: Piece,
        start: Position,
        end: Position,
    },
    CastlingShort {
        owner: Player,
    },
    CastlingLong {
        owner: Player,
    },
    EnPassant {
        owner: Player,
        start_col: Column,
        end_col: Column,
    },
}

impl Move {
    pub fn is_tactical_move(&self) -> bool {
        matches!(
            self,
            Self::Capture { .. }
                | Self::PromotionQuiet { .. }
                | Self::PromotionCapture { .. }
                | Self::EnPassant { .. }
        )
    }

    // Return the moves index inside [piece][tp] tables
    pub fn index_history(self) -> usize {
        let (piece, end) = match self {
            Move::Quiet { piece, end, .. } => (piece, end),
            Move::Capture { piece, end, .. } => (piece, end),
            Move::CastlingShort { owner } => {
                let piece = Piece::new(PieceType::King, owner);

                let end = match owner {
                    Player::White => Position::new(0, 6).unwrap(),
                    Player::Black => Position::new(7, 6).unwrap(),
                };

                (piece, end)
            }
            Move::CastlingLong { owner } => {
                let piece = Piece::new(PieceType::King, owner);

                let end = match owner {
                    Player::White => Position::new(0, 2).unwrap(),
                    Player::Black => Position::new(7, 2).unwrap(),
                };

                (piece, end)
            }
            Move::EnPassant { owner, end_col, .. } => {
                let piece = Piece::new(PieceType::Pawn, owner);

                let end = match owner {
                    Player::White => Position::new(5, end_col.get()).unwrap(),
                    Player::Black => Position::new(2, end_col.get()).unwrap(),
                };

                (piece, end)
            }
            Move::PromotionQuiet { new_piece, end, .. } => (new_piece, end),

            Move::PromotionCapture { new_piece, end, .. } => (new_piece, end),
        };

        piece.as_index() * 64 + end.as_index()
    }

    pub fn uci_notation(&self) -> String {
        let mut s = String::new();
        match self {
            Self::Quiet { start, end, .. } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
            }
            Self::Capture { start, end, .. } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
            }
            Self::PromotionQuiet {
                start,
                end,
                new_piece,
                ..
            } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
                s.push(match new_piece.piece_type {
                    PieceType::Queen => 'q',
                    PieceType::Rook => 'r',
                    PieceType::Bishop => 'b',
                    PieceType::Knight => 'n',
                    _ => unreachable!(),
                });
            }
            Self::PromotionCapture {
                start,
                end,
                new_piece,
                ..
            } => {
                s.push((start.col() as u8 + b'a') as char);
                s.push((start.row() as u8 + b'1') as char);
                s.push((end.col() as u8 + b'a') as char);
                s.push((end.row() as u8 + b'1') as char);
                s.push(match new_piece.piece_type {
                    PieceType::Queen => 'q',
                    PieceType::Rook => 'r',
                    PieceType::Bishop => 'b',
                    PieceType::Knight => 'n',
                    _ => unreachable!(),
                });
            }
            Self::CastlingShort { owner } => {
                let row = match owner {
                    Player::White => '1',
                    Player::Black => '8',
                };
                s.push('e');
                s.push(row);
                s.push('g');
                s.push(row);
            }
            Self::CastlingLong { owner } => {
                let row = match owner {
                    Player::White => '1',
                    Player::Black => '8',
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
                    Player::White => ('5', '6'),
                    Player::Black => ('4', '3'),
                };
                s.push((start_col.get() as u8 + b'a') as char);
                s.push(start_row);
                s.push((end_col.get() as u8 + b'a') as char);
                s.push(end_row);
            }
        }
        s
    }

    pub fn pgn_notation(&self) -> String {
        match self {
            Self::Quiet { piece, start, end } => {
                let mut s = String::new();
                s.push_str(piece.as_str_pgn());
                s.push(((start.col()) as u8 + b'a') as char);
                s.push(((end.col()) as u8 + b'a') as char);
                s.push_str((end.row() + 1).to_string().as_str());
                s
            }
            Self::Capture {
                piece, start, end, ..
            } => {
                let mut s = String::new();
                s.push_str(piece.as_str_pgn());
                s.push(((start.col()) as u8 + b'a') as char);
                s.push('x');
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
                s.push((start_col.get() as u8 + b'a') as char);
                s.push('x');
                s.push((end_col.get() as u8 + b'a') as char);
                match owner {
                    Player::White => s.push('6'),
                    Player::Black => s.push('3'),
                };
                s
            }
            Self::PromotionQuiet { new_piece, end, .. } => {
                let mut s = String::new();

                s.push(((end.col()) as u8 + b'a') as char);
                s.push_str((end.row() + 1).to_string().as_str());
                s.push('=');
                s.push(match new_piece.piece_type {
                    PieceType::Queen => 'Q',
                    PieceType::Rook => 'R',
                    PieceType::Bishop => 'K',
                    PieceType::Knight => 'B',
                    _ => unreachable!(),
                });
                s
            }
            Self::PromotionCapture { end, new_piece, .. } => {
                let mut s = String::new();
                s.push('x');
                s.push(((end.col()) as u8 + b'a') as char);
                s.push_str((end.row() + 1).to_string().as_str());
                s.push('=');
                s.push(match new_piece.piece_type {
                    PieceType::Queen => 'Q',
                    PieceType::Rook => 'R',
                    PieceType::Bishop => 'K',
                    PieceType::Knight => 'B',
                    _ => unreachable!(),
                });
                s
            }
        }
    }

    pub fn from_uci_notation(s: &str, game: &Game) -> Option<Self> {
        if s == "e1g1" && game.get_king_position(Player::White) == Position::new_assert(0, 4) {
            Some(Self::CastlingShort {
                owner: Player::White,
            })
        } else if s == "e8g8" && game.get_king_position(Player::Black) == Position::new_assert(7, 4)
        {
            Some(Self::CastlingShort {
                owner: Player::Black,
            })
        } else if s == "e1c1" && game.get_king_position(Player::White) == Position::new_assert(0, 4)
        {
            Some(Self::CastlingLong {
                owner: Player::White,
            })
        } else if s == "e8c8" && game.get_king_position(Player::Black) == Position::new_assert(7, 4)
        {
            Some(Self::CastlingLong {
                owner: Player::Black,
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
                    'q' | 'Q' => PieceType::Queen,
                    'r' | 'R' => PieceType::Rook,
                    'n' | 'N' => PieceType::Knight,
                    'b' | 'B' => PieceType::Bishop,
                    _ => return None,
                };

                if let Some(capture) = game.get_position(end) {
                    return Some(Self::PromotionCapture {
                        new_piece: Piece::new(new_piece, game.current_player),
                        capture,
                        start,
                        end,
                    });
                } else {
                    return Some(Self::PromotionQuiet {
                        new_piece: Piece::new(new_piece, game.current_player),
                        start,
                        end,
                    });
                }
            }

            if let Some(piece) = game.get_position(start) {
                // This move is either en passant or normal
                return if piece.piece_type == PieceType::Pawn
                    && game.get_position(end).is_none()
                    && i8::abs(start.col() - end.col()) == 1
                {
                    Some(Self::EnPassant {
                        owner: game.current_player,
                        start_col: Column::new(start.col()).unwrap(),
                        end_col: Column::new(end.col()).unwrap(),
                    })
                } else if let Some(capture) = game.get_position(end) {
                    Some(Self::Capture {
                        piece,
                        capture,
                        start,
                        end,
                    })
                } else {
                    Some(Self::Quiet { piece, start, end })
                };
            }

            None
        }
    }
}

impl std::fmt::Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Quiet { piece, start, end } => write!(
                f,
                "{:?} {:?} from {} to {}",
                piece.owner, piece.piece_type, start, end,
            ),
            Self::Capture {
                piece,
                start,
                end,
                capture,
            } => write!(
                f,
                "{:?} {:?} from {} to {} captured {:?} ",
                piece.owner,
                piece.piece_type,
                start,
                end,
                format!("{:?} {:?}", capture.owner, capture.piece_type)
            ),
            Self::CastlingLong { owner } => write!(f, "castling long {:?} ", *owner),
            Self::CastlingShort { owner } => write!(f, "castling short {:?} ", *owner),
            Self::EnPassant {
                owner,
                start_col,
                end_col,
            } => write!(
                f,
                "en passant {:?} from {:?} to {:?} ",
                *owner, start_col, end_col
            ),
            Self::PromotionQuiet {
                new_piece,
                start,
                end,
            } => write!(
                f,
                "promotion {:?} {:?} from {} to {}",
                new_piece.owner, new_piece.piece_type, start, end,
            ),
            Self::PromotionCapture {
                new_piece,
                capture,
                start,
                end,
            } => write!(
                f,
                "promotion {:?} {:?} from {} to {}, captured {:?} {:?} ",
                new_piece.owner,
                new_piece.piece_type,
                start,
                end,
                capture.owner,
                capture.piece_type,
            ),
        }
    }
}
