use crate::chess::{
    piece::{Piece, PieceType},
    player::Player,
    position::{Column, Position},
};

use super::Move;

#[derive(Debug, Clone, Copy)]
pub struct CompactMove(CompactMoveInner);

impl From<Move> for CompactMove {
    fn from(_move: Move) -> Self {
        CompactMove(_move.into())
    }
}

impl From<CompactMove> for Move {
    fn from(compact_move: CompactMove) -> Self {
        compact_move.0.into()
    }
}

#[derive(Debug, Clone, Copy)]
struct CompactPiece(u8);

impl From<Piece> for CompactPiece {
    fn from(piece: Piece) -> Self {
        CompactPiece(((piece.piece_type.as_index() as u8) << 1) | (piece.owner.as_index() as u8))
    }
}

impl From<CompactPiece> for Piece {
    fn from(piece: CompactPiece) -> Self {
        Piece::new(
            PieceType::from_index((piece.0 >> 1) as usize).unwrap(),
            Player::from_index((piece.0 & 1) as usize).unwrap(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct CompactTwoPieces(u8);

impl From<(Piece, Piece)> for CompactTwoPieces {
    fn from((piece1, piece2): (Piece, Piece)) -> Self {
        let compact_1: CompactPiece = piece1.into();
        let compact_2: CompactPiece = piece2.into();

        CompactTwoPieces((compact_1.0) << 4 | compact_2.0)
    }
}

impl From<CompactTwoPieces> for (Piece, Piece) {
    fn from(pieces: CompactTwoPieces) -> Self {
        (
            CompactPiece(pieces.0 >> 4).into(),
            CompactPiece(pieces.0 & 0xF).into(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum CompactMoveInner {
    Quiet {
        compact_piece: CompactPiece,
        start: Position,
        end: Position,
    },
    Capture {
        compact_pieces: CompactTwoPieces,
        start: Position,
        end: Position,
    },
    PromotionQuiet {
        compact_piece: CompactPiece,
        start: Position,
        end: Position,
    },
    PromotionCapture {
        compact_pieces: CompactTwoPieces,
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

impl From<Move> for CompactMoveInner {
    fn from(_move: Move) -> Self {
        match _move {
            Move::Quiet { piece, start, end } => CompactMoveInner::Quiet {
                compact_piece: piece.into(),
                start,
                end,
            },
            Move::Capture {
                piece,
                capture,
                start,
                end,
            } => CompactMoveInner::Capture {
                compact_pieces: (piece, capture).into(),
                start,
                end,
            },
            Move::PromotionQuiet {
                new_piece,
                start,
                end,
            } => CompactMoveInner::PromotionQuiet {
                compact_piece: new_piece.into(),
                start,
                end,
            },
            Move::PromotionCapture {
                new_piece,
                capture,
                start,
                end,
            } => CompactMoveInner::PromotionCapture {
                compact_pieces: (new_piece, capture).into(),
                start,
                end,
            },
            Move::CastlingShort { owner } => CompactMoveInner::CastlingShort { owner },
            Move::CastlingLong { owner } => CompactMoveInner::CastlingLong { owner },
            Move::EnPassant {
                start_col,
                end_col,
                owner,
            } => CompactMoveInner::EnPassant {
                start_col,
                end_col,
                owner,
            },
        }
    }
}

impl From<CompactMoveInner> for Move {
    fn from(compact_move: CompactMoveInner) -> Self {
        match compact_move {
            CompactMoveInner::Quiet {
                compact_piece,
                start,
                end,
            } => Move::Quiet {
                piece: compact_piece.into(),
                start,
                end,
            },
            CompactMoveInner::Capture {
                compact_pieces,
                start,
                end,
            } => {
                let (piece, capture) = compact_pieces.into();
                Move::Capture {
                    piece,
                    capture,
                    start,
                    end,
                }
            }
            CompactMoveInner::PromotionQuiet {
                compact_piece,
                start,
                end,
            } => Move::PromotionQuiet {
                new_piece: compact_piece.into(),
                start,
                end,
            },
            CompactMoveInner::PromotionCapture {
                compact_pieces,
                start,
                end,
            } => {
                let (new_piece, capture) = compact_pieces.into();

                Move::PromotionCapture {
                    new_piece,
                    capture,
                    start,
                    end,
                }
            }
            CompactMoveInner::CastlingShort { owner } => Move::CastlingShort { owner },
            CompactMoveInner::CastlingLong { owner } => Move::CastlingLong { owner },
            CompactMoveInner::EnPassant {
                start_col,
                end_col,
                owner,
            } => Move::EnPassant {
                owner,
                start_col,
                end_col,
            },
        }
    }
}
