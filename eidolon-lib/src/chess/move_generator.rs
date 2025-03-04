use super::{
    Game, Player, bitboard,
    move_struct::Move,
    piece::Piece,
    position::{Column, Position},
};
use crate::chess::{deltas::*, piece::PieceType};
use std::cell::LazyCell;

#[inline(always)]
pub fn get_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    macro_rules! search_deltas {
        ( $( $deltas:expr ),* ) => { $ (
            for delta in $deltas {
                if let Some(new_pos) = pos.add(delta) {
                    let place = game.get_position(new_pos);

                    if let Some(capture) = place  {
                        if capture.owner != game.current_player {
                            let _move = Move::Capture {
                                piece,
                                start: pos,
                                end: new_pos,
                                capture,
                            };

                            push(_move);
                        }
                        break;
                    }

                    let _move = Move::Quiet {
                        piece,
                        start: pos,
                        end: new_pos,
                    };

                    push(_move);
                } else {
                    break;
                }
            }
        )* };
    }

    match piece.piece_type {
        PieceType::Pawn => get_pawn_moves(push, game, pos, piece),
        PieceType::King => get_king_moves(push, game, pos, piece),
        PieceType::Knight => get_knight_moves(push, game, pos, piece),
        PieceType::Rook => {
            search_deltas![DELTA_ROOK_1, DELTA_ROOK_2, DELTA_ROOK_3, DELTA_ROOK_4];
        }
        PieceType::Bishop => {
            search_deltas![
                DELTA_BISHOP_1,
                DELTA_BISHOP_2,
                DELTA_BISHOP_3,
                DELTA_BISHOP_4
            ];
        }

        PieceType::Queen => {
            search_deltas![
                DELTA_ROOK_1,
                DELTA_ROOK_2,
                DELTA_ROOK_3,
                DELTA_ROOK_4,
                DELTA_BISHOP_1,
                DELTA_BISHOP_2,
                DELTA_BISHOP_3,
                DELTA_BISHOP_4
            ];
        }
    }
}

#[inline(always)]
fn get_pawn_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    let first_row = match piece.owner {
        Player::White => 1,
        Player::Black => 6,
    };

    let last_row = match piece.owner {
        Player::White => 7,
        Player::Black => 0,
    };

    let en_passant_row = match piece.owner {
        Player::White => 4,
        Player::Black => 3,
    };

    let normal_delta = match piece.owner {
        Player::White => (1, 0),
        Player::Black => (-1, 0),
    };

    let first_row_delta = match piece.owner {
        Player::White => (2, 0),
        Player::Black => (-2, 0),
    };

    // The advanced position always exists
    let advanced_pos = pos.add(normal_delta).unwrap();
    let advanced_bitboard = 1 << advanced_pos.as_index();

    if pos.row() == first_row && game.bitboard_all() & advanced_bitboard == 0 {
        let double_advanced_pos = pos.add(first_row_delta).unwrap();
        let double_advanced_bitboard = 1 << double_advanced_pos.as_index();

        if game.bitboard_all() & double_advanced_bitboard == 0 {
            push(Move::Quiet {
                piece,
                start: pos,
                end: double_advanced_pos,
            });
        }
    }

    if game.bitboard_all() & advanced_bitboard == 0 {
        if last_row == advanced_pos.row() {
            for new_piece in [
                PieceType::Queen,
                PieceType::Rook,
                PieceType::Bishop,
                PieceType::Knight,
            ] {
                let _move = Move::PromotionQuiet {
                    new_piece: Piece::new(new_piece, piece.owner),
                    start: pos,
                    end: advanced_pos,
                };
                push(_move);
            }
        } else {
            let _move = Move::Quiet {
                piece,
                start: pos,
                end: advanced_pos,
            };
            push(_move);
        };
    }

    let attack = match piece.owner {
        Player::White => bitboard::PAWN_ATTACK_WHITE[pos.as_index()],
        Player::Black => bitboard::PAWN_ATTACK_BLACK[pos.as_index()],
    };

    let opp_pieces = game.bitboard_opp();

    let mut attacked = opp_pieces & attack;

    while attacked != 0 {
        let new_pos = Position::from_bitboard(attacked);
        let capture = game.get_position(new_pos).unwrap();

        if last_row == new_pos.row() {
            for new_piece in [
                PieceType::Queen,
                PieceType::Rook,
                PieceType::Bishop,
                PieceType::Knight,
            ] {
                let _move = Move::PromotionCapture {
                    new_piece: Piece::new(new_piece, piece.owner),
                    start: pos,
                    end: new_pos,
                    capture,
                };

                push(_move);
            }
        } else {
            let _move = Move::Capture {
                piece,
                start: pos,
                end: new_pos,
                capture,
            };

            push(_move);
        };

        attacked &= attacked - 1;
    }

    let valid_en_passant = game.state().en_passant();
    if pos.row() == en_passant_row
        && valid_en_passant < 8
        && i8::abs(valid_en_passant - pos.col()) == 1
    {
        let _move = Move::EnPassant {
            owner: game.current_player,
            start_col: Column::new(pos.col()).unwrap(),
            end_col: Column::new(valid_en_passant).unwrap(),
        };
        push(_move);
    }
}

#[inline(always)]
fn get_king_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    let other_king_pos = game.get_king_position(game.current_player.opposite());

    let all_pieces = game.bitboard_all();
    let opp_pieces = game.bitboard_opp();

    let attack = bitboard::KING_ATTACK[pos.as_index()];
    let other_king_attack = bitboard::KING_ATTACK[other_king_pos.as_index()];

    let mut attacked = (!all_pieces & attack) & !other_king_attack;

    while attacked != 0 {
        let new_pos = Position::from_bitboard(attacked);

        let _move = Move::Quiet {
            piece,
            start: pos,
            end: new_pos,
        };

        push(_move);

        attacked &= attacked - 1;
    }

    let mut attacked = (opp_pieces & attack) & !other_king_attack;

    while attacked != 0 {
        let new_pos = Position::from_bitboard(attacked);

        let _move = Move::Capture {
            piece,
            start: pos,
            end: new_pos,
            capture: game.get_position(new_pos).unwrap(),
        };

        push(_move);

        attacked &= attacked - 1;
    }

    let state = game.state();

    let (king_side_castling, queen_side_castling) = match game.current_player {
        Player::White => (state.white_king_castling(), state.white_queen_castling()),
        Player::Black => (state.black_king_castling(), state.black_queen_castling()),
    };

    let row = match game.current_player {
        Player::White => 0,
        Player::Black => 7,
    };

    let king = Position::new_assert(row, 4);

    // We may need this value 0, 1, or 2 times so we lazy-initialize it.
    let is_king_targeted = LazyCell::new(|| game.is_targeted(king, game.current_player));

    if king_side_castling {
        let (pos1, pos2) = (Position::new_assert(row, 5), Position::new_assert(row, 6));
        if game.is_empty(pos1)
            && game.is_empty(pos2)
            && !*is_king_targeted
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

        if game.is_empty(pos1)
            && game.is_empty(pos2)
            && game.is_empty(pos3)
            && !*is_king_targeted
            && !game.is_targeted(pos2, game.current_player)
            && !game.is_targeted(pos3, game.current_player)
        {
            push(Move::CastlingLong {
                owner: game.current_player,
            });
        }
    }
}

#[inline(always)]
fn get_knight_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    let all_pieces = game.bitboard_all();
    let opp_pieces = game.bitboard_opp();

    let attack = bitboard::KNIGHT_ATTACK[pos.as_index()];

    let mut attacked = !all_pieces & attack;

    while attacked != 0 {
        let new_pos = Position::from_bitboard(attacked);

        let _move = Move::Quiet {
            piece,
            start: pos,
            end: new_pos,
        };

        push(_move);

        attacked &= attacked - 1;
    }

    let mut attacked = opp_pieces & attack;

    while attacked != 0 {
        let new_pos = Position::from_bitboard(attacked);

        let _move = Move::Capture {
            piece,
            start: pos,
            end: new_pos,
            capture: game.get_position(new_pos).unwrap(),
        };

        push(_move);

        attacked &= attacked - 1;
    }
}
