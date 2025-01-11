use super::{move_struct::Move, piece::Piece, position::Position, Game, Player};
use crate::chess::{deltas::*, piece::PieceType};
use std::cell::OnceCell;

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

    // The double-advanced position exists if pawn is on first row
    let double_advanced_pos = pos.add(first_row_delta);

    // First moves for pawns always exist if available
    if pos.row() == first_row
        && game.get_position(advanced_pos).is_none()
        && game.get_position(double_advanced_pos.unwrap()).is_none()
    {
        push(Move::Quiet {
            piece,
            start: pos,
            end: double_advanced_pos.unwrap(),
        });
    }

    let side_deltas = match piece.owner {
        Player::White => [(1, 1), (1, -1)],
        Player::Black => [(-1, 1), (-1, -1)],
    };

    if game.get_position(advanced_pos).is_none() {
        if last_row == advanced_pos.row() {
            for new_piece in [
                PieceType::Queen,
                PieceType::Rook,
                PieceType::Bishop,
                PieceType::Knight,
            ] {
                let _move = Move::Promotion {
                    owner: game.current_player,
                    start: pos,
                    end: advanced_pos,
                    captured_piece: None,
                    new_piece,
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

    for delta in side_deltas {
        if let Some(new_pos) = pos.add(delta) {
            let place = game.get_position(new_pos);
            if place.is_some_and(|place_piece| place_piece.owner != piece.owner) {
                if last_row == new_pos.row() {
                    for new_piece in [
                        PieceType::Queen,
                        PieceType::Rook,
                        PieceType::Bishop,
                        PieceType::Knight,
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
                    let _move = if let Some(capture) = place {
                        Move::Capture {
                            piece,
                            start: pos,
                            end: new_pos,
                            capture,
                        }
                    } else {
                        Move::Quiet {
                            piece,
                            start: pos,
                            end: new_pos,
                        }
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

fn get_king_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    let other_king_pos = game.get_king_position(game.current_player.the_other());
    for delta in DELTA_KING {
        if let Some(new_pos) = pos.add(delta) {
            let place = game.get_position(new_pos);
            if !place.is_some_and(|place_piece| place_piece.owner == game.current_player) {
                // Kings can't move into each other
                if i8::abs(new_pos.row() - other_king_pos.row()) <= 1
                    && i8::abs(new_pos.col() - other_king_pos.col()) <= 1
                {
                    continue;
                }

                let _move = if let Some(capture) = place {
                    Move::Capture {
                        piece,
                        start: pos,
                        end: new_pos,
                        capture,
                    }
                } else {
                    Move::Quiet {
                        piece,
                        start: pos,
                        end: new_pos,
                    }
                };

                push(_move);
            }
        }
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

fn get_knight_moves(mut push: impl FnMut(Move), game: &Game, pos: Position, piece: Piece) {
    for delta in DELTA_KNIGHT {
        if let Some(new_pos) = pos.add(delta) {
            let place = game.get_position(new_pos);
            if !place.is_some_and(|place_piece| place_piece.owner == game.current_player) {
                let _move = if let Some(capture) = place {
                    Move::Capture {
                        piece,
                        start: pos,
                        end: new_pos,
                        capture,
                    }
                } else {
                    Move::Quiet {
                        piece,
                        start: pos,
                        end: new_pos,
                    }
                };

                push(_move);
            }
        }
    }
}
