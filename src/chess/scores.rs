// Source: https://www.chessprogramming.org/Simplified_Evaluation_Function

/// 1 King + 1 Queen + 6 pawns / 3 pawns + a minor piece / 1 pawn + rook
pub const ENDGAME_THRESHOLD: u32 = 1500 + 20000;

pub const PAWN_SCORE_BASE: i16 = 100;
pub const KNIGHT_SCORE_BASE: i16 = 320;
pub const BISHOP_SCORE_BASE: i16 = 330;
pub const ROOK_SCORE_BASE: i16 = 500;
pub const QUEEN_SCORE_BASE: i16 = 900;
pub const KING_SCORE_BASE: i16 = 20000;

#[rustfmt::skip]
pub const PAWN_SCORES_DELTA: [i16; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
     5,  5, 10, 25, 25, 10,  5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5, -5,-10,  0,  0,-10, -5,  5,
     5, 10, 10,-20,-20, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0
];

#[rustfmt::skip]
pub const KNIGHT_SCORES_DELTA: [i16; 64] = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
];

#[rustfmt::skip]
pub const BISHOP_SCORES_DELTA: [i16; 64] = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
];

#[rustfmt::skip]
pub const ROOK_SCORES_DELTA: [i16; 64] = [
    0,  0,  0,  0,  0,  0,  0,  0,
    5, 10, 10, 10, 10, 10, 10,  5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
    0,  0,  0,  5,  5,  0,  0,  0
];

#[rustfmt::skip]
pub const QUEEN_SCORES_DELTA: [i16; 64] = [
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
];

#[rustfmt::skip]
pub const KING_MIDDLE_SCORES_DELTA: [i16; 64] = [
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
     20, 20,  0,  0,  0,  0, 20, 20,
     20, 30, 10,  0,  0, 10, 30, 20
];

#[rustfmt::skip]
pub const KING_END_SCORES_DELTA: [i16; 64] = [
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50
];

pub const PAWN_SCORES: [i16; 64] = generate_scores(PAWN_SCORE_BASE, PAWN_SCORES_DELTA);
pub const KNIGHT_SCORES: [i16; 64] = generate_scores(KNIGHT_SCORE_BASE, KNIGHT_SCORES_DELTA);
pub const BISHOP_SCORES: [i16; 64] = generate_scores(BISHOP_SCORE_BASE, BISHOP_SCORES_DELTA);
pub const ROOK_SCORES: [i16; 64] = generate_scores(ROOK_SCORE_BASE, ROOK_SCORES_DELTA);
pub const QUEEN_SCORES: [i16; 64] = generate_scores(QUEEN_SCORE_BASE, QUEEN_SCORES_DELTA);
pub const KING_SCORES_MIDDLE: [i16; 64] =
    generate_scores(KING_SCORE_BASE, KING_MIDDLE_SCORES_DELTA);
pub const KING_SCORES_END: [i16; 64] = generate_scores(KING_SCORE_BASE, KING_END_SCORES_DELTA);

const fn generate_scores(base: i16, mut delta: [i16; 64]) -> [i16; 64] {
    let mut i = 0;

    while i < 64 {
        delta[i] += base;

        i += 1;
    }

    delta
}
