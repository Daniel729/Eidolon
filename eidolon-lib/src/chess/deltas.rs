#[rustfmt::skip]
pub const DELTA_KING: [(i8, i8); 8] =
    [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, 1), (1, -1), (-1, -1)];

#[rustfmt::skip]
pub const DELTA_KNIGHT: [(i8, i8); 8] = 
    [(1, 2), (2, 1), (-1, -2), (-2, -1), (1, -2), (-2, 1), (-1, 2), (2, -1)];

#[rustfmt::skip]
pub const DELTA_ROOK_1: [(i8, i8); 7] =
    [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)];

#[rustfmt::skip]
pub const DELTA_ROOK_2: [(i8, i8); 7] =
    [(0, -1), (0, -2), (0, -3), (0, -4), (0, -5), (0, -6), (0, -7)];

#[rustfmt::skip]
pub const DELTA_ROOK_3: [(i8, i8); 7] =
    [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)];

#[rustfmt::skip]
pub const DELTA_ROOK_4: [(i8, i8); 7] =
    [(-1, 0), (-2, 0), (-3, 0), (-4, 0), (-5, 0), (-6, 0), (-7, 0)];

#[rustfmt::skip]
pub const DELTA_BISHOP_1: [(i8, i8); 7] =
    [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7)];

#[rustfmt::skip]
pub const DELTA_BISHOP_2: [(i8, i8); 7] =
    [(-1, -1), (-2, -2), (-3, -3), (-4, -4), (-5, -5), (-6, -6), (-7, -7)];

#[rustfmt::skip]
pub const DELTA_BISHOP_3: [(i8, i8); 7] =
    [(1, -1), (2, -2), (3, -3), (4, -4), (5, -5), (6, -6), (7, -7)];

#[rustfmt::skip]
pub const DELTA_BISHOP_4: [(i8, i8); 7] =
    [(-1, 1), (-2, 2), (-3, 3), (-4, 4), (-5, 5), (-6, 6), (-7, 7)];
