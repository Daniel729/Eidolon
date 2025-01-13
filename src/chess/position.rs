use std::hint::assert_unchecked;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
/// This struct will always contain a valid position.
/// That is, values for row and col are always in 0..8
pub struct Position(i8, i8);

impl Position {
    pub const WHITE_QUEEN_ROOK: Self = Self::new_assert(0, 0);
    pub const WHITE_KING_ROOK: Self = Self::new_assert(0, 7);
    pub const BLACK_QUEEN_ROOK: Self = Self::new_assert(7, 0);
    pub const BLACK_KING_ROOK: Self = Self::new_assert(7, 7);

    pub const CASTLING_SHORT_OLD_KING: [Self; 2] = [Self::new_assert(0, 4), Self::new_assert(7, 4)];
    pub const CASTLING_SHORT_NEW_KING: [Self; 2] = [Self::new_assert(0, 6), Self::new_assert(7, 6)];
    pub const CASTLING_SHORT_OLD_ROOK: [Self; 2] = [Self::new_assert(0, 7), Self::new_assert(7, 7)];
    pub const CASTLING_SHORT_NEW_ROOK: [Self; 2] = [Self::new_assert(0, 5), Self::new_assert(7, 5)];

    pub const CASTLING_LONG_OLD_KING: [Self; 2] = [Self::new_assert(0, 4), Self::new_assert(7, 4)];
    pub const CASTLING_LONG_NEW_KING: [Self; 2] = [Self::new_assert(0, 2), Self::new_assert(7, 2)];
    pub const CASTLING_LONG_OLD_ROOK: [Self; 2] = [Self::new_assert(0, 0), Self::new_assert(7, 0)];
    pub const CASTLING_LONG_NEW_ROOK: [Self; 2] = [Self::new_assert(0, 3), Self::new_assert(7, 3)];

    pub const fn new(row: i8, col: i8) -> Option<Self> {
        if 0 <= row && row < 8 && 0 <= col && col < 8 {
            Some(Self(row, col))
        } else {
            None
        }
    }

    pub const fn new_assert(row: i8, col: i8) -> Self {
        assert!(0 <= row && row < 8 && 0 <= col && col < 8);

        Self(row, col)
    }

    pub const fn row(self) -> i8 {
        let row = self.0;

        // SAFETY: This is the invariant of this struct
        unsafe {
            assert_unchecked(row >= 0);
            assert_unchecked(row < 8);
        }

        row
    }

    pub const fn col(self) -> i8 {
        let col = self.1;

        // SAFETY: This is the invariant of this struct
        unsafe {
            assert_unchecked(col >= 0);
            assert_unchecked(col < 8);
        }

        col
    }

    pub const fn add(self, delta: (i8, i8)) -> Option<Self> {
        let row = self.row() + delta.0;
        let col = self.col() + delta.1;

        if 0 <= row && row < 8 && 0 <= col && col < 8 {
            Some(Self(row, col))
        } else {
            None
        }
    }

    /// Returns the index this position would take in a linear board array
    /// i.e. it always lies in 0..64
    pub const fn as_index(self) -> usize {
        (self.row() * 8 + self.col()) as usize
    }

    pub const fn from_bitboard(bitboard: u64) -> Self {
        let index = bitboard.trailing_zeros() as i8;

        Self(index as i8 / 8, index as i8 % 8)
    }
}
