use std::hint::assert_unchecked;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
/// This struct will always contain a valid column.
///
/// Used for en passant columns.
pub struct Column(i8);

impl Column {
    pub const fn new(col: i8) -> Option<Self> {
        if col < 0 || col >= 8 {
            None
        } else {
            Some(Self(col))
        }
    }

    pub const fn get(&self) -> i8 {
        let col = self.0;

        // SAFETY: The constructor guarantees that the column is always valid.
        unsafe { std::hint::assert_unchecked(col >= 0) };
        unsafe { std::hint::assert_unchecked(col < 8) };

        col
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
/// This struct will always contain a valid position.
pub struct Position(i8);

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
            Some(Self(row * 8 + col))
        } else {
            None
        }
    }

    pub const fn new_assert(row: i8, col: i8) -> Self {
        assert!(0 <= row && row < 8 && 0 <= col && col < 8);

        Self(row * 8 + col)
    }

    const fn inner(self) -> i8 {
        let inner = self.0;

        // SAFETY: The constructors guarantee that the position is always valid.
        // That is, 0 <= row < 8 and 0 <= col < 8 => 0 <= inner < 64.
        unsafe {
            assert_unchecked(0 <= inner);
            assert_unchecked(inner < 64);
        }

        inner
    }

    pub const fn row(self) -> i8 {
        self.inner() / 8
    }

    pub const fn col(self) -> i8 {
        self.inner() % 8
    }

    pub const fn add(self, delta: (i8, i8)) -> Option<Self> {
        let row = self.row() + delta.0;
        let col = self.col() + delta.1;

        Self::new(row, col)
    }

    pub const fn as_index(self) -> usize {
        self.inner() as usize
    }

    pub fn from_bitboard(bitboard: u64) -> Self {
        assert!(bitboard != 0);

        Self(bitboard.trailing_zeros() as i8)
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            (self.col() as u8 + b'a') as char,
            (self.row() as u8 + b'1') as char
        )
    }
}
