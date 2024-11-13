#[derive(PartialEq, Eq, Clone, Copy, Debug)]
/// This struct should always contain a valid position.
/// That is, values for row and col are always in 0..8
pub struct Position(i8, i8);

impl Position {
    pub const WHITE_QUEEN_ROOK: Self = Self(0, 0);
    pub const WHITE_KING_ROOK: Self = Self(0, 7);
    pub const BLACK_QUEEN_ROOK: Self = Self(7, 0);
    pub const BLACK_KING_ROOK: Self = Self(7, 7);

    #[inline]
    pub fn new(row: i8, col: i8) -> Option<Self> {
        if (0..8).contains(&row) && (0..8).contains(&col) {
            Some(Self(row, col))
        } else {
            None
        }
    }

    #[inline]
    pub fn new_assert(row: i8, col: i8) -> Self {
        assert!((0..8).contains(&row) && (0..8).contains(&col));
        Self(row, col)
    }

    /// # Safety
    /// Same as self.new, but unchecked
    ///
    /// Caller must guarantee that row and col are valid
    #[inline]
    pub unsafe fn new_unsafe(row: i8, col: i8) -> Self {
        debug_assert!((0..8).contains(&row) && (0..8).contains(&col));
        Self(row, col)
    }

    #[inline]
    pub fn row(self) -> i8 {
        self.0
    }

    #[inline]
    pub fn col(self) -> i8 {
        self.1
    }

    #[inline]
    pub fn add(self, delta: (i8, i8)) -> Option<Self> {
        let row = self.0 + delta.0;
        let col = self.1 + delta.1;
        if (0..8).contains(&row) && (0..8).contains(&col) {
            Some(Self(row, col))
        } else {
            None
        }
    }

    /// # Safety
    /// Same as self.add, but unchecked
    ///
    /// Caller must guarantee that return value is valid
    #[inline]
    pub unsafe fn add_unsafe(self, delta: (i8, i8)) -> Self {
        let row = self.0 + delta.0;
        let col = self.1 + delta.1;
        debug_assert!((0..8).contains(&row) && (0..8).contains(&col));
        Self(row, col)
    }

    /// Returns the index this position would take in a linear board array
    /// i.e. it always lies in 0..64
    #[inline]
    pub fn as_usize(self) -> usize {
        (self.0 * 8 + self.1) as usize
    }
}
