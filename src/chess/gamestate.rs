use super::zobrist;

/// Information about the state of the game at a moment in time that can't be derived easily
/// Because of that, we hold it in a stack in the ChessGame struct
#[derive(Clone, Copy, Debug)]
pub struct GameState {
    /// First 4 bits represent en passant
    /// The last 4 bits of castling_rights indicate castling rights
    bitfield: u8,
}

impl GameState {
    #[inline]
    pub fn hash(self) -> u64 {
        zobrist::STATE[self.bitfield as usize]
    }

    #[inline]
    pub const fn en_passant(self) -> i8 {
        (self.bitfield & 0b1111) as i8
    }

    #[inline]
    pub fn set_en_passant(&mut self, value: i8) {
        self.bitfield = (self.bitfield & 0b11110000) + (value as u8);
    }

    #[inline]
    pub const fn white_king_castling(self) -> bool {
        (self.bitfield & (1 << 4)) != 0
    }

    #[inline]
    pub fn set_white_king_castling_false(&mut self) {
        self.bitfield &= !(1 << 4);
    }

    #[inline]
    pub fn set_white_king_castling_true(&mut self) {
        self.bitfield |= 1 << 4;
    }

    #[inline]
    pub const fn white_queen_castling(self) -> bool {
        (self.bitfield & (1 << 5)) != 0
    }

    #[inline]
    pub fn set_white_queen_castling_false(&mut self) {
        self.bitfield &= !(1 << 5);
    }

    #[inline]
    pub fn set_white_queen_castling_true(&mut self) {
        self.bitfield |= 1 << 5;
    }

    #[inline]
    pub const fn black_king_castling(self) -> bool {
        (self.bitfield & (1 << 6)) != 0
    }

    #[inline]
    pub fn set_black_king_castling_false(&mut self) {
        self.bitfield &= !(1 << 6);
    }

    #[inline]
    pub fn set_black_king_castling_true(&mut self) {
        self.bitfield |= 1 << 6;
    }

    #[inline]
    pub const fn black_queen_castling(self) -> bool {
        (self.bitfield & (1 << 7)) != 0
    }

    #[inline]
    pub fn set_black_queen_castling_false(&mut self) {
        self.bitfield &= !(1 << 7);
    }

    #[inline]
    pub fn set_black_queen_castling_true(&mut self) {
        self.bitfield |= 1 << 7;
    }
}

impl Default for GameState {
    /// Default state is no en passant square, and no castling rights
    fn default() -> Self {
        Self {
            // 8 Represents no en passant square
            bitfield: 8,
        }
    }
}
