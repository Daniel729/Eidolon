use super::{position::Column, zobrist};

/// Information about the state of the game at a moment in time that can't be derived easily
/// Because of that, we hold it in a stack to be able to undo moves
#[derive(Clone, Copy, Debug)]
pub struct GameState {
    /// The first 4 bits indicate castling rights
    /// and the last 4 bits represent en passant, with 8 representing no en passant rights
    bitfield: u8,
}

impl GameState {
    pub fn hash(self) -> u64 {
        zobrist::STATE[self.bitfield as usize]
    }

    pub const fn en_passant(self) -> i8 {
        ((self.bitfield & 0xF0) >> 4) as i8
    }

    pub fn set_en_passant(&mut self, value: Option<Column>) {
        self.bitfield = (self.bitfield & 0xF) + (value.map_or(8, |v| v.get()) << 4) as u8;
    }

    pub const fn white_king_castling(self) -> bool {
        (self.bitfield & (1 << 0)) != 0
    }

    pub fn set_white_king_castling_false(&mut self) {
        self.bitfield &= !(1 << 0);
    }

    pub fn set_white_king_castling_true(&mut self) {
        self.bitfield |= 1 << 0;
    }

    pub const fn white_queen_castling(self) -> bool {
        (self.bitfield & (1 << 1)) != 0
    }

    pub fn set_white_queen_castling_false(&mut self) {
        self.bitfield &= !(1 << 1);
    }

    pub fn set_white_queen_castling_true(&mut self) {
        self.bitfield |= 1 << 1;
    }

    pub const fn black_king_castling(self) -> bool {
        (self.bitfield & (1 << 2)) != 0
    }

    pub fn set_black_king_castling_false(&mut self) {
        self.bitfield &= !(1 << 2);
    }

    pub fn set_black_king_castling_true(&mut self) {
        self.bitfield |= 1 << 2;
    }

    pub const fn black_queen_castling(self) -> bool {
        (self.bitfield & (1 << 3)) != 0
    }

    pub fn set_black_queen_castling_false(&mut self) {
        self.bitfield &= !(1 << 3);
    }

    pub fn set_black_queen_castling_true(&mut self) {
        self.bitfield |= 1 << 3;
    }
}

impl Default for GameState {
    /// Default state is no en passant square, and no castling rights
    fn default() -> Self {
        Self {
            // No en passant square, no castling rights
            bitfield: 0b1000_0000,
        }
    }
}
