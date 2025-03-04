#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Player {
    White = 0,
    Black = 1,
}

impl Player {
    pub fn as_index(&self) -> usize {
        match self {
            Self::White => 0,
            Self::Black => 1,
        }
    }

    pub fn as_score(&self) -> i16 {
        match self {
            Self::White => 1,
            Self::Black => -1,
        }
    }

    pub fn opposite(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }

    pub fn from_index(index: usize) -> Option<Self> {
        let player = match index {
            0 => Self::White,
            1 => Self::Black,
            _ => return None,
        };

        Some(player)
    }
}
