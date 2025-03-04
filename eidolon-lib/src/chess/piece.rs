use super::Player;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PieceType {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub owner: Player,
}

impl Piece {
    pub fn new(piece_type: PieceType, owner: Player) -> Self {
        Self { piece_type, owner }
    }

    pub fn opposite(self) -> Self {
        Self {
            piece_type: self.piece_type,
            owner: self.owner.opposite(),
        }
    }
}

impl PieceType {
    pub fn value(self) -> u8 {
        match self {
            Self::Pawn => 0,
            Self::Knight => 1,
            Self::Bishop => 1,
            Self::Rook => 2,
            Self::Queen => 3,
            Self::King => 4,
        }
    }

    pub const fn as_index(self) -> usize {
        self as usize
    }

    pub const fn from_index(index: usize) -> Option<Self> {
        let piece_type = match index {
            0 => Self::Pawn,
            1 => Self::Knight,
            2 => Self::Bishop,
            3 => Self::Rook,
            4 => Self::Queen,
            5 => Self::King,
            _ => return None,
        };

        Some(piece_type)
    }

    pub const ALL: [Self; 6] = [
        Self::Pawn,
        Self::Knight,
        Self::Bishop,
        Self::Rook,
        Self::Queen,
        Self::King,
    ];
}

impl Piece {
    pub fn value(self) -> u8 {
        self.piece_type.value()
    }

    /// This returns a value in the range 0..12
    pub fn as_index(self) -> usize {
        let mut index = self.piece_type as usize;

        if self.owner == Player::Black {
            index += 6;
        }

        index
    }

    pub fn as_str_pgn(self) -> &'static str {
        match self.piece_type {
            PieceType::King => "K",
            PieceType::Queen => "Q",
            PieceType::Rook => "R",
            PieceType::Bishop => "B",
            PieceType::Knight => "N",
            PieceType::Pawn => "",
        }
    }

    pub fn as_char(self) -> char {
        let piece = match self.piece_type {
            PieceType::King => 'K',
            PieceType::Queen => 'Q',
            PieceType::Rook => 'R',
            PieceType::Bishop => 'B',
            PieceType::Knight => 'N',
            PieceType::Pawn => 'P',
        };

        match self.owner {
            Player::White => piece,
            Player::Black => piece.to_ascii_lowercase(),
        }
    }

    pub fn from_char_ascii(piece: char) -> Option<Self> {
        let piece = match piece {
            'K' => Piece::new(PieceType::King, Player::White),
            'Q' => Piece::new(PieceType::Queen, Player::White),
            'R' => Piece::new(PieceType::Rook, Player::White),
            'B' => Piece::new(PieceType::Bishop, Player::White),
            'N' => Piece::new(PieceType::Knight, Player::White),
            'P' => Piece::new(PieceType::Pawn, Player::White),

            'k' => Piece::new(PieceType::King, Player::Black),
            'q' => Piece::new(PieceType::Queen, Player::Black),
            'r' => Piece::new(PieceType::Rook, Player::Black),
            'b' => Piece::new(PieceType::Bishop, Player::Black),
            'n' => Piece::new(PieceType::Knight, Player::Black),
            'p' => Piece::new(PieceType::Pawn, Player::Black),

            _ => return None,
        };

        Some(piece)
    }
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

impl std::fmt::Debug for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}
