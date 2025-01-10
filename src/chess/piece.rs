use super::position::Position;
use super::scores;
use super::zobrist;
use super::Player;
use super::Score;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
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
}

impl PieceType {
    pub fn material_value(self) -> u8 {
        match self {
            Self::Pawn => 1,
            Self::Bishop => 3,
            Self::Knight => 3,
            Self::Rook => 5,
            Self::Queen => 9,
            Self::King => 100,
        }
    }
}

impl Piece {
    pub fn score(self, pos: Position) -> (Score, Score, u8) {
        (
            scores::MG_TABLE[self.as_index()][pos.as_index()],
            scores::EG_TABLE[self.as_index()][pos.as_index()],
            scores::GAMEPHASE_INC[self.as_index()],
        )
    }

    pub fn material_value(self) -> u8 {
        self.piece_type.material_value()
    }

    /// This returns a value in the range 0..12
    pub fn as_index(self) -> usize {
        let mut index = self.piece_type as usize;

        if self.owner == Player::Black {
            index += 6;
        }

        index
    }

    pub fn hash(self, position: Position) -> u64 {
        zobrist::PIECE[position.as_index()][self.as_index()]
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
        let owner = if piece.is_ascii_lowercase() {
            Player::Black
        } else {
            Player::White
        };

        let piece_type = match piece.to_ascii_uppercase() {
            'K' => PieceType::King,
            'Q' => PieceType::Queen,
            'R' => PieceType::Rook,
            'B' => PieceType::Bishop,
            'N' => PieceType::Knight,
            'P' => PieceType::Pawn,
            _ => return None,
        };

        Some(Self { piece_type, owner })
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
