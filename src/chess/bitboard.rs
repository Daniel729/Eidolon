use super::{deltas, position::Position};

pub const PAWN_ATTACK_WHITE: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);
            let start_index = position.as_index();

            if let Some(new_pos) = position.add((1, 1)) {
                let attack_index = new_pos.as_index();
                array[start_index] |= 1 << attack_index;
            }

            if let Some(new_pos) = position.add((1, -1)) {
                let attack_index = new_pos.as_index();
                array[start_index] |= 1 << attack_index;
            }

            col += 1;
        }

        row += 1;
    }

    array
};

pub const PAWN_ATTACK_BLACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);
            let start_index = position.as_index();

            if let Some(new_pos) = position.add((-1, 1)) {
                let attack_index = new_pos.as_index();
                array[start_index] |= 1 << attack_index;
            }

            if let Some(new_pos) = position.add((-1, -1)) {
                let attack_index = new_pos.as_index();
                array[start_index] |= 1 << attack_index;
            }

            col += 1;
        }

        row += 1;
    }

    array
};

pub const KNIGHT_ATTACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);
            let start_index = position.as_index();

            let mut i = 0;

            while i < 8 {
                let delta = deltas::DELTA_KNIGHT[i];

                if let Some(position) = position.add(delta) {
                    let attack_index = position.as_index();
                    array[start_index] |= 1 << attack_index;
                }

                i += 1;
            }

            col += 1;
        }

        row += 1;
    }

    array
};

pub const KING_ATTACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);
            let start_index = position.as_index();

            let mut i = 0;

            while i < 8 {
                let delta = deltas::DELTA_KING[i];

                if let Some(position) = position.add(delta) {
                    let attack_index = position.as_index();
                    array[start_index] |= 1 << attack_index;
                }

                i += 1;
            }

            col += 1;
        }

        row += 1;
    }

    array
};

macro_rules! add_delta {
    ($array:ident, $position:expr, $( $deltas:expr ),* ) => { $ (
        let mut i = 0;
        while i < $deltas.len() {
            let delta = $deltas[i];

            if let Some(new_pos) = $position.add(delta) {
                // if new_pos.col()  0 && new_pos.row() > 0 && new_pos.col() < 7 && new_pos.row() < 7 {
                let start_index = $position.as_index();
                let attack_index = new_pos.as_index();
                $array[start_index] |= 1 << attack_index;
                // }
            }

            i += 1;
        }
    )*};
}

pub const _ROOK_ATTACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);

            add_delta!(
                array,
                position,
                deltas::DELTA_ROOK_1,
                deltas::DELTA_ROOK_2,
                deltas::DELTA_ROOK_3,
                deltas::DELTA_ROOK_4
            );

            col += 1;
        }

        row += 1;
    }

    array
};

pub const _BISHOP_ATTACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);

            add_delta!(
                array,
                position,
                deltas::DELTA_BISHOP_1,
                deltas::DELTA_BISHOP_2,
                deltas::DELTA_BISHOP_3,
                deltas::DELTA_BISHOP_4
            );

            col += 1;
        }

        row += 1;
    }

    array
};

pub const _QUEEN_ATTACK: [u64; 64] = {
    let mut array = [0; 64];

    let mut row = 0;
    while row < 8 {
        let mut col = 0;

        while col < 8 {
            let position = Position::new_assert(row, col);

            add_delta!(
                array,
                position,
                deltas::DELTA_ROOK_1,
                deltas::DELTA_ROOK_2,
                deltas::DELTA_ROOK_3,
                deltas::DELTA_ROOK_4,
                deltas::DELTA_BISHOP_1,
                deltas::DELTA_BISHOP_2,
                deltas::DELTA_BISHOP_3,
                deltas::DELTA_BISHOP_4
            );

            col += 1;
        }

        row += 1;
    }

    array
};
