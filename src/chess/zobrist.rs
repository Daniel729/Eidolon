// Randomly generated constant numbers used for zobrist hashing
pub const BLACK_TO_MOVE: u64 = get_random_nums::<1>(0)[0];

pub const _EMPTY_PLACE: u64 = get_random_nums::<1>(1)[0];

pub const STATE: [u64; 256] = get_random_nums::<256>(2);

pub const PIECE: [[u64; 12]; 64] = {
    let flat_array = get_random_nums::<768>(3 + 256);

    let mut array = [[0u64; 12]; 64];
    let mut i = 0;

    while i < 64 {
        let mut j = 0;

        while j < 12 {
            array[i][j] = flat_array[i * 12 + j];
            j += 1;
        }

        i += 1;
    }
    array
};

const fn get_random_nums<const COUNT: usize>(start: usize) -> [u64; COUNT] {
    // Generated using this bash command:
    // $ dd if=/dev/urandom of=./zobrist_bytes.bin bs=1 count=8208
    const ZOBRIST_NUMS: &[u8; 8208] = include_bytes!("../../zobrist_bytes.bin");

    let mut result = [0u64; COUNT];

    let mut i = 0;

    while i < COUNT {
        let bytes = [
            ZOBRIST_NUMS[start + i * 8],
            ZOBRIST_NUMS[start + i * 8 + 1],
            ZOBRIST_NUMS[start + i * 8 + 2],
            ZOBRIST_NUMS[start + i * 8 + 3],
            ZOBRIST_NUMS[start + i * 8 + 4],
            ZOBRIST_NUMS[start + i * 8 + 5],
            ZOBRIST_NUMS[start + i * 8 + 6],
            ZOBRIST_NUMS[start + i * 8 + 7],
        ];

        result[i] = u64::from_le_bytes(bytes);

        i += 1;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_zobrist_nums_are_unique() {
        let mut set = std::collections::HashSet::new();

        assert!(set.insert(BLACK_TO_MOVE));

        assert!(set.insert(_EMPTY_PLACE));

        for &num in &STATE {
            assert!(set.insert(num));
        }

        for board in &PIECE {
            for &num in board {
                assert!(set.insert(num));
            }
        }
    }
}
