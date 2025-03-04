use super::weights::{NNUE_1_RAW, PSQT_RAW};

use nnue_constants::PSQT_SIZE;
pub use nnue_constants::{
    ACC_SCALE, ACCS, FINAL_SCALE, NNUE_1_LEN, PSQT_ACCS, occupancy_to_bucket,
};

pub const MATERIAL: [i16; 6] = {
    let mut sum = [0; 6];

    let mut acc = 0;

    while acc < PSQT_ACCS {
        let mut piece = 0;

        while piece < 6 {
            let mut i = 0;
            while i < 64 {
                sum[piece] += ACCUMULATORS[piece][i][NNUE_1_LEN + acc] as i64;
                sum[piece] += -ACCUMULATORS[6 + piece][i][NNUE_1_LEN + acc] as i64;

                i += 1;
            }

            piece += 1;
        }

        acc += 1;
    }

    [
        (sum[0] / 64 / 2 / PSQT_ACCS as i64) as i16,
        (sum[1] / 64 / 2 / PSQT_ACCS as i64) as i16,
        (sum[2] / 64 / 2 / PSQT_ACCS as i64) as i16,
        (sum[3] / 64 / 2 / PSQT_ACCS as i64) as i16,
        (sum[4] / 64 / 2 / PSQT_ACCS as i64) as i16,
        (sum[5] / 64 / 2 / PSQT_ACCS as i64) as i16,
    ]
};

pub const QUEEN_VALUE: i16 = MATERIAL[4];
pub const PAWN_VALUE: i16 = QUEEN_VALUE / 10;

pub const fn scale_to_cp(score: i16) -> i16 {
    (100.0 / PAWN_VALUE as f64 * score as f64) as i16
}

pub static ACCUMULATORS: [[[i16; ACCS]; 64]; 12] = {
    let mut i = 0;

    let mut arr = [[[0; ACCS]; 64]; 12];

    while i < 12 {
        let mut sq = 0;

        while sq < 64 {
            let mut acc = 0;

            while acc < NNUE_1_LEN {
                let start = acc * PSQT_SIZE;

                arr[i][sq][acc] = NNUE_1_RAW[start + i * 64 + (sq ^ 56)];

                acc += 1;
            }

            sq += 1;
        }

        i += 1;
    }

    let mut i = 0;

    while i < 12 {
        let mut sq = 0;

        while sq < 64 {
            let mut acc = 0;

            while acc < PSQT_ACCS {
                let start = acc * PSQT_SIZE;

                arr[i][sq][NNUE_1_LEN + acc] = PSQT_RAW[start + i * 64 + (sq ^ 56)];

                acc += 1;
            }

            sq += 1;
        }

        i += 1;
    }

    arr
};
