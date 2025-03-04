use nnue_constants::PSQT_SIZE;

use super::scores::{NNUE_1_LEN, PSQT_ACCS};

pub static PSQT_RAW: [i16; PSQT_ACCS * PSQT_SIZE] = {
    let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/fc0.weight"));

    bytes_to_i16(bytes)
};

pub static NNUE_1_RAW: [i16; NNUE_1_LEN * PSQT_SIZE] = {
    let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/fc1.weight"));

    bytes_to_i16(bytes)
};

pub static NNUE_1_BIAS: [i16; NNUE_1_LEN] = {
    let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/fc1.bias"));

    bytes_to_i16(bytes)
};

pub static NNUE_2_WEIGHTS: [i16; 2 * NNUE_1_LEN] = {
    let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/fc2.weight"));

    bytes_to_i16(bytes)
};

const fn bytes_to_i16<const N: usize, const M: usize>(input: &[u8; N]) -> [i16; M] {
    assert!(N == 2 * M, "Model weights are not the expected size");

    let mut output = [0; M];

    let mut i = 0;

    while i < M {
        output[i] = i16::from_le_bytes([input[2 * i], input[2 * i + 1]]);

        i += 1;
    }

    output
}
