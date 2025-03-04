pub const PSQT_SIZE: usize = 12 * 64;

pub const ACCS: usize = NNUE_1_LEN + PSQT_ACCS;

pub const NNUE_1_LEN: usize = 128;
pub const PSQT_ACCS: usize = NNUE_BUCKETS;

pub const NNUE_BUCKETS: usize = 8;

pub const ACC_SCALE: i32 = 1024;
pub const FINAL_SCALE: f64 = 100.0;

pub const fn occupancy_to_bucket(occupancy: u32) -> usize {
    let result = ((occupancy - 1) / 4) as usize;

    assert!(result < NNUE_BUCKETS);

    result
}
