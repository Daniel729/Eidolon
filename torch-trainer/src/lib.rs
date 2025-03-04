use std::sync::LazyLock;

use nnue_constants::{NNUE_1_LEN, NNUE_BUCKETS, PSQT_SIZE, occupancy_to_bucket};
use tch::{
    Kind, Tensor,
    nn::{self, LinearConfig, Module},
};

static NNUE_ROOT: LazyLock<&str> = LazyLock::new(|| {
    std::env::var("NNUE_ROOT")
        .expect("Please set the NNUE_ROOT environment variable appropiately")
        .leak()
});

pub static PATH_TO_SAVE: LazyLock<&str> = LazyLock::new(|| {
    let mut string = NNUE_ROOT.to_string();
    string.push_str("./eidolon_nnue_new.safetensors");

    string.leak()
});

pub static PATH_SAVED: LazyLock<&str> = LazyLock::new(|| {
    let mut string = NNUE_ROOT.to_string();
    string.push_str("./eidolon_nnue.safetensors");

    string.leak()
});

const MATERIAL: [i16; 6] = [86, 321, 338, 509, 1000, 5000];

/// Initial PSQT values
const INITIAL_PSQT: [i16; PSQT_SIZE] = {
    let mut arr = [0; PSQT_SIZE];

    let mut i = 0;
    while i < 64 {
        let mut j = 0;

        while j < 6 {
            arr[j * 64 + i] = MATERIAL[j];
            arr[(j + 6) * 64 + i] = -MATERIAL[j];

            j += 1;
        }

        i += 1;
    }

    arr
};

#[derive(Debug)]
pub struct ChessEvalNet {
    pub fc0: [nn::Linear; NNUE_BUCKETS],
    pub fc1: nn::Linear,
    pub fc2: nn::Linear,
}

fn screlu(x: &Tensor) -> Tensor {
    x.clamp(0.0, 1.0).square()
}

impl ChessEvalNet {
    pub fn new(vs: &nn::Path) -> ChessEvalNet {
        if NNUE_ROOT.len() == 0 {
            panic!("Please set the NNUE_ROOT environment variable appropiately");
        }

        let no_bias = LinearConfig {
            bias: false,
            ..Default::default()
        };

        let mut fc0: [_; 8] = (0..NNUE_BUCKETS)
            .map(|i| nn::linear(vs / "fc0" / i, PSQT_SIZE as i64, 1, no_bias))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        tch::no_grad(|| {
            for fc in fc0.iter_mut() {
                let tensor = 1.0 / 106.0
                    * Tensor::from_slice(&INITIAL_PSQT)
                        .view([1, PSQT_SIZE as i64])
                        .to_kind(Kind::Float);

                fc.ws.copy_(&tensor);
            }
        });

        let fc1 = nn::linear(
            vs / "fc1",
            PSQT_SIZE as i64,
            NNUE_1_LEN as i64,
            Default::default(),
        );
        let fc2 = nn::linear(vs / "fc2", 2 * NNUE_1_LEN as i64, 1, no_bias);

        ChessEvalNet { fc0, fc1, fc2 }
    }

    pub fn forward(
        &self,
        (buckets, perspective1, perspective2): &(Vec<usize>, Tensor, Tensor),
    ) -> Tensor {
        let mut groups: [Vec<i64>; NNUE_BUCKETS] = [const { Vec::new() }; 8];

        for (i, &b) in buckets.iter().enumerate() {
            groups[b].push(i as i64);
        }

        let mut material_results: Vec<Tensor> = (0..buckets.len()).map(|_| Tensor::new()).collect();

        for bucket in 0..NNUE_BUCKETS {
            let indices = &groups[bucket];

            let indices_tensor = Tensor::f_from_slice(indices).unwrap();

            let batch1 = perspective1.index_select(0, &indices_tensor);
            let batch2 = perspective2.index_select(0, &indices_tensor);

            let out1 = self.fc0[bucket].forward(&batch1);
            let out2 = self.fc0[bucket].forward(&batch2);

            let out = (out1 - out2) / 2.0;

            let outs = out.split(1, 0);

            for (idx, out) in indices.iter().zip(outs.into_iter()) {
                material_results[*idx as usize] = out;
            }
        }

        let material_score = Tensor::cat(&material_results, 0);

        let accs1 = self.fc1.forward(perspective1);
        let accs2 = self.fc1.forward(perspective2);

        let accs = tch::Tensor::cat(&[accs1, accs2], 1);

        let accs = screlu(&accs);

        let complex_positional = self.fc2.forward(&accs);

        material_score + complex_positional
    }
}

pub fn fen_to_arrays(fen: &str) -> (usize, [u8; PSQT_SIZE], [u8; PSQT_SIZE]) {
    let mut terms = fen.split_ascii_whitespace();

    let Some(pieces) = terms.next() else {
        panic!("Missing board");
    };

    let Some(player) = terms.next() else {
        panic!("Missing player");
    };

    let mut perspective1 = [0; PSQT_SIZE];
    let mut perspective2 = [0; PSQT_SIZE];

    let mut occupancy = 0;

    let mut row = 7;
    let mut col = 0;

    for character in pieces.chars() {
        match character {
            '/' => {
                if row == 0 {
                    panic!("Too many rows");
                }
                col = 0;
                row -= 1;
            }
            piece if piece.is_ascii_alphabetic() => {
                if col >= 8 {
                    panic!("Too many columns");
                }

                let piece_index = match piece {
                    'P' => 0,
                    'N' => 1,
                    'B' => 2,
                    'R' => 3,
                    'Q' => 4,
                    'K' => 5,

                    'p' => 6,
                    'n' => 7,
                    'b' => 8,
                    'r' => 9,
                    'q' => 10,
                    'k' => 11,

                    _ => panic!("Invalid piece"),
                };

                let piece_mirror_index = {
                    if piece_index < 6 {
                        piece_index + 6
                    } else {
                        piece_index - 6
                    }
                };

                let position_index = ((7 - row) * 8 + col) as usize;
                let position_mirror_index = (row * 8 + col) as usize;

                perspective1[piece_index * 64 + position_index] = 1;
                perspective2[piece_mirror_index * 64 + position_mirror_index] = 1;

                occupancy += 1;

                col += 1;
            }

            empty_count if character.is_ascii_digit() => {
                let count = (empty_count as u8 - b'0') as i8;
                col += count;
            }
            _ => panic!("Unknown character met"),
        }
    }

    if row != 0 || col != 8 {
        panic!("Invalid board size");
    }

    let bucket = occupancy_to_bucket(occupancy);

    if player == "w" {
        (bucket, perspective1, perspective2)
    } else if player == "b" {
        (bucket, perspective2, perspective1)
    } else {
        panic!("Invalid player");
    }
}
