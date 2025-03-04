use std::{
    env,
    io::{BufRead, BufReader},
};

use eidolon::chess::{scores::FINAL_SCALE, Game};
use nnue_constants::PSQT_SIZE;
use tch::{nn, Device, Kind, Tensor};
use torch::{fen_to_arrays, ChessEvalNet, PATH_SAVED};

fn main() {
    let mut vs = nn::VarStore::new(Device::Cpu);
    let net = ChessEvalNet::new(&vs.root());

    vs.load(*PATH_SAVED).unwrap();

    let mut fens = Vec::new();

    let path = env::args().nth(1).unwrap();

    let file = std::fs::File::open(path).unwrap();
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.unwrap();

        let fen = line.split("c9").next().unwrap().trim();

        fens.push(fen.to_string());
    }

    let mut sum = 0.0;
    let mut sum_sq = 0.0;

    for fen in &fens {
        let (bucket, perspective1, perspective2) = fen_to_arrays(fen);

        let input = (
            vec![bucket],
            Tensor::from_slice(&perspective1)
                .view([1, PSQT_SIZE as i64])
                .to_kind(Kind::Float),
            Tensor::from_slice(&perspective2)
                .view([1, PSQT_SIZE as i64])
                .to_kind(Kind::Float),
        );

        let nn_score = net.forward(&input).double_value(&[0]);

        let game = Game::new(fen).unwrap();

        let real_score = game.score(true) as f64 / FINAL_SCALE;

        let diff = nn_score - real_score;

        sum += diff;
        sum_sq += diff * diff;
    }

    let mean = sum / fens.len() as f64;
    let variance = sum_sq / fens.len() as f64 - mean * mean;

    println!(
        "Mean: {:.3}e-3 Variance {:.3}e-3",
        mean * 1e3,
        variance * 1e3
    );
}
