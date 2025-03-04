use anyhow::Result;
use nnue_constants::PSQT_SIZE;
use std::{
    env::args,
    io::{BufRead, BufReader},
    path::PathBuf,
    time::Instant,
};
use tch::{
    Device, Kind, Tensor,
    nn::{self, OptimizerConfig},
};
use torch::{ChessEvalNet, PATH_SAVED, PATH_TO_SAVE, fen_to_arrays};

const BATCH_SIZE: usize = 1024;
const TEST_FRACTION: usize = 50;
const EPOCHS: usize = 10;
const INITIAL_LR: f64 = 2e-4;
const LR_DECAY: f64 = 0.75;
const L2_LAMBDA: f64 = 1e-2;

fn main() -> Result<()> {
    let arg = args().nth(1).unwrap();

    let paths = args().skip(2).map(PathBuf::from).collect::<Vec<_>>();

    if arg == "test" {
        return test_error(paths, true);
    } else if arg == "test_new" {
        return test_error(paths, false);
    } else if arg == "train" {
        return train(paths, true);
    } else if arg == "train_new" {
        return train(paths, false);
    }

    Ok(())
}

fn train(paths: Vec<PathBuf>, load_data: bool) -> Result<()> {
    let mut vs = nn::VarStore::new(Device::Cpu);
    let net = ChessEvalNet::new(&vs.root());

    let (train_data, test_data) = get_dataloader(paths, BATCH_SIZE)?;

    if load_data {
        vs.load(*PATH_SAVED).unwrap();
    }

    let mut lr = INITIAL_LR;
    let mut opt = nn::Adam::default().build(&vs, lr)?;
    let mut previous_loss = 1.0;

    for epoch in 1..=EPOCHS {
        opt.set_lr(lr);
        lr *= LR_DECAY;

        let mut total_loss = 0.0;

        let instant = Instant::now();

        for (inputs, raw_targets) in &train_data {
            opt.zero_grad();

            let len = inputs.1.size()[0] as f64;

            let raw_outputs = net.forward(&inputs);

            let sigmoid_outputs = raw_outputs.sigmoid();
            let sigmoid_targets: Tensor = raw_targets;

            let mse_loss = (sigmoid_outputs - sigmoid_targets)
                .square()
                .sum(Kind::Float);

            total_loss += mse_loss.double_value(&[]);

            let l2_reg_loss = len * L2_LAMBDA * net.fc1.ws.square().mean(Kind::Float);

            let loss = mse_loss + l2_reg_loss;

            loss.backward();
            opt.step();
        }

        let loss_training = total_loss / train_data.len() as f64;
        let delta_loss = previous_loss - loss_training;

        let l2_reg_loss = L2_LAMBDA * net.fc1.ws.square().mean(Kind::Float).double_value(&[]);

        if delta_loss > 0.0 {
            previous_loss = loss_training;

            vs.save(*PATH_TO_SAVE)?;
        }

        print!(
            "Epoch {:02} Loss: {:10.8} Data Loss: {:10.8} L2 Loss: {:10.8} Delta: {:10.8} ",
            epoch,
            loss_training + l2_reg_loss,
            loss_training,
            l2_reg_loss,
            delta_loss,
        );

        let mut total_test_loss = 0.0;

        for (inputs, raw_targets) in &test_data {
            let raw_outputs = net.forward(&inputs);

            let sigmoid_outputs = raw_outputs.sigmoid();
            let sigmoid_targets: Tensor = raw_targets;

            let loss: Tensor = (sigmoid_outputs - sigmoid_targets)
                .square()
                .sum(Kind::Float);

            total_test_loss += loss.double_value(&[]);
        }

        let loss_test = total_test_loss / test_data.len() as f64;

        println!(
            "Test Loss: {:10.8} Time: {:?}",
            loss_test,
            instant.elapsed()
        );
    }

    Ok(())
}

fn test_error(paths: Vec<PathBuf>, load_data: bool) -> Result<()> {
    let mut vs = nn::VarStore::new(Device::Cpu);
    let net = ChessEvalNet::new(&vs.root());

    if load_data {
        vs.load(*PATH_SAVED)?;
    } else {
        vs.save(*PATH_TO_SAVE)?;
    }

    let (train_data, test_data) = get_dataloader(paths, BATCH_SIZE)?;

    for (data, name) in [(&train_data, "Train"), (&test_data, "Test ")] {
        let instant = Instant::now();

        let mut total_loss = 0.0;

        for (inputs, raw_targets) in data {
            let len = inputs.1.size()[0] as f64;

            let raw_outputs = net.forward(&inputs);

            let sigmoid_outputs = raw_outputs.sigmoid();
            let sigmoid_targets: Tensor = raw_targets;

            let mse_loss = (sigmoid_outputs - sigmoid_targets)
                .square()
                .sum(Kind::Float);

            let l2_reg_loss = len * L2_LAMBDA * net.fc1.ws.square().mean(Kind::Float);

            let loss = mse_loss + l2_reg_loss;

            total_loss += &loss.double_value(&[]);
        }

        let loss_training = total_loss / data.len() as f64;

        let l2_reg_loss = L2_LAMBDA * net.fc1.ws.square().mean(Kind::Float).double_value(&[]);

        println!(
            "{} Data Loss: {:10.8} Data Loss: {:10.8} L2 Loss: {:10.8} Time {:?} ",
            name,
            loss_training,
            loss_training - l2_reg_loss,
            l2_reg_loss,
            instant.elapsed()
        );
    }

    Ok(())
}

struct DataLoader {
    data: Vec<(&'static str, f32)>,
    batch_size: usize,
}

impl DataLoader {
    fn len(&self) -> usize {
        self.data.len()
    }
}

fn get_dataloader(paths: Vec<PathBuf>, batch_size: usize) -> Result<(DataLoader, DataLoader)> {
    let mut train_data = DataLoader {
        data: Vec::new(),
        batch_size,
    };

    let mut test_data = DataLoader {
        data: Vec::new(),
        batch_size,
    };

    let mut files = Vec::new();

    for path in paths {
        let file = std::fs::File::open(path)?;
        let reader = BufReader::new(file);
        files.push(reader);
    }

    for file in files {
        for (i, line) in file.lines().enumerate() {
            let line = line.unwrap();

            let black_to_play = line.contains(" b ");

            let mut line = line.split("c9");

            let fen = line.next().unwrap().trim();

            let result = match line.next().unwrap().trim() {
                "\"1-0\";" => 1,
                "\"0-1\";" => -1,
                "\"1/2-1/2\";" => 0,
                _ => unreachable!(),
            };

            let label = {
                let mut label = match result {
                    1 => 1.0,
                    -1 => 0.0,
                    0 => 0.5,
                    _ => unreachable!(),
                };

                if black_to_play {
                    label = 1.0 - label;
                }

                label
            };

            let mut fen = fen.to_string();
            fen.shrink_to_fit();

            let fen = fen.leak();

            if i % TEST_FRACTION == 0 {
                test_data.data.push((fen, label));
            } else {
                train_data.data.push((fen, label));
            }
        }
    }

    train_data.data.shrink_to_fit();
    test_data.data.shrink_to_fit();

    println!("Finished loading input");

    Ok((train_data, test_data))
}

struct DataIter<'a> {
    data: &'a [(&'static str, f32)],
    batch_size: usize,

    // Randomized indexes in order to shuffle data
    permutation: Vec<usize>,

    // Next index to start at
    index: usize,
}

type Data = ((Vec<usize>, Tensor, Tensor), Tensor);

impl Iterator for DataIter<'_> {
    type Item = Data;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.data.len() {
            return None;
        }

        let mut labels = Vec::with_capacity(BATCH_SIZE);
        let mut buckets = Vec::with_capacity(BATCH_SIZE);
        let mut perspectives1 = Vec::with_capacity(BATCH_SIZE);
        let mut perspectives2 = Vec::with_capacity(BATCH_SIZE);

        for i in 0..self.batch_size {
            if self.index + i >= self.permutation.len() {
                break;
            }

            let idx = self.permutation[self.index + i];
            let (fen, label) = &self.data[idx];

            let (bucket, perspective1, perspective2) = fen_to_arrays(fen);

            labels.push(Tensor::from_slice(&[*label]).view([1, 1]));
            buckets.push(bucket);
            perspectives1.push(Tensor::from_slice(&perspective1).view([1, PSQT_SIZE as i64]));
            perspectives2.push(Tensor::from_slice(&perspective2).view([1, PSQT_SIZE as i64]));
        }

        let tensor_labels = Tensor::cat(&labels, 0).to_kind(Kind::Float);
        let tensor_perspectives1 = Tensor::cat(&perspectives1, 0).to_kind(Kind::Float);
        let tensor_perspectives2 = Tensor::cat(&perspectives2, 0).to_kind(Kind::Float);

        self.index += self.batch_size;

        Some((
            (buckets, tensor_perspectives1, tensor_perspectives2),
            tensor_labels,
        ))
    }
}

impl<'a> IntoIterator for &'a DataLoader {
    type Item = Data;

    type IntoIter = DataIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        let mut permutation: Vec<_> = (0..self.data.len()).collect();
        fastrand::shuffle(&mut permutation);

        DataIter {
            data: &self.data,
            batch_size: self.batch_size,
            permutation,
            index: 0,
        }
    }
}
