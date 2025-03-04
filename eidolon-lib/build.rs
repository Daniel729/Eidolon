use nnue_constants::{ACC_SCALE, FINAL_SCALE, NNUE_BUCKETS};
use safetensors::SafeTensors;
use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;

fn quantize(tensors: &SafeTensors<'_>, name: &str, scale: f32) {
    let data: Vec<u8> = tensors
        .tensor(name)
        .unwrap()
        .data()
        .chunks_exact(4)
        .map(|c| f32::from_le_bytes(c.try_into().unwrap()))
        .map(|x| (scale * x).round() as i16)
        .flat_map(|x| x.to_le_bytes())
        .collect();

    let out_dir = env::var("OUT_DIR").unwrap();

    let dest_path = Path::new(&out_dir).join(name);

    let mut file = File::create(dest_path).unwrap();

    file.write_all(&data[..]).unwrap();
}

fn quantize_buckets(tensors: &SafeTensors<'_>, name: &str, scale: f32) {
    let mut all_data = Vec::new();

    for i in 0..NNUE_BUCKETS {
        let name = format!("{}.{}.weight", name, i);
        let data: Vec<u8> = tensors
            .tensor(&name)
            .unwrap()
            .data()
            .chunks_exact(4)
            .map(|c| f32::from_le_bytes(c.try_into().unwrap()))
            .map(|x| (scale * x).round() as i16)
            .flat_map(|x| x.to_le_bytes())
            .collect();

        all_data.extend(data);
    }

    let out_dir = env::var("OUT_DIR").unwrap();

    let dest_path = Path::new(&out_dir).join(format!("{}.weight", name));

    let mut file = File::create(dest_path).unwrap();

    file.write_all(&all_data[..]).unwrap();
}

fn main() -> std::io::Result<()> {
    let model_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../eidolon_nnue.safetensors");

    println!("cargo:rerun-if-changed={}", model_path);

    let mut model = std::fs::File::open(model_path).unwrap_or_else(|err| {
        eprintln!(
            "Model file could not be opened. Make sure it exists at: {}",
            model_path
        );
        eprintln!("Error: {}", err);

        std::process::exit(1);
    });

    let mut buffer = Vec::new();

    model.read_to_end(&mut buffer).unwrap();

    let tensors = SafeTensors::deserialize(&buffer).unwrap();

    quantize_buckets(&tensors, "fc0", FINAL_SCALE as f32);
    quantize(&tensors, "fc1.weight", ACC_SCALE as f32);
    quantize(&tensors, "fc1.bias", ACC_SCALE as f32);
    quantize(&tensors, "fc2.weight", FINAL_SCALE as f32);

    Ok(())
}
