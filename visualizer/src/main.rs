use eidolon::chess::{
    position::Position,
    scores::{ACCUMULATORS, MATERIAL, NNUE_1_LEN, PSQT_ACCS},
};

fn main() {
    let beginning = r#"<body>
<script>
const firstLayer = ["#;

    let ending = r#"];
const canvasSize = 160;
const cellSize = canvasSize / 8;

function getColor(value, min, max) {
  const clamped_value = Math.max(min, Math.min(max, value));
  const normalized = (clamped_value - min) / (max - min);
  const gray = Math.round(normalized * 255);
  return `rgb(${gray}, ${gray}, ${gray})`;
}

function drawGrid(weights, canvas, rows, cols, max) {
  const ctx = canvas.getContext('2d');
  const maxWeight = max;
  const minWeight = -max;

  function getColor(value, min, max) {
    const mid = 0;
    if (value > mid) {
      const normalized = (value - mid) / (max - mid);
      return `rgb(255, ${255 - Math.round(normalized * 255)}, ${255 - Math.round(normalized * 255)})`;
    } else if (value < mid) {
      const normalized = (mid - value) / (mid - min);
      return `rgb(${255 - Math.round(normalized * 255)}, ${255 - Math.round(normalized * 255)}, 255)`;
    } else {
      return 'rgb(255, 255, 255)';
    }
  }

  for (let row = 0; row < rows; row++) {
    for (let col = 0; col < cols; col++) {
      ctx.fillStyle = getColor(weights[row * cols + col], minWeight, maxWeight);
      ctx.fillRect(col * cellSize, row * cellSize, cellSize, cellSize);
      }
  }
}

const container = document.createElement('div');
document.body.appendChild(container);

container.style.display = 'grid';
container.style.width = canvasSize * 12 + 10 * 11 + 'px';
container.style.gridTemplateColumns = 'repeat(12, 1fr)';
container.style.gridTemplateRows = 'repeat(64, auto)';
container.style.gap = '10px';

firstLayer.forEach((weights, index) => {
  const canvas = document.createElement('canvas');
  canvas.width = canvasSize;
  canvas.height = canvasSize;
  canvas.style.display = 'block';
  canvas.style.marginBottom = '5px';
  container.appendChild(canvas);
  drawGrid(weights, canvas, 8, 8, "#;

    let ending2 = r#");
});

  
</script>
</body>
"#;

    println!("{}", beginning);

    for acc in 0..PSQT_ACCS {
        for i in 0..6 {
            let name = match i {
                0 => "Our Pawn",
                1 => "Our Knight",
                2 => "Our Bishop",
                3 => "Our Rook",
                4 => "Our Queen",
                5 => "Our King",
                _ => unreachable!(),
            };

            println!("    // {} - PSQT - {}", name, acc);
            println!("[");

            for row in (0..8).rev() {
                for col in 0..8 {
                    let position = Position::new_assert(row, col);
                    let value =
                        ACCUMULATORS[i][position.as_index()][NNUE_1_LEN + acc] - MATERIAL[i];

                    print!("{:5},", value * 10);
                }
                println!();
            }
            println!("],");
            println!();
        }

        for _ in 0..6 {
            println!("[");
            for _ in 0..8 {
                for _ in 0..8 {
                    print!("{:5},", 0);
                }
                println!();
            }
            println!("],");
            println!();
        }
    }

    let mut max_weight: i16 = 0;
    let mut sum: u64 = 0;
    let mut count: u64 = 0;

    for acc in 0..NNUE_1_LEN {
        for j in 0..6 {
            for k in 0..2 {
                let i = 6 * k + j;

                let name = match i {
                    0 => "Our Pawn",
                    1 => "Our Knight",
                    2 => "Our Bishop",
                    3 => "Our Rook",
                    4 => "Our Queen",
                    5 => "Our King",
                    6 => "Enemy Pawn",
                    7 => "Enemy Knight",
                    8 => "Enemy Bishop",
                    9 => "Enemy Rook",
                    10 => "Enemy Queen",
                    11 => "Enemy King",
                    _ => unreachable!(),
                };

                println!("    // {} - {}", name, acc);
                println!("[");

                for row in (0..8).rev() {
                    for col in 0..8 {
                        let position = Position::new_assert(row, col);
                        let value = ACCUMULATORS[i][position.as_index()][acc];

                        if value.abs() > max_weight.abs() {
                            max_weight = value;
                        }

                        sum += value.unsigned_abs() as u64;
                        count += 1;

                        print!("{:5},", value);
                    }
                    println!();
                }
                println!("],");
                println!();
            }
        }
    }

    println!("{}", ending);

    println!("{}", max_weight.abs());

    println!("{}", ending2);

    eprintln!("Material: {:?}", MATERIAL);

    eprintln!("Max weight: {}", max_weight);
    eprintln!("Avg weight: {}", sum / count);
}
