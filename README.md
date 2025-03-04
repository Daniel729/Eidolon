# Eidolon

A performant NNUE-powered chess engine written in Rust.

## Features

- Support for the UCI protocol
- (Only) Single-threaded search
- Uses 8 PSQT buckets + a single NNUE network with 2x(768 -> 128) -> 1 architecture, and the clamp(x, 0, 1) ^ 2 activation function
   - Uses runtime CPU detection to choose the best instruction set to use for inference (SSE2 / SSE4.2 / AVX2 / AVX512)
   - The current network was trained on the following data sets
      - [Zurichess's quiet labeled v7](https://bitbucket.org/zurichess/tuner/downloads/) - created by Alexandru Moșoi
      - [Lichess big3 resolved](https://archive.org/details/lichess-big3-resolved.7z) - created by Jay Honnold
      - [Ethereal's data dump](https://www.talkchess.com/forum3/viewtopic.php?t=75350) - created by Andrew Grant
- Uses both bitboards + 8x8 matrix for representing the game's state
- Alpha-beta search with iterative deepening, move ordering, null move pruning, late move reduction, and quiescence searching
- Zobrist hashing, with hashes consistent acrosss compilations. (e.g. the starting position hash is always `2DFBA8745DE39E4F`)
- A transposition table, defaults to 16MB in size
- Very roughly ~3000 ELO per my testing

## Installation

### Build Instructions

   ```bash
   $ cargo build --profile=release-lto --bin eidolon
   ```

## Usage

1. Run the engine in UCI mode:
```
$ ./target/release/eidolon
> position startpos
> go infinite
[...]
> stop
bestmove g1f3
> position startpos
> show

[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]


Hash: 2DFBA8745DE39E4F
Final eval (White's POV) 40
Current FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

8 |r|n|b|q|k|b|n|r|
7 |p|p|p|p|p|p|p|p|
6 | | | | | | | | |
5 | | | | | | | | |
4 | | | | | | | | |
3 | | | | | | | | |
2 |P|P|P|P|P|P|P|P|
1 |R|N|B|Q|K|B|N|R|

   a b c d e f g h

> position startpos moves g1f3
> show

[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]
1. Ngf3 

Hash: AC0ECD2828C11674
Final eval (White's POV) 9
Current FEN: rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 0 1

8 |r|n|b|q|k|b|n|r|
7 |p|p|p|p|p|p|p|p|
6 | | | | | | | | |
5 | | | | | | | | |
4 | | | | | | | | |
3 | | | | | |N| | |
2 |P|P|P|P|P|P|P|P|
1 |R|N|B|Q|K|B| |R|

   a b c d e f g h
```
2. Additional commands
    1. Selfplay with a given time limit per move (in ms)
    ```
    $ eidolon auto 100
    ```

    2. Running [perft](https://www.chessprogramming.org/Perft)
    ```
    $ eidolon
    position startpos
    go perft 5
    a2a3: 181046
    a2a4: 217832
    b1a3: 198572
    b1c3: 234656
    [...]

    4865609
    ```

## Contribution Guidelines

Please do not create pull requests for this repository. This project does not accept external contributions at this time.

You are free to fork the repository and make any modifications on your own fork if you find it helpful.

## License

Eidolon is free and distributed under the GNU General Public License version 3 (GPL v3) - see the [LICENSE](LICENSE) file for details.