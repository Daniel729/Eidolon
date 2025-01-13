# Eidolon

A simple chess engine written in Rust.

## Features

- Support for the UCI protocol
- Single-threaded search
- Uses [PeSTO](https://www.talkchess.com/forum3/viewtopic.php?p=772515#p772515)'s piece-square table for a simple evaluation 
- At the moment uses both bitboards + 8x8 matrix for representing pieces
- Alpha-beta search with iterative deepening, move ordering, null move pruning, late move reduction, and quiescence searching
- Zobrist hashing, with hashes consistent acrosss compilations. (e.g. the starting position hash is always `4304F7D20D654676`)
- A transposition table, currently ~64MB in size
- Very roughly ~2400 ELO per my testing


## Installation

### Build Instructions

   ```bash
   $ cargo build --release
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

Hash: 4304F7D20D654676
Fen: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
PGN: 
Eval: mg 0, eg 0, phase 24

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

Hash: AC0ECD2828C11674
Fen: rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 0 1
PGN: 1. Ngf3 
Eval: mg 36, eg 47, phase 24

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