# RustyBait

A basic chess engine written in Rust.

## Features

- Support for the UCI protocol
- Single-threaded search
- Simple piece-square evaluation score
- Piece-centric board representation using an 8x8 matrix
- Alpha-beta search with iterative deepening, move ordering, and quiescence searching
- Can achieve search depths of 9-10 half-plies given a few seconds of search time, depending on the position and previous searches
- Zobrist hashing of the game position in order to utilize a transposition table, with hashes consistent acrosss different versions and compilations. (e.g. the starting position hash is always `D9C54592621D7040`)
- Around ~1700 ELO on Lichess, blitz time control 
- Uses `unsafe` in a few places to avoid index bound checking in order to gain a ~10% performance boost

## Installation

### Build Instructions

   ```bash
   cargo build --release
   ```

## Usage

1. Run the engine in UCI mode:
```
$ ./target/release/rustybait
> position startpos
> go infinite
[...]
> stop
bestmove g1f3
> position startpos moves g1f3
> show
Hash: D9C54592621D7040
Fen: rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq -
PGN: 1. Ngf3 

8 |♜|♞|♝|♛|♚|♝|♞|♜|
7 |♟|♟|♟|♟|♟|♟|♟|♟|
6 | | | | | | | | |
5 | | | | | | | | |
4 | | | | | | | | |
3 | | | | | |♘| | |
2 |♙|♙|♙|♙|♙|♙|♙|♙|
1 |♖|♘|♗|♕|♔|♗| |♖|
   a b c d e f g h
```

2. Additional commands
    1. Play by itself with a given time limit per move (in ms)
    ```
    $ ./target/release/rustybait auto 1000
    ```

    2. Running [perft](https://www.chessprogramming.org/Perft) on the start position up to a give depth
    ```
    $ ./target/release/rustybait perft 5
    a2a3: 181046
    a2a4: 217832
    b1a3: 198572
    b1c3: 234656
    [...]

    4865609
    ```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.