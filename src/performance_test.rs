use super::*;

// Documentation and source of correct values for perft: https://www.chessprogramming.org/Perft_Results

/// Performance Test
pub fn perft(game: &mut ChessGame, depth: u8) -> usize {
    let mut moves = ArrayVec::new();
    game.get_moves(&mut moves, true);

    let mut count = 0;
    if depth == 0 {
        return 1;
    } else if depth == 1 {
        for _move in moves.iter() {
            let _move = *_move;
            game.push(_move);
            count += 1;
            game.pop(_move);
        }
        return moves.len();
    }

    for _move in moves.iter() {
        let _move = *_move;
        game.push(_move);
        count += perft(game, depth - 1);
        game.pop(_move);
    }

    count
}

#[cfg(test)]

mod tests {
    use super::*;

    #[test]
    fn perft1_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 1), 20);
    }

    #[test]
    fn perft2_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 2), 400);
    }

    #[test]
    fn perft3_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 3), 8902);
    }

    #[test]
    fn perft4_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 4), 197281);
    }

    #[test]
    fn perft5_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 5), 4865609);
    }

    #[test]
    fn perft6_startpos() {
        let mut game = ChessGame::default();
        assert_eq!(perft(&mut game, 6), 119060324);
    }

    #[test]
    fn perft1_kiwipete() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        assert_eq!(perft(&mut game, 1), 48);
    }

    #[test]
    fn perft2_kiwipete() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        assert_eq!(perft(&mut game, 2), 2039);
    }

    #[test]
    fn perft3_kiwipete() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        assert_eq!(perft(&mut game, 3), 97862);
    }

    #[test]
    fn perft4_kiwipete() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        assert_eq!(perft(&mut game, 4), 4085603);
    }

    #[test]
    fn perft5_kiwipete() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        assert_eq!(perft(&mut game, 5), 193690690);
    }

    #[test]
    fn perft1_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 1), 14);
    }

    #[test]
    fn perft2_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 2), 191);
    }

    #[test]
    fn perft3_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 3), 2812);
    }

    #[test]
    fn perft4_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 4), 43238);
    }

    #[test]
    fn perft5_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 5), 674624);
    }

    #[test]
    fn perft6_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 6), 11030083);
    }

    #[test]
    fn perft7_position_3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&mut game, 7), 178633661);
    }

    #[test]
    fn perft1_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 1), 6);
    }

    #[test]
    fn perft2_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 2), 264);
    }

    #[test]
    fn perft3_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 3), 9467);
    }

    #[test]
    fn perft4_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 4), 422333);
    }

    #[test]
    fn perft5_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 5), 15833292);
    }

    #[test]
    fn perft6_position_4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        assert_eq!(perft(&mut game, 6), 706045033);
    }

    #[test]
    fn perft1_position_5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 1), 44);
    }

    #[test]
    fn perft2_position_5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 2), 1486);
    }

    #[test]
    fn perft3_position_5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 3), 62379);
    }

    #[test]
    fn perft4_position_5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 4), 2103487);
    }

    #[test]
    fn perft5_position_5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 5), 89941194);
    }

    #[test]
    fn perft1_position_6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(perft(&mut game, 1), 46);
    }

    #[test]
    fn perft2_position_6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(perft(&mut game, 2), 2079);
    }

    #[test]
    fn perft3_position_6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(perft(&mut game, 3), 89890);
    }

    #[test]
    fn perft4_position_6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(perft(&mut game, 4), 3894594);
    }

    #[test]
    fn perft5_position_6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(perft(&mut game, 5), 164075551);
    }

    #[test]
    fn perft_many_position5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(perft(&mut game, 1), 44);
        assert_eq!(perft(&mut game, 2), 1486);
        assert_eq!(perft(&mut game, 3), 62379);
        assert_eq!(perft(&mut game, 4), 2103487);
        assert_eq!(perft(&mut game, 5), 89941194);
    }

    #[test]
    fn zobrist1() {
        let mut game = ChessGame::default();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }

    #[test]
    fn zobrist2() {
        let mut game =
            ChessGame::new("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
                .unwrap();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }

    #[test]
    fn zobrist3() {
        let mut game = ChessGame::new("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }

    #[test]
    fn zobrist4() {
        let mut game =
            ChessGame::new("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0")
                .unwrap();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }

    #[test]
    fn zobrist5() {
        let mut game =
            ChessGame::new("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }

    #[test]
    fn zobrist6() {
        let mut game = ChessGame::new(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        let hash = game.hash();
        perft(&mut game, 4);
        assert_eq!(game.hash(), hash);
    }
}
