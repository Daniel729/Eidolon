use std::time::Duration;

use eidolon::{
    chess::Game,
    constants::DEFAULT_HASH_SIZE,
    search::{
        HaltInfo, HistoryData, MAX_SCORE, MIN_SCORE, SearchInfo, TranspositionTable, main_search,
    },
};

pub fn bench(depth: u8) {
    let start = std::time::Instant::now();

    let mut search_info = SearchInfo {
        nodes: 0,
        max_depth: 0,
    };

    let halt_info = HaltInfo {
        start,
        duration: Duration::from_secs(1_000_000),
        search_is_running: None,
    };

    let mut table = TranspositionTable::new(DEFAULT_HASH_SIZE);

    for fen in &FENS {
        table.clear();

        let mut game = Game::new(fen).unwrap();

        let mut history = HistoryData::default();

        for i in 1..depth {
            main_search(
                &mut game,
                &mut table,
                &mut search_info,
                &mut history,
                halt_info,
                0,
                i,
                MIN_SCORE,
                MAX_SCORE,
            );
        }
    }

    let elapsed = start.elapsed();

    println!("Elapsed: {:?}", elapsed);
    println!("Nodes:   {}", search_info.nodes);
    println!(
        "NPS:     {:.1}k",
        search_info.nodes as f64 / elapsed.as_secs_f64() / 1_000.0
    );
}

const FENS: [&str; 20] = [
    "5rk1/ppr1p1bp/5np1/P5P1/q2P1P2/2P4P/1R1BQ3/1N3RK1 b - -",
    "1r1qk2r/p2n1p2/4b2p/2p5/4B1pP/2BP2P1/P1P2P2/3QR1K1 b k -",
    "r2q1rk1/1p2ppbp/p3bnp1/2B1p3/2P5/2NP1QPP/PP3PB1/R4RK1 b - -",
    "3k4/1K5p/2P2bp1/P7/5P2/6P1/7P/2r5 b - -",
    "8/p3kp1p/2pp2p1/2n5/1K2p2P/P3P2R/3PBPP1/b1B5 w - -",
    "6k1/1p3pb1/p1p2n2/P2r2p1/NQ1Pp1Pp/4P2P/5P2/2R2RK1 b - -",
    "8/5k2/8/8/8/Pp6/2r5/5K2 w - -",
    "2rr2k1/1b6/4N1p1/p1p2p1p/P1N4b/3P4/1PP2PPP/R3R1K1 b - -",
    "3k4/1r6/6BB/8/3b4/6PP/5PK1/1r6 b - -",
    "3r2k1/1p3p2/p6p/4p3/5b1P/P2B1P2/1P3PK1/1Q6 w - -",
    "rnb1r1k1/pp3pp1/1q5p/2pPp3/4P3/8/PPQNBPPP/3RK2R w K -",
    "Q7/5k2/6pp/2b5/2b4P/4P1P1/5PK1/8 w - -",
    "r1b1kb1r/pp3ppp/2n1p3/2pq4/5P2/P4N2/1P1PQ2P/RNB1KB1R b KQkq -",
    "2b1k3/6R1/1P6/p4p1p/r2BpP2/4P2K/6PP/8 w - -",
    "2r1kb1r/pp1b1ppp/8/3Pn1q1/8/2NBP3/PP1QN1PP/R4RK1 b k -",
    "3b4/8/2k2p2/7P/p7/2BK4/8/8 w - -",
    "r6r/1b2k1pp/p3pp2/N2n4/3N4/7P/1P3PP1/R2R2K1 b - -",
    "r4rk1/2Q2pp1/b1p2n1p/p7/N1B5/1P2P3/P4PPP/2R2RK1 b - -",
    "8/1r6/1k5p/2pPbB2/8/pq4PP/4Q1K1/3R4 w - -",
    "rn1qkbnr/p1p2pp1/1p2p2p/1P1p4/5P2/2N1PN2/1PPP2PP/R1BQK2R w KQkq -",
];
