use anyhow::Context;
use eidolon::{chess::Game, perft::perft_uci};

mod autoplay;
mod bench;
mod epd;
mod uci;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args();
    args.next();

    if let Some(arg) = args.next() {
        if arg == "auto" {
            // Auto play in terminal
            let millis = args.next().map_or(1000, |s| s.parse().unwrap_or(1000));

            autoplay::autoplay(millis);
            return Ok(());
        } else if arg == "epd" {
            // Run EPD test suite
            let filename = args.next().context("No filename provided")?;
            let millis = args.next().map_or(100, |s| s.parse().unwrap_or(100));
            epd::epd(&filename, millis);

            return Ok(());
        } else if arg == "bench" {
            // Run benchmark
            let depth = args.next().map_or(10, |s| s.parse().unwrap_or(10));
            bench::bench(depth);
            return Ok(());
        } else if arg == "perft" {
            // Run benchmark
            let depth = args.next().map_or(10, |s| s.parse().unwrap_or(5));
            let game = Game::default();
            perft_uci(game, depth);
            return Ok(());
        }
    }

    // Enter UCI mode
    uci::uci_talk()
}
