use anyhow::Context;

mod autoplay;
mod chess;
mod constants;
mod epd;
mod performance_test;
mod search;
mod uci;

fn main() -> anyhow::Result<()> {
    #[cfg(feature = "log")]
    simplelog::WriteLogger::init(
        simplelog::LevelFilter::Info,
        simplelog::Config::default(),
        std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .append(true)
            .open("/tmp/rustybait_uci.log")?,
    )?;

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
        }
    }

    // Enter UCI mode
    uci::uci_talk()
}
