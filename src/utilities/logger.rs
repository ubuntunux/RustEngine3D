use env_logger;
use std::io::Write;
use chrono::Local;
use log::LevelFilter;

pub fn initialize_logger() {
    env_logger::Builder::new()
        .format(|buffer, record| {
            writeln!(buffer,
                     "{} [{}] {} ({} line:{})",
                     Local::now().format("%Y-%m-%dT%H:%M:%S"),
                     record.level(),
                     record.args(),
                     record.file().unwrap(),
                     record.line().unwrap(),
            )
        })
        .filter(None, LevelFilter::Trace)
        .init();
}