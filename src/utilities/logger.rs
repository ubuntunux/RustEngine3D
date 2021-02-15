use env_logger;
use std::io::Write;
use chrono::Local;
use log::LevelFilter;

const LOG_LEVEL: LevelFilter = LevelFilter::Warn;

#[cfg(target_os = "android")]
pub fn initialize_logger() {
    let debug_level = match LOG_LEVEL {
        LevelFilter::Off => "off",
        LevelFilter::Error => "error",
        LevelFilter::Warn => "warn",
        LevelFilter::Info => "info",
        LevelFilter::Debug => "debug",
        LevelFilter::Trace => "trace",
    };
    std::env::set_var("RUST_LOG", debug_level);
    env_logger::init();
}
#[cfg(not(target_os = "android"))]
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
        .filter(None, LOG_LEVEL)
        .init();
}