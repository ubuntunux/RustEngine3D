use env_logger;
use std::io::Write;
use chrono::Local;
use log::LevelFilter;

#[cfg(target_os = "android")]
pub fn initialize_logger(log_level: LevelFilter) {
    let debug_level = match log_level {
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
pub fn initialize_logger(log_level: LevelFilter) {
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
        .filter(None, log_level)
        .init();
}