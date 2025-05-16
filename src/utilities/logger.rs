use std::fs::File;
#[cfg(not(target_os = "android"))]
use chrono::Local;
use env_logger;
use log::LevelFilter;
#[cfg(not(target_os = "android"))]
use std::io::Write;
use crate::constants::IS_SHIPPING_BUILD;

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
    let mut builder = env_logger::Builder::new();
    builder.format(|buffer, record| {
        writeln!(
            buffer,
            "{} [{}] {} ({} line:{})",
            Local::now().format("%Y-%m-%dT%H:%M:%S"),
            record.level(),
            record.args(),
            record.file().unwrap(),
            record.line().unwrap(),
        )
    });
    builder.filter(None, log_level);

    if IS_SHIPPING_BUILD {
        let log_file = File::create(".log").expect("Could not create log file");
        let writer = std::io::BufWriter::new(log_file);
        builder.target(env_logger::Target::Pipe(Box::new(writer)));
    } else {
        builder.target(env_logger::Target::Stdout);
    }

    builder.init();
}
