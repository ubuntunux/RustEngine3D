use env_logger;
use std::io::Write;
use chrono::Local;
use log::LevelFilter;

use rust_engine_3d::application::*;

fn main() {
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
    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 1;
    let window_size: (u32, u32) = (1024, 786);
    run_application(app_name, app_version, window_size);
}