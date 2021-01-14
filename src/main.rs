use rust_engine_3d::application::*;
use rust_engine_3d::utilities::logger;

fn main() {
    logger::initialize_logger();
    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 1;
    let window_size: (u32, u32) = (800, 600);
    run_application(app_name, app_version, window_size);
}