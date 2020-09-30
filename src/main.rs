use env_logger;

use rust_engine_3d::application::*;

fn main() {
    env_logger::init();
    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 0;
    run_application(app_name, app_version);
}