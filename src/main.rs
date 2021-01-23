use rust_engine_3d::application::*;
use rust_engine_3d::utilities::logger;

fn main() {
    env_logger::init();
    logger::initialize_logger();
    run_application();
}