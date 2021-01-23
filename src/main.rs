use rust_engine_3d::application::*;
use rust_engine_3d::utilities::logger;

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
fn main() {
    #[cfg(target_os = "android")]
    std::env::set_var("RUST_LOG", "debug");
    env_logger::init();
    log::info!("run_application");

    //logger::initialize_logger();
    run_application();
}