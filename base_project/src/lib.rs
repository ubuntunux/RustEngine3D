pub mod application;
pub mod constants;
pub mod render_pass_create_info;
pub mod renderer;

use rust_engine_3d::application::*;
use rust_engine_3d::constants::Constants;

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    let constants = Constants {
        ..Default::default()
    };
    application::run_application(constants);
}