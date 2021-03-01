use rust_engine_3d::application::*;

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    application::run_application();
}