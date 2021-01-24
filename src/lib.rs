#![allow(dead_code)]
pub mod constants;
pub mod application;
pub mod renderer;
pub mod resource;
pub mod utilities;
pub mod vulkan_context;

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    #[cfg(target_os = "android")]
    std::env::set_var("RUST_LOG", "debug");
    env_logger::init();
    application::run_application();
}