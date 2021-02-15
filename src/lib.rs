#![allow(dead_code)]
pub mod constants;
pub mod application;
pub mod renderer;
pub mod resource;
pub mod utilities;
pub mod vulkan_context;

#[cfg_attr(target_os = "android", ndk_glue::main(backtrace = "on"))]
pub fn main() {
    application::run_application();
}