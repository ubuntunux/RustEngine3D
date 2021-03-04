pub mod application;
pub mod application_constants;
pub mod render_pass_create_info;
pub mod renderer;

use crate::application::application::run_application;

pub fn main() {
    run_application();
}