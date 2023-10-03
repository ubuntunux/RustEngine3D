use log::LevelFilter;
use nalgebra::Vector2;

use ash::vk;
use winit::event::VirtualKeyCode;

use rust_engine_3d::constants;
use rust_engine_3d::application::application::{
    self,
    ProjectApplicationBase,
    EngineApplication,
    WindowMode
};
use rust_engine_3d::application::audio_manager::AudioManager;
use rust_engine_3d::application::scene_manager::ProjectSceneManagerBase;
use rust_engine_3d::effect::effect_manager::EffectManager;
use rust_engine_3d::renderer::renderer_data::RendererData;
use rust_engine_3d::renderer::ui::ProjectUIManagerBase;
use rust_engine_3d::utilities::system::{ptr_as_ref, ptr_as_mut};

pub struct ProjectApplication {
    pub _engine_application: *const EngineApplication,
    pub _audio_manager: *const AudioManager,
    pub _effect_manager: *const EffectManager,
    pub _renderer_data: *const RendererData,
    pub _project_resources: Box<ProjectResources>,
    pub _project_scene_manager: Box<ProjectSceneManager>,
    pub _project_ui_manager: Box<ProjectUIManager>,
    pub _game_client: Box<GameClient>,
    pub _is_game_mode: bool
}

fn main() {

}