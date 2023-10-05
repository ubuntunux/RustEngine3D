use log::LevelFilter;
use nalgebra::Vector2;

use ash::vk;
use winit::event::VirtualKeyCode;

use rust_engine_3d::application::application::{
    self, EngineApplication, ProjectApplicationBase, WindowMode,
};
use rust_engine_3d::audio::audio_manager::AudioManager;
use rust_engine_3d::scene::scene_manager::SceneManager;
use rust_engine_3d::constants;
use rust_engine_3d::effect::effect_manager::EffectManager;
use rust_engine_3d::renderer::renderer_data::RendererData;
use rust_engine_3d::scene::ui::ProjectUIManagerBase;
use rust_engine_3d::utilities::system::{ptr_as_mut, ptr_as_ref};

pub struct ProjectApplication {
    pub _engine_application: *const EngineApplication,
    pub _audio_manager: *const AudioManager,
    pub _effect_manager: *const EffectManager,
    pub _renderer_data: *const RendererData,
    pub _project_resources: Box<ProjectResources>,
    pub _project_scene_manager: Box<SceneManager>,
    pub _project_ui_manager: Box<ProjectUIManager>,
    pub _game_client: Box<GameClient>,
    pub _is_game_mode: bool,
}

fn main() {}
