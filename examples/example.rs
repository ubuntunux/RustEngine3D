use rust_engine_3d::core::engine_core::EngineCore;
use rust_engine_3d::audio::audio_manager::AudioManager;
use rust_engine_3d::effect::effect_manager::EffectManager;
use rust_engine_3d::renderer::renderer_data::RendererData;
use rust_engine_3d::scene::scene_manager::SceneManager;

pub struct ApplicationResources {}

pub struct GameUIManager {}

pub struct GameClient {}

pub struct Application {
    pub _engine_core: *const EngineCore,
    pub _audio_manager: *const AudioManager,
    pub _effect_manager: *const EffectManager,
    pub _renderer_data: *const RendererData,
    pub _game_scene_manager: Box<SceneManager>,
    pub _game_client: Box<GameClient>,
    pub _is_game_mode: bool,
}

fn main() {}
