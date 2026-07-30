use crate::audio::audio_manager::AudioManager;
use crate::core::engine_core::EngineCore;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::RendererData;
use crate::resource::resource::EngineResources;
use crate::scene::debug_line::DebugLineManager;
use crate::scene::font::FontManager;
use crate::scene::scene_manager::SceneManager;
use crate::scene::ui::UIManager;
use crate::utilities::system::{ptr_as_mut, ptr_as_ref};
use std::ptr;

static mut ENGINE_CORE_PTR: *const EngineCore<'static> = ptr::null();

pub fn set_engine_core(engine_core: *const EngineCore) {
    unsafe {
        ENGINE_CORE_PTR = engine_core as *const EngineCore<'static>;
    }
}

// Global Getters
pub fn get_engine_core<'a>() -> &'a EngineCore<'a> {
    ptr_as_ref(unsafe { ENGINE_CORE_PTR } as *const EngineCore<'a>)
}

pub fn get_engine_core_mut<'a>() -> &'a mut EngineCore<'a> {
    ptr_as_mut(unsafe { ENGINE_CORE_PTR } as *const EngineCore<'a>)
}

pub fn get_engine_resources<'a>() -> &'a EngineResources<'a> {
    get_engine_core()._engine_resources.as_ref()
}

pub fn get_engine_resources_mut<'a>() -> &'a mut EngineResources<'a> {
    ptr_as_mut(get_engine_core()._engine_resources.as_ref())
}

pub fn get_audio_manager<'a>() -> &'a AudioManager {
    get_engine_core()._audio_manager.as_ref()
}

pub fn get_audio_manager_mut<'a>() -> &'a mut AudioManager {
    ptr_as_mut(get_engine_core()._audio_manager.as_ref())
}

pub fn get_effect_manager<'a>() -> &'a EffectManager<'a> {
    get_engine_core()._effect_manager.as_ref()
}

pub fn get_effect_manager_mut<'a>() -> &'a mut EffectManager<'a> {
    ptr_as_mut(get_engine_core()._effect_manager.as_ref())
}

pub fn get_scene_manager<'a>() -> &'a SceneManager<'a> {
    get_engine_core()._scene_manager.as_ref()
}

pub fn get_scene_manager_mut<'a>() -> &'a mut SceneManager<'a> {
    ptr_as_mut(get_engine_core()._scene_manager.as_ref())
}

pub fn get_renderer_data<'a>() -> &'a RendererData<'a> {
    get_engine_core()._renderer_context._renderer_data.as_ref()
}

pub fn get_renderer_data_mut<'a>() -> &'a mut RendererData<'a> {
    ptr_as_mut(get_engine_core()._renderer_context._renderer_data.as_ref())
}

pub fn get_renderer_context<'a>() -> &'a RendererContext<'a> {
    get_engine_core()._renderer_context.as_ref()
}

pub fn get_renderer_context_mut<'a>() -> &'a mut RendererContext<'a> {
    ptr_as_mut(get_engine_core()._renderer_context.as_ref())
}

pub fn get_ui_manager<'a>() -> &'a UIManager<'a> {
    get_engine_core()._ui_manager.as_ref()
}

pub fn get_ui_manager_mut<'a>() -> &'a mut UIManager<'a> {
    ptr_as_mut(get_engine_core()._ui_manager.as_ref())
}

pub fn get_font_manager<'a>() -> &'a FontManager {
    get_engine_core()._font_manager.as_ref()
}

pub fn get_font_manager_mut<'a>() -> &'a mut FontManager {
    ptr_as_mut(get_engine_core()._font_manager.as_ref())
}

pub fn get_debug_line_manager<'a>() -> &'a DebugLineManager {
    get_engine_core()._debug_line_manager.as_ref()
}

pub fn get_debug_line_manager_mut<'a>() -> &'a mut DebugLineManager {
    ptr_as_mut(get_engine_core()._debug_line_manager.as_ref())
}
