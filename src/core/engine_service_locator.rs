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

pub struct EngineServiceLocator {
    pub _engine_core: *const EngineCore<'static>,
    pub _audio_manager: *const AudioManager,
    pub _effect_manager: *const EffectManager<'static>,
    pub _scene_manager: *const SceneManager<'static>,
    pub _renderer_data: *const RendererData<'static>,
    pub _renderer_context: *const RendererContext<'static>,
    pub _ui_manager: *const UIManager<'static>,
    pub _font_manager_ptr: *const FontManager,
    pub _debug_line_manager: *const DebugLineManager,
}

impl Default for EngineServiceLocator {
    fn default() -> Self {
        Self {
            _engine_core: ptr::null(),
            _audio_manager: ptr::null(),
            _effect_manager: ptr::null(),
            _scene_manager: ptr::null(),
            _renderer_data: ptr::null(),
            _renderer_context: ptr::null(),
            _ui_manager: ptr::null(),
            _font_manager_ptr: ptr::null(),
            _debug_line_manager: ptr::null(),
        }
    }
}

static mut ENGINE_SERVICE_LOCATOR: EngineServiceLocator = EngineServiceLocator {
    _engine_core: ptr::null(),
    _audio_manager: ptr::null(),
    _effect_manager: ptr::null(),
    _scene_manager: ptr::null(),
    _renderer_data: ptr::null(),
    _renderer_context: ptr::null(),
    _ui_manager: ptr::null(),
    _font_manager_ptr: ptr::null(),
    _debug_line_manager: ptr::null(),
};

static mut ENGINE_RESOURCES: *mut EngineResources<'static> = ptr::null_mut();

pub fn set_engine_resources(engine_resources: *mut EngineResources<'static>) {
    unsafe {
        ENGINE_RESOURCES = engine_resources;
    }
}

pub fn get_engine_service_locator() -> &'static EngineServiceLocator {
    ptr_as_ref(std::ptr::addr_of!(ENGINE_SERVICE_LOCATOR))
}

pub fn get_engine_service_locator_mut() -> &'static mut EngineServiceLocator {
    ptr_as_mut(std::ptr::addr_of!(ENGINE_SERVICE_LOCATOR))
}

pub fn register_engine_service_locator<'a>(
    engine_core: *const EngineCore<'a>,
    audio_manager: *const AudioManager,
    effect_manager: *const EffectManager<'a>,
    scene_manager: *const SceneManager<'a>,
    renderer_data: *const RendererData<'a>,
    renderer_context: *const RendererContext<'a>,
    ui_manager: *const UIManager<'a>,
    font_manager: *const FontManager,
    debug_line_manager: *const DebugLineManager,
) {
    let locator = get_engine_service_locator_mut();
    locator._engine_core = engine_core as *const EngineCore<'static>;
    locator._audio_manager = audio_manager as *const AudioManager;
    locator._effect_manager = effect_manager as *const EffectManager<'static>;
    locator._scene_manager = scene_manager as *const SceneManager<'static>;
    locator._renderer_data = renderer_data as *const RendererData<'static>;
    locator._renderer_context = renderer_context as *const RendererContext<'static>;
    locator._ui_manager = ui_manager as *const UIManager<'static>;
    locator._font_manager_ptr = font_manager;
    locator._debug_line_manager = debug_line_manager;
}

pub fn clear_engine_service_locator() {
    let locator = get_engine_service_locator_mut();
    *locator = EngineServiceLocator::default();
}

// Global Getters
pub fn get_engine_core<'a>() -> &'a EngineCore<'a> {
    ptr_as_ref(get_engine_service_locator()._engine_core as *const EngineCore<'a>)
}

pub fn get_engine_core_mut<'a>() -> &'a mut EngineCore<'a> {
    ptr_as_mut(get_engine_service_locator()._engine_core as *const EngineCore<'a>)
}

pub fn get_engine_resources<'a>() -> &'a EngineResources<'a> {
    ptr_as_ref(unsafe { ENGINE_RESOURCES } as *const EngineResources<'a>)
}

pub fn get_engine_resources_mut<'a>() -> &'a mut EngineResources<'a> {
    ptr_as_mut(unsafe { ENGINE_RESOURCES } as *const EngineResources<'a>)
}

pub fn get_audio_manager<'a>() -> &'a AudioManager {
    ptr_as_ref(get_engine_service_locator()._audio_manager as *const AudioManager)
}

pub fn get_audio_manager_mut<'a>() -> &'a mut AudioManager {
    ptr_as_mut(get_engine_service_locator()._audio_manager as *const AudioManager)
}

pub fn get_effect_manager<'a>() -> &'a EffectManager<'a> {
    ptr_as_ref(get_engine_service_locator()._effect_manager as *const EffectManager<'a>)
}

pub fn get_effect_manager_mut<'a>() -> &'a mut EffectManager<'a> {
    ptr_as_mut(get_engine_service_locator()._effect_manager as *const EffectManager<'a>)
}

pub fn get_scene_manager<'a>() -> &'a SceneManager<'a> {
    ptr_as_ref(get_engine_service_locator()._scene_manager as *const SceneManager<'a>)
}

pub fn get_scene_manager_mut<'a>() -> &'a mut SceneManager<'a> {
    ptr_as_mut(get_engine_service_locator()._scene_manager as *const SceneManager<'a>)
}

pub fn get_renderer_data<'a>() -> &'a RendererData<'a> {
    ptr_as_ref(get_engine_service_locator()._renderer_data as *const RendererData<'a>)
}

pub fn get_renderer_data_mut<'a>() -> &'a mut RendererData<'a> {
    ptr_as_mut(get_engine_service_locator()._renderer_data as *const RendererData<'a>)
}

pub fn get_renderer_context<'a>() -> &'a RendererContext<'a> {
    ptr_as_ref(get_engine_service_locator()._renderer_context as *const RendererContext<'a>)
}

pub fn get_renderer_context_mut<'a>() -> &'a mut RendererContext<'a> {
    ptr_as_mut(get_engine_service_locator()._renderer_context as *const RendererContext<'a>)
}

pub fn get_ui_manager<'a>() -> &'a UIManager<'a> {
    ptr_as_ref(get_engine_service_locator()._ui_manager as *const UIManager<'a>)
}

pub fn get_ui_manager_mut<'a>() -> &'a mut UIManager<'a> {
    ptr_as_mut(get_engine_service_locator()._ui_manager as *const UIManager<'a>)
}

pub fn get_font_manager() -> &'static FontManager {
    ptr_as_ref(get_engine_service_locator()._font_manager_ptr)
}

pub fn get_font_manager_mut() -> &'static mut FontManager {
    ptr_as_mut(get_engine_service_locator()._font_manager_ptr)
}

pub fn get_debug_line_manager() -> &'static DebugLineManager {
    ptr_as_ref(get_engine_service_locator()._debug_line_manager)
}

pub fn get_debug_line_manager_mut() -> &'static mut DebugLineManager {
    ptr_as_mut(get_engine_service_locator()._debug_line_manager)
}
