use ash::Device;

use crate::application::application::TimeData;
use crate::renderer::font::FontManager;
use crate::renderer::renderer::RendererData;
use crate::resource::resource::Resources;
use crate::utilities::system::RcRefCell;
use crate::renderer::effect::EffectManagerData;

pub trait SceneManagerBase {
    fn initialize_scene_manager(
        &mut self,
        window_width: u32,
        window_height: u32,
        scene_manager_data: &SceneManagerData,
        renderer_data: &RendererData,
        resources: &Resources,
        effect_manager_data: *const EffectManagerData,
    );
    fn initialize_scene_graphics_data(&self);
    fn destroy_scene_graphics_data(&self, device: &Device);
    fn get_window_size(&self) -> (u32, u32);
    fn set_window_size(&mut self, width: u32, height: u32);
    fn resized_window(&mut self, width: u32, height: u32);
    fn open_scene_data(&mut self, resources: &Resources);
    fn close_scene_data(&mut self, device: &Device);
    fn destroy_scene_manager_data(&mut self, device: &Device);
    fn update_scene_manager_data(&mut self, time_data: &TimeData, font_manager: &mut FontManager);
}

pub struct SceneManagerData {
    pub _renderer_data: RcRefCell<RendererData>,
    pub _resources: RcRefCell<Resources>,
    pub _effect_manager_data: *const EffectManagerData,
    pub _scene_manager: *const dyn SceneManagerBase,
}

impl SceneManagerData {
    pub fn create_scene_manager_data(
        renderer_data: &RcRefCell<RendererData>,
        resources: &RcRefCell<Resources>,
        scene_manager: *const dyn SceneManagerBase
    ) -> SceneManagerData {
        SceneManagerData {
            _renderer_data: renderer_data.clone(),
            _resources: resources.clone(),
            _effect_manager_data: std::ptr::null(),
            _scene_manager: scene_manager,
        }
    }

    pub fn initialize_scene_manager_data(
        &mut self,
        window_width: u32,
        window_height: u32,
        renderer_data: &RendererData,
        resources: &Resources,
        effect_manager_data: *const EffectManagerData,
    ) {
        self._effect_manager_data = effect_manager_data;
        self.get_scene_manager_mut().initialize_scene_manager(
            window_width,
            window_height,
            self,
            renderer_data,
            resources,
            effect_manager_data
        );
    }

    pub fn get_scene_manager(&self) -> &dyn SceneManagerBase {
        unsafe { &*self._scene_manager }
    }

    pub fn get_scene_manager_mut(&self) -> &mut dyn SceneManagerBase {
        unsafe { &mut *(self._scene_manager as *mut dyn SceneManagerBase) }
    }

    pub fn get_effect_manager_data(&self) -> &EffectManagerData {
        unsafe { &*self._effect_manager_data }
    }

    pub fn get_effect_manager_data_mut(&self) -> &mut EffectManagerData {
        unsafe { &mut *(self._effect_manager_data as *mut EffectManagerData) }
    }

    pub fn open_scene_data(&mut self) {
        self.get_scene_manager_mut().open_scene_data(&self._resources.borrow());
    }

    pub fn close_scene_data(&mut self, device: &Device) {
        self.get_scene_manager_mut().close_scene_data(device);
    }

    pub fn destroy_scene_manager_data(&mut self, device: &Device) {
        self.get_scene_manager_mut().destroy_scene_manager_data(device);
    }

    pub fn initialize_scene_graphics_data(&self) {
        self.get_scene_manager_mut().initialize_scene_graphics_data();
    }

    pub fn destroy_scene_graphics_data(&self, device: &Device) {
        self.get_scene_manager_mut().destroy_scene_graphics_data(device);
    }
    pub fn resized_window(&self, width: u32, height: u32) {
        self.get_scene_manager_mut().resized_window(width, height);
    }

    pub fn update_scene_manager_data(&self, time_data: &TimeData, font_manager: &mut FontManager) {
        self.get_scene_manager_mut().update_scene_manager_data(time_data, font_manager);
    }
}