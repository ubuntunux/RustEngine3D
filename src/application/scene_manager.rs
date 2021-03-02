use ash::Device;

use crate::renderer::renderer::RendererData;
use crate::resource::resource::Resources;
use crate::utilities::system::RcRefCell;

pub trait SceneManagerBase {
    fn initialize_scene_manager_data(&mut self, renderer_data: &RendererData, resources: &Resources, window_width: u32, window_height: u32);
    fn regist_scene_graphics_data(&self, renderer_data: &RcRefCell<RendererData>, resources: &RcRefCell<Resources>);
    fn initialize_scene_graphics_data(&self, renderer_data: &RendererData, resources: &Resources);
    fn destroy_scene_graphics_data(&self, device: &Device);
    fn get_window_size(&self) -> (u32, u32);
    fn set_window_size(&mut self, width: u32, height: u32);
    fn resized_window(&mut self, width: u32, height: u32);
    fn open_scene_manager_data(&mut self, resources: &Resources);
    fn close_scene_manager_data(&mut self, device: &Device);
    fn update_scene_manager_data(&mut self, _elapsed_time: f64, delta_time: f64);
}

pub struct SceneManagerData {
    pub _renderer_data: RcRefCell<RendererData>,
    pub _resources: RcRefCell<Resources>,
    pub _scene_manager: RcRefCell<dyn SceneManagerBase>,
}

impl SceneManagerData {
    pub fn create_scene_manager_data(
        renderer_data: &RcRefCell<RendererData>,
        resources: &RcRefCell<Resources>,
        scene_manager: &RcRefCell<dyn SceneManagerBase>,
        window_width: u32,
        window_height: u32,
    ) -> SceneManagerData {
        let scene_manager_data = SceneManagerData {
            _renderer_data: renderer_data.clone(),
            _resources: resources.clone(),
            _scene_manager: scene_manager.clone(),
        };
        scene_manager.borrow_mut().initialize_scene_manager_data(&renderer_data.borrow(), &resources.borrow(), window_width, window_height);
        scene_manager_data
    }

    pub fn open_scene_manager_data(&mut self) {
        self._scene_manager.borrow_mut().open_scene_manager_data(&self._resources.borrow());
    }

    pub fn close_scene_manager_data(&mut self, device: &Device) {
        self._scene_manager.borrow_mut().close_scene_manager_data(device);
    }

    pub fn regist_scene_graphics_data(&self) {
        self._scene_manager.borrow_mut().regist_scene_graphics_data(&self._renderer_data, &self._resources);
    }

    pub fn initialize_scene_graphics_data(&self) {
        self._scene_manager.borrow_mut().initialize_scene_graphics_data(&self._renderer_data.borrow(), &self._resources.borrow());
    }

    pub fn destroy_scene_graphics_data(&self, device: &Device) {
        self._scene_manager.borrow_mut().destroy_scene_graphics_data(device);
    }
    pub fn resized_window(&self, width: u32, height: u32) {
        self._scene_manager.borrow_mut().resized_window(width, height);
    }

    pub fn update_scene_manager_data(&self, _elapsed_time: f64, delta_time: f64) {
        self._scene_manager.borrow_mut().update_scene_manager_data(_elapsed_time, delta_time);
    }
}