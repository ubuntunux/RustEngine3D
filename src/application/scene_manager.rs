use ash::Device;
use nalgebra::Vector2;

use crate::application::application::TimeData;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::camera::CameraObjectData;
use crate::renderer::font::FontManager;
use crate::renderer::light::DirectionalLightData;
use crate::renderer::render_element::RenderElementData;
use crate::renderer::renderer_context::RendererContext;
use crate::resource::resource::EngineResources;
use crate::utilities::system::RcRefCell;


pub trait ProjectSceneManagerBase {
    fn initialize_project_scene_manager(
        &mut self,
        scene_manager: &SceneManager,
        renderer_context: &RendererContext,
        effect_manager: &EffectManager,
        engine_resources: &EngineResources,
        window_size: &Vector2<i32>,
    );
    fn initialize_scene_graphics_data(&self);
    fn destroy_scene_graphics_data(&self, device: &Device);
    fn get_window_size(&self) -> &Vector2<i32>;
    fn set_window_size(&mut self, width: i32, height: i32);
    fn resized_window(&mut self, width: i32, height: i32);

    fn create_default_scene_data(&self, scene_data_name: &str);
    fn open_scene_data(&mut self, scene_data_name: &str);
    fn close_scene_data(&mut self, device: &Device);
    fn save_scene_data(&mut self);
    fn destroy_project_scene_manager(&mut self, device: &Device);
    fn update_project_scene_manager(&mut self, time_data: &TimeData, font_manager: &mut FontManager);

    fn get_main_camera(&self) -> &RcRefCell<CameraObjectData>;
    fn get_main_light(&self) -> &RcRefCell<DirectionalLightData>;
    fn get_light_probe_camera(&self, index: usize) -> &RcRefCell<CameraObjectData>;
    fn get_capture_height_map(&self) -> &RcRefCell<DirectionalLightData>;
    fn get_static_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_static_shadow_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_skeletal_shadow_render_elements(&self) -> &Vec<RenderElementData>;
}

pub struct SceneManager {
    pub _renderer_context: RcRefCell<RendererContext>,
    pub _engine_resources: RcRefCell<EngineResources>,
    pub _project_scene_manager: *const dyn ProjectSceneManagerBase,
}

impl SceneManager {
    pub fn create_scene_manager(
        renderer_context: &RcRefCell<RendererContext>,
        engine_resources: &RcRefCell<EngineResources>,
        project_scene_manager: *const dyn ProjectSceneManagerBase
    ) -> SceneManager {
        SceneManager {
            _renderer_context: renderer_context.clone(),
            _engine_resources: engine_resources.clone(),
            _project_scene_manager: project_scene_manager,
        }
    }

    pub fn initialize_scene_manager(
        &mut self,
        window_size: &Vector2<i32>,
        renderer_context: &RendererContext,
        effect_manager: &EffectManager,
        engine_resources: &EngineResources
    ) {
        self.get_project_scene_manager_mut().initialize_project_scene_manager(
            self,
            renderer_context,
            effect_manager,
            engine_resources,
            window_size
        );
    }

    pub fn get_project_scene_manager(&self) -> &dyn ProjectSceneManagerBase {
        unsafe { &*self._project_scene_manager }
    }

    pub fn get_project_scene_manager_mut(&self) -> &mut dyn ProjectSceneManagerBase {
        unsafe { &mut *(self._project_scene_manager as *mut dyn ProjectSceneManagerBase) }
    }

    pub fn open_scene_data(&mut self) {
        self.get_project_scene_manager_mut().open_scene_data("default");
    }

    pub fn close_scene_data(&mut self, device: &Device) {
        self.get_project_scene_manager_mut().close_scene_data(device);
    }

    pub fn save_scene_data(&mut self) {
        self.get_project_scene_manager_mut().save_scene_data();
    }

    pub fn destroy_scene_manager(&mut self, device: &Device) {
        self.get_project_scene_manager_mut().destroy_project_scene_manager(device);
    }

    pub fn initialize_scene_graphics_data(&self) {
        self.get_project_scene_manager_mut().initialize_scene_graphics_data();
    }

    pub fn destroy_scene_graphics_data(&self, device: &Device) {
        self.get_project_scene_manager_mut().destroy_scene_graphics_data(device);
    }

    pub fn resized_window(&self, width: i32, height: i32) {
        self.get_project_scene_manager_mut().resized_window(width, height);
    }

    pub fn update_scene_manager(&self, time_data: &TimeData, font_manager: &mut FontManager) {
        self.get_project_scene_manager_mut().update_project_scene_manager(time_data, font_manager);
    }
}