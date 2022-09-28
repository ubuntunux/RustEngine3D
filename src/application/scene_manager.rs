use nalgebra::Matrix4;

use crate::renderer::camera::CameraObjectData;
use crate::renderer::light::DirectionalLightData;
use crate::renderer::render_element::RenderElementData;
use crate::utilities::system::RcRefCell;

pub trait ProjectSceneManagerBase {
    fn get_main_camera(&self) -> &CameraObjectData;
    fn get_main_camera_mut(&self) -> &mut CameraObjectData;
    fn get_main_light(&self) -> &RcRefCell<DirectionalLightData>;
    fn get_light_probe_camera(&self, index: usize) -> &RcRefCell<CameraObjectData>;
    fn get_capture_height_map(&self) -> &RcRefCell<DirectionalLightData>;
    fn get_static_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_static_shadow_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_skeletal_shadow_render_elements(&self) -> &Vec<RenderElementData>;
    fn get_render_element_transform_count(&self) -> usize;
    fn get_render_element_transform_metrices(&self) -> &Vec<Matrix4<f32>>;
}