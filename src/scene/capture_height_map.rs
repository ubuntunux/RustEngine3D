use nalgebra::{Matrix4, Vector3, Vector4};
use crate::constants;
use crate::scene::light::{DirectionalLight, DirectionalLightCreateInfo};
use crate::scene::render_element::RenderElementData;

pub struct CaptureHeightMap<'a> {
    pub _capture_height_map_view: DirectionalLight,
    pub _static_render_elements: Vec<RenderElementData<'a>>,
}

impl<'a> CaptureHeightMap<'a> {
    pub fn create_capture_height_map(object_id: i64) -> CaptureHeightMap<'a> {
        unsafe {
            let capture_height_map_view = DirectionalLight::create_directional_light(
                object_id,
                &String::from("capture_height_map"),
                &DirectionalLightCreateInfo {
                    _rotation: Vector3::new(std::f32::consts::PI * 0.5, 0.0, 0.0),
                    _shadow_dimensions: Vector4::new(
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        -constants::CAPTURE_HEIGHT_MAP_DEPTH,
                        constants::CAPTURE_HEIGHT_MAP_DEPTH,
                    ),
                    ..Default::default()
                }
            );

            CaptureHeightMap {
                _capture_height_map_view: capture_height_map_view,
                _static_render_elements: Vec::new()
            }
        }
    }
    pub fn need_to_capture_height_map(&self) -> bool {
        !self._static_render_elements.is_empty()
    }
    pub fn get_static_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._static_render_elements
    }
    pub fn clear_static_render_elements(&mut self) {
        self._static_render_elements.clear();
    }
    pub fn add_static_render_elements(&mut self, render_element: RenderElementData<'a>) {
        self._static_render_elements.push(render_element);
    }
    pub fn get_shadow_view_projection(&mut self) -> &Matrix4<f32> {
        self._capture_height_map_view.get_shadow_view_projection()
    }
    pub fn get_inv_shadow_view_projection(&mut self) -> &Matrix4<f32> {
        self._capture_height_map_view.get_inv_shadow_view_projection()
    }
    pub fn update_light_data(&mut self, view_position: &Vector3<f32>) {
        self._capture_height_map_view.update_light_data(view_position);
    }
}