use ash::vk;
use nalgebra::{Matrix4, Vector4};
use crate::constants;
use crate::render_pass::common::capture_height_map;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::{RenderObjectType, RendererData};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::light::{DirectionalLight, DirectionalLightCreateInfo};
use crate::scene::render_element::RenderElementData;
use crate::utilities::math;
use crate::vulkan_context::debug_utils::ScopedDebugLabel;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::texture;
use crate::vulkan_context::texture::TextureRawData;

#[derive(Default)]
pub struct CaptureHeightMap<'a> {
    pub _render_height_map: bool,
    pub _capture_height_map_view: DirectionalLight,
    pub _static_render_elements: Vec<RenderElementData<'a>>,
    pub _skeletal_render_elements: Vec<RenderElementData<'a>>,
    pub _height_map_raw_data: TextureRawData,
    pub _read_back_frames: Vec<u64>,
}

impl<'a> CaptureHeightMap<'a> {
    pub fn create_capture_height_map(object_id: i64) -> CaptureHeightMap<'a> {
        unsafe {
            let capture_height_map_view = DirectionalLight::create_directional_light(
                object_id,
                &String::from("capture_height_map"),
                &DirectionalLightCreateInfo {
                    _position: Default::default(),
                    _rotation: math::get_top_down_view(),
                    _light_data: Default::default(),
                    _shadow_dimensions: Vector4::new(
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        -constants::CAPTURE_HEIGHT_MAP_DEPTH,
                        constants::CAPTURE_HEIGHT_MAP_DEPTH,
                    ),
                    _shadow_update_distance: 0.0
                }
            );

            CaptureHeightMap {
                _render_height_map: false,
                _capture_height_map_view: capture_height_map_view,
                _static_render_elements: Vec::new(),
                _skeletal_render_elements: Vec::new(),
                _height_map_raw_data: TextureRawData::None,
                _read_back_frames: Vec::new()
            }
        }
    }
    pub fn is_render_height_map(&self) -> bool {
        self._render_height_map
    }
    pub fn set_render_height_map(&mut self, is_render_height_map: bool) {
        self._render_height_map = is_render_height_map;
    }
    pub fn need_to_render_height_map(&self) -> bool {
        !self._static_render_elements.is_empty()
    }
    pub fn need_to_read_back_height_map(&mut self, current_render_frame: u64) -> bool {
        let mut need_to_read_back: bool = false;
        if self._read_back_frames.is_empty() == false {
            let mut read_back_frames: Vec<u64> = Vec::new();
            for read_back_frame in self._read_back_frames.iter() {
                if *read_back_frame <= current_render_frame {
                    need_to_read_back = true;
                } else {
                    read_back_frames.push(*read_back_frame);
                }
            }
            self._read_back_frames = read_back_frames;
        }
        need_to_read_back
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
    pub fn get_shadow_view_projection(&self) -> &Matrix4<f32> {
        self._capture_height_map_view.get_shadow_view_projection()
    }
    pub fn get_inv_shadow_view_projection(&self) -> &Matrix4<f32> {
        self._capture_height_map_view.get_inv_shadow_view_projection()
    }
    pub fn render_capture_height_map(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext<'a>,
        renderer_data: &RendererData<'a>
    ) {
        let _label_capture_height_map = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "capture_height_map",
        );

        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/clear_framebuffer",
            "clear_capture_height_map/clear",
            &quad_geometry_data,
            None,
            None,
            None,
        );

        // capture height map
        renderer_data.render_solid_object(
            renderer_context,
            command_buffer,
            swapchain_index,
            capture_height_map::get_render_pass_name(RenderObjectType::Static),
            self.get_static_render_elements()
        );
        self.clear_static_render_elements();

        // be able to read back at next frame
        self._read_back_frames.push(renderer_context.get_render_frame() + 1);
    }

    pub fn read_back_height_map(
        &mut self,
        command_buffer: vk::CommandBuffer,
        renderer_context: &RendererContext<'a>,
        renderer_data: &RendererData<'a>
    ) {
        let _label_render_debug = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer, "render_debug"
        );

        let texture_data = renderer_data.get_render_target(RenderTargetType::CaptureHeightMap);
        self._height_map_raw_data = texture::read_texture_data(
            renderer_context.get_device(),
            renderer_context.get_command_pool(),
            renderer_context.get_graphics_queue(),
            renderer_context.get_device_memory_properties(),
            renderer_context.get_debug_utils(),
            texture_data
        );
    }

    pub fn update_capture_height_map(&mut self, bounding_box: &BoundingBox) {
        let max_side = bounding_box._size.x.max(bounding_box._size.z) * 0.5;
        let shadow_dimensions = Vector4::new(
            max_side,
            max_side,
            -bounding_box._size.y * 0.5,
            bounding_box._size.y * 0.5,
        );
        self._capture_height_map_view.update_shadow_orthogonal(&shadow_dimensions);
        self._capture_height_map_view.update_light_data(&bounding_box._center);
    }
}