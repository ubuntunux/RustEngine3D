use crate::render_pass::render_object::capture_height_map;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::{RenderObjectType, RendererData};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::height_map::HeightMapData;
use crate::scene::light::{DirectionalLight, DirectionalLightCreateInfo};
use crate::scene::render_element::RenderElementData;
use crate::scene::scene_manager::SceneObjectID;
use crate::utilities::math;
use crate::vulkan_context::debug_utils::ScopedDebugLabel;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::texture;
use crate::vulkan_context::texture::TextureRawData;
use ash::vk;
use nalgebra::{Matrix4, Vector4};

const SCENE_TOP_VIEW_HEIGHT_MARGIN: f32 = 1.0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CaptureHeightMapState {
    #[default]
    Idle,
    PendingTerrainCapture,
    RenderingTerrain,
    WaitingTerrainReadback {
        _target_frame: u64,
    },
    PendingSceneTopView,
    RenderingSceneTopView,
}

#[derive(Default)]
pub struct CaptureHeightMap<'a> {
    pub _state: CaptureHeightMapState,
    pub _enable_capture_normal_map: bool,
    pub _capture_height_map_view: DirectionalLight,
    pub _static_render_elements: Vec<RenderElementData<'a>>,
    pub _skeletal_render_elements: Vec<RenderElementData<'a>>,
    pub _dead_zone: f32,
    pub _stretch_height_map_xz: bool,
    pub _bounding_box: BoundingBox,
    pub _height_map_data: HeightMapData,
}

impl<'a> CaptureHeightMap<'a> {
    pub fn create_capture_height_map(object_id: SceneObjectID) -> CaptureHeightMap<'a> {
        let capture_height_map_view = DirectionalLight::create_directional_light(
            object_id,
            &String::from("capture_height_map"),
            &DirectionalLightCreateInfo {
                _rotation: math::get_top_down_view(),
                ..Default::default()
            },
        );

        CaptureHeightMap {
            _state: CaptureHeightMapState::Idle,
            _enable_capture_normal_map: false,
            _capture_height_map_view: capture_height_map_view,
            _static_render_elements: Vec::new(),
            _skeletal_render_elements: Vec::new(),
            _dead_zone: 0.0,
            _stretch_height_map_xz: false,
            _bounding_box: BoundingBox::default(),
            _height_map_data: HeightMapData::default(),
        }
    }
    pub fn get_state(&self) -> CaptureHeightMapState {
        self._state
    }
    pub fn is_capture_height_map_complete(&self) -> bool {
        matches!(
            self._state,
            CaptureHeightMapState::Idle
                | CaptureHeightMapState::PendingSceneTopView
                | CaptureHeightMapState::RenderingSceneTopView
        )
    }
    pub fn request_terrain_capture(&mut self) {
        self._static_render_elements.clear();
        self._state = CaptureHeightMapState::PendingTerrainCapture;
    }
    pub fn request_scene_top_view(&mut self) {
        // Stage 1 automatically chains into PendingSceneTopView once terrain readback finishes
        if self.is_capture_height_map_complete() {
            self._static_render_elements.clear();
            self._state = CaptureHeightMapState::PendingSceneTopView;
        }
    }
    pub fn finish_terrain_gather(&mut self, bounding_box: BoundingBox, dead_zone: f32) {
        self.update_capture_height_map(bounding_box, dead_zone);
        if self._static_render_elements.is_empty() {
            self._state = CaptureHeightMapState::PendingSceneTopView;
        } else {
            self._state = CaptureHeightMapState::RenderingTerrain;
        }
    }
    pub fn finish_scene_top_view_gather(&mut self, bounding_box: BoundingBox) {
        if self._static_render_elements.is_empty() {
            self._state = CaptureHeightMapState::Idle;
        } else {
            self.update_scene_top_view(bounding_box);
            self._state = CaptureHeightMapState::RenderingSceneTopView;
        }
    }
    pub fn need_to_render_height_map(&self) -> bool {
        matches!(
            self._state,
            CaptureHeightMapState::RenderingTerrain | CaptureHeightMapState::RenderingSceneTopView
        ) && !self._static_render_elements.is_empty()
    }
    pub fn need_to_read_back_height_map(&self, current_render_frame: u64) -> bool {
        matches!(
            self._state,
            CaptureHeightMapState::WaitingTerrainReadback { _target_frame } if _target_frame <= current_render_frame
        )
    }
    pub fn get_static_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._static_render_elements
    }
    pub fn clear_static_render_elements(&mut self) {
        self._static_render_elements.clear();
    }
    pub fn reset_capture_height_map(&mut self) {
        self._static_render_elements.clear();
        self._skeletal_render_elements.clear();
        self._state = CaptureHeightMapState::Idle;
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
        renderer_data: &RendererData<'a>,
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
            self.get_static_render_elements(),
        );
        self.clear_static_render_elements();

        self._state = match self._state {
            CaptureHeightMapState::RenderingTerrain => CaptureHeightMapState::WaitingTerrainReadback {
                _target_frame: renderer_context.get_render_frame() + 1,
            },
            CaptureHeightMapState::RenderingSceneTopView => CaptureHeightMapState::Idle,
            other => other,
        };
    }

    pub fn read_back_height_map(
        &mut self,
        command_buffer: vk::CommandBuffer,
        renderer_context: &RendererContext<'a>,
        renderer_data: &RendererData<'a>,
    ) {
        let _label_render_debug = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_debug",
        );

        let mut normal_map_raw_data: Vec<Vector4<u8>> = Vec::new();
        if self._enable_capture_normal_map {
            let normal_map_texture = renderer_data.get_render_target(RenderTargetType::CaptureNormalMap);
            if let TextureRawData::R8G8B8A8_UNORM(texture_raw_data) = texture::read_texture_data(
                renderer_context.get_device(),
                renderer_context.get_command_pool(),
                renderer_context.get_graphics_queue(),
                renderer_context.get_device_memory_properties(),
                renderer_context.get_debug_utils(),
                normal_map_texture,
            ) {
                normal_map_raw_data = texture_raw_data;
            }
        }

        let mut height_map_raw_data: Vec<f32> = Vec::new();
        let height_map_texture = renderer_data.get_render_target(RenderTargetType::CaptureHeightMap);
        if let TextureRawData::R32(texture_raw_data) = texture::read_texture_data(
            renderer_context.get_device(),
            renderer_context.get_command_pool(),
            renderer_context.get_graphics_queue(),
            renderer_context.get_device_memory_properties(),
            renderer_context.get_debug_utils(),
            height_map_texture,
        ) {
            height_map_raw_data = texture_raw_data;
        }

        self._height_map_data.initialize_height_map_data(
            &self._bounding_box,
            height_map_texture._image_width as i32,
            height_map_texture._image_height as i32,
            &normal_map_raw_data,
            &height_map_raw_data,
            self._dead_zone,
        );
        // CPU terrain readback finished; transition to GPU-only full-scene top-view pass
        self._state = CaptureHeightMapState::PendingSceneTopView;
    }

    fn apply_capture_view_projection(&mut self, mut bounding_box: BoundingBox) -> BoundingBox {
        if self._stretch_height_map_xz == false {
            let mut bounding_box_min = bounding_box._min.clone();
            let mut bounding_box_max = bounding_box._max.clone();
            bounding_box_min.x = bounding_box_min.x.min(bounding_box_min.z);
            bounding_box_max.x = bounding_box_max.x.max(bounding_box_max.z);
            bounding_box_min.z = bounding_box_min.x;
            bounding_box_max.z = bounding_box_max.x;
            bounding_box = BoundingBox::create_bounding_box(&bounding_box_min, &bounding_box_max);
        }

        let shadow_dimensions = Vector4::new(
            bounding_box._extents.x,
            bounding_box._extents.z,
            -bounding_box._extents.y,
            bounding_box._extents.y,
        );
        self._capture_height_map_view.update_shadow_orthogonal(&shadow_dimensions);
        self._capture_height_map_view.update_light_data(&bounding_box._center);
        bounding_box
    }

    pub fn update_capture_height_map(&mut self, bounding_box: BoundingBox, dead_zone: f32) {
        self._dead_zone = dead_zone;
        self._bounding_box = self.apply_capture_view_projection(bounding_box);
    }

    pub fn update_scene_top_view(&mut self, bounding_box: BoundingBox) {
        let mut expanded_min = bounding_box._min.clone();
        let mut expanded_max = bounding_box._max.clone();
        expanded_min.y -= SCENE_TOP_VIEW_HEIGHT_MARGIN;
        expanded_max.y += SCENE_TOP_VIEW_HEIGHT_MARGIN;
        let expanded_box = BoundingBox::create_bounding_box(&expanded_min, &expanded_max);
        // Update GPU view projection only; keep self._bounding_box intact for CPU HeightMapData
        let _ = self.apply_capture_view_projection(expanded_box);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nalgebra::Vector3;

    #[test]
    fn test_scene_top_view_lifecycle_and_projection_isolation() {
        let mut capture = CaptureHeightMap::create_capture_height_map(uuid::Uuid::new_v4());
        assert_eq!(capture.get_state(), CaptureHeightMapState::Idle);
        assert!(capture.is_capture_height_map_complete());

        // Stage 1: Terrain capture requested -> not complete while pending or waiting readback
        capture.request_terrain_capture();
        assert_eq!(capture.get_state(), CaptureHeightMapState::PendingTerrainCapture);
        assert!(!capture.is_capture_height_map_complete());

        // Requesting scene top-view while Stage 1 is active must not clobber Stage 1
        capture.request_scene_top_view();
        assert_eq!(capture.get_state(), CaptureHeightMapState::PendingTerrainCapture);

        let terrain_bbox =
            BoundingBox::create_bounding_box(&Vector3::new(-50.0, 0.0, -50.0), &Vector3::new(50.0, 20.0, 50.0));
        capture.update_capture_height_map(terrain_bbox.clone(), -10.0);
        assert_eq!(capture._bounding_box._min, terrain_bbox._min);
        assert_eq!(capture._bounding_box._max, terrain_bbox._max);
        let terrain_vp = capture.get_shadow_view_projection().clone();

        // Waiting readback gates completion and readback frame cleanly
        capture._state = CaptureHeightMapState::WaitingTerrainReadback { _target_frame: 10 };
        assert!(!capture.is_capture_height_map_complete());
        assert!(!capture.need_to_read_back_height_map(9));
        assert!(capture.need_to_read_back_height_map(10));

        // Stage 2: Scene top-view updates GPU view projection without clobbering terrain _bounding_box
        capture._state = CaptureHeightMapState::PendingSceneTopView;
        assert!(capture.is_capture_height_map_complete());
        let full_scene_bbox =
            BoundingBox::create_bounding_box(&Vector3::new(-50.0, 0.0, -50.0), &Vector3::new(50.0, 45.0, 50.0));
        capture.update_scene_top_view(full_scene_bbox);
        assert_eq!(capture._bounding_box._max.y, 20.0);
        assert_ne!(*capture.get_shadow_view_projection(), terrain_vp);

        // Reset returns cleanly to Idle
        capture.reset_capture_height_map();
        assert_eq!(capture.get_state(), CaptureHeightMapState::Idle);
        assert!(capture.is_capture_height_map_complete());
        assert!(!capture.need_to_read_back_height_map(100));
    }

    #[test]
    fn test_capture_height_map_projection_matches_terrain_bounding_box() {
        let mut capture = CaptureHeightMap::create_capture_height_map(uuid::Uuid::new_v4());
        let bbox = BoundingBox::create_bounding_box(&Vector3::new(-40.0, -5.0, -40.0), &Vector3::new(60.0, 35.0, 60.0));
        capture.update_capture_height_map(bbox.clone(), -10.0);

        let vp = capture.get_shadow_view_projection();
        let p_center = vp * Vector4::new(bbox._center.x, bbox._center.y, bbox._center.z, 1.0);
        let p_top = vp * Vector4::new(bbox._center.x, bbox._max.y, bbox._center.z, 1.0);
        let p_bottom = vp * Vector4::new(bbox._center.x, bbox._min.y, bbox._center.z, 1.0);
        let p_min_x = vp * Vector4::new(bbox._min.x, bbox._center.y, bbox._center.z, 1.0);
        let p_max_x = vp * Vector4::new(bbox._max.x, bbox._center.y, bbox._center.z, 1.0);

        // Center projects to NDC (0, 0, 0.5)
        assert!(p_center.x.abs() < 1e-4, "center x={}", p_center.x);
        assert!(p_center.y.abs() < 1e-4, "center y={}", p_center.y);
        assert!((p_center.z - 0.5).abs() < 1e-4, "center z={}", p_center.z);
        // Reverse-Z top-down projection: max.y -> 1.0, min.y -> 0.0
        assert!((p_top.z - 1.0).abs() < 1e-4, "top z={}", p_top.z);
        assert!(p_bottom.z.abs() < 1e-4, "bottom z={}", p_bottom.z);
        // X extents map tightly to [-1, 1]
        assert!((p_min_x.x.abs() - 1.0).abs() < 1e-4, "min_x={}", p_min_x.x);
        assert!((p_max_x.x.abs() - 1.0).abs() < 1e-4, "max_x={}", p_max_x.x);
    }
}
