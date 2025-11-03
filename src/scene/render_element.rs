use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::scene::render_object::RenderObjectData;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::render_pass::PipelinePushConstantData;

#[derive(Clone, Debug)]
pub struct RenderElementData<'a> {
    pub _geometry_data: RcRefCell<GeometryData>,
    pub _material_instance_data: RcRefCell<MaterialInstanceData<'a>>,
    pub _push_constant_data_list: Vec<PipelinePushConstantData>, // possible multiple push-constants for a render element
    pub _num_render_instances: u32,
}

#[derive(Clone, Debug)]
pub struct RenderElementInfo<'a> {
    pub _render_object: RcRefCell<RenderObjectData<'a>>,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _transform_offset: usize,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _is_render_height_map: bool,
    pub _geometry_index: usize,
    pub _geometry_data: RcRefCell<GeometryData>,
    pub _material_instance_data: RcRefCell<MaterialInstanceData<'a>>,
}
