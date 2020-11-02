
use crate::renderer::render_object::RenderObjectData;
use crate::renderer::material_instance::MaterialInstanceData;
use crate::vulkan_context::geometry_buffer::GeometryData;

#[derive(Clone, Debug)]
pub struct RenderElementData {
    pub _render_object: RenderObjectData,
    pub _geometry_data: GeometryData,
    pub _material_instance_data: MaterialInstanceData,
}