use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::render_object::RenderObjectData;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::render_pass::PipelinePushConstantData;

#[derive(Clone, Debug)]
pub struct RenderElementData {
    pub _render_object: RcRefCell<RenderObjectData>,
    pub _geometry_data: RcRefCell<GeometryData>,
    pub _material_instance_data: RcRefCell<MaterialInstanceData>,
    pub _push_constant_data_list: *const Vec<PipelinePushConstantData>,
}