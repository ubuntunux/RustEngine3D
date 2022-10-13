use crate::renderer::render_object::RenderObjectData;
use crate::renderer::material_instance::MaterialInstanceData;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::render_pass::PipelinePushConstantData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct RenderElementData {
    pub _render_object: RcRefCell<RenderObjectData>,
    pub _geometry_data: RcRefCell<GeometryData>,
    pub _material_instance_data: RcRefCell<MaterialInstanceData>,
    pub _push_constant_datas: *const Vec<PipelinePushConstantData>
}