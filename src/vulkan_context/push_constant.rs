use nalgebra::{
    Matrix4
};

pub trait PushConstantInterface {
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_StaticRenderObject {
    pub _model_matrixt: Matrix4<f32>,
}

impl Default for PushConstants_StaticRenderObject {
    fn default() -> PushConstants_StaticRenderObject {
        PushConstants_StaticRenderObject {
            _model_matrixt: Matrix4::identity(),
        }
    }
}

impl PushConstantInterface for PushConstants_StaticRenderObject {

}