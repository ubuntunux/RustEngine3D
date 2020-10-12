use nalgebra::{
    Matrix4
};

use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

pub trait PushConstantInterface {
}

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