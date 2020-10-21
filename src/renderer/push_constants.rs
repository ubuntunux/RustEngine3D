use nalgebra::{
    Matrix4
};

pub trait PushConstantInterface {
    fn get_push_constants_size() -> u32;
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
    fn get_push_constants_size() -> u32 {
        std::mem::size_of::<PushConstants_StaticRenderObject>() as u32
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_SkeletalRenderObject {
    pub _model_matrixt: Matrix4<f32>,
}

impl Default for PushConstants_SkeletalRenderObject {
    fn default() -> PushConstants_SkeletalRenderObject {
        PushConstants_SkeletalRenderObject {
            _model_matrixt: Matrix4::identity(),
        }
    }
}

impl PushConstantInterface for PushConstants_SkeletalRenderObject {
    fn get_push_constants_size() -> u32 {
        std::mem::size_of::<PushConstants_SkeletalRenderObject>() as u32
    }
}