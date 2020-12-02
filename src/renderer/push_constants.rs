use nalgebra::{
    Matrix4
};

pub trait PushConstantInterface {
    fn get_push_constants_size() -> u32;
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_StaticRenderObject {
    pub _local_matrix: Matrix4<f32>,
}

impl Default for PushConstants_StaticRenderObject {
    fn default() -> PushConstants_StaticRenderObject {
        PushConstants_StaticRenderObject {
            _local_matrix: Matrix4::identity(),
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
    pub _local_matrix: Matrix4<f32>,
    pub _bone_matrix_offset: u32,
    pub _bone_matrix_count: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstants_SkeletalRenderObject {
    fn default() -> PushConstants_SkeletalRenderObject {
        PushConstants_SkeletalRenderObject {
            _local_matrix: Matrix4::identity(),
            _bone_matrix_offset: 0,
            _bone_matrix_count: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

impl PushConstantInterface for PushConstants_SkeletalRenderObject {
    fn get_push_constants_size() -> u32 {
        std::mem::size_of::<PushConstants_SkeletalRenderObject>() as u32
    }
}