use rand;
use nalgebra::{
    Vector3,
    Matrix,
    ArrayStorage,
    U4,
    U64,
    Matrix4x1
};

use crate::constants;
use crate::renderer::uniform_buffer_data::{ SSAOConstants };

type Matrix4x64f = Matrix<f32, U4, U64, ArrayStorage<f32, U4, U64>>;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
}


impl Default for PostProcessData_SSAO {
    fn default() -> PostProcessData_SSAO {
        let mut random_normals: Matrix4x64f = Matrix4x64f::zeros();
        let (_rows, columns) = random_normals.shape();
        for i in 0..columns {
            let scale = rand::random::<f32>();
            let normal = Vector3::new(
                rand::random::<f32>() * 2.0 - 1.0,
                rand::random::<f32>() * 0.5 + 0.5,
                rand::random::<f32>() * 2.0 - 1.0
            ).normalize() * scale;
            random_normals.set_column(i, &Matrix4x1::new(normal.x, normal.y, normal.z, 0.0));
        }

        PostProcessData_SSAO {
            _ssao_kernel_size: constants::SSAO_KERNEL_SIZE,
            _ssao_radius: constants::SSAO_RADIUS,
            _ssao_noise_dim: constants::SSAO_NOISE_DIM,
            _ssao_constants: SSAOConstants {
                _ssao_kernel_samples: random_normals
            },
        }
    }
}
