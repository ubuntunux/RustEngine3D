use rand;
use nalgebra::{Vector3, Vector4};

use crate::constants;
use crate::renderer::buffer_data_infos::{ SSAOConstants };

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
}


impl Default for PostProcessData_SSAO {
    fn default() -> PostProcessData_SSAO {
        let mut random_normals: [Vector4<f32>; 64] = [Vector4::new(0.0, 0.0, 0.0, 0.0); constants::SSAO_KERNEL_SIZE];
        for i in 0..constants::SSAO_KERNEL_SIZE {
            let scale = rand::random::<f32>();
            let normal = Vector3::new(
                rand::random::<f32>() * 2.0 - 1.0,
                rand::random::<f32>() * 0.5 + 0.5,
                rand::random::<f32>() * 2.0 - 1.0
            ).normalize() * scale;
            random_normals[i] = Vector4::new(normal.x, normal.y, normal.z, 0.0);
        }

        PostProcessData_SSAO {
            _ssao_kernel_size: constants::SSAO_KERNEL_SIZE as i32,
            _ssao_radius: constants::SSAO_RADIUS,
            _ssao_noise_dim: constants::SSAO_NOISE_DIM,
            _ssao_constants: SSAOConstants {
                _ssao_kernel_samples: random_normals
            },
        }
    }
}
