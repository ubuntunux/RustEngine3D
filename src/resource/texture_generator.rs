use std::cmp::min;
use std::fs;
use std::path::PathBuf;

use ash::vk;
use image;
use nalgebra::{
    Vector3,
    Vector4,
};
use rand;

use crate::constants;
use crate::renderer::renderer_context::RendererContext;
use crate::vulkan_context::texture::{ TextureData, TextureCreateInfo };
use crate::vulkan_context::vulkan_context;

fn generate_flat_color_image_rgba8(texture_source_directory: &PathBuf, file_path: &str, image_width: u32, image_height: u32, color: [u8; 4]) {
    let mut image_file_path = texture_source_directory.clone();
    image_file_path.push(file_path);
    if false == image_file_path.is_file() {
        log::debug!("generate_flat_color_image_rgba8: {:?}", image_file_path);
        let directory = image_file_path.parent().unwrap();
        if false == directory.is_dir() {
            fs::create_dir_all(directory).expect("Failed to create directories.");
        }
        let image = image::ImageBuffer::from_fn(image_width, image_height, |_x, _y| {
            image::Rgba(color)
        });
        image.save(image_file_path.as_path()).expect("Failed to save image.");
    }
}

pub fn generate_3d_data(size: u32) -> Vec<u32> {
    let value: f32 = 255.0 / size as f32;
    let buffer_size = size * size * size;
    let mut data: Vec<u32> = Vec::new();
    data.resize(buffer_size as usize, 255);
    for z in 0..size {
        for y in 0..size {
            for x in 0..size {
                let index = x + y * size + z * size * size;
                data[index as usize] = vulkan_context::get_color32(
                    min(255, (x as f32 * value) as u32),
                    min(255, (y as f32 * value) as u32),
                    min(255, (z as f32 * value) as u32),
                    255
                );
            }
        }
    }
    data
}

pub fn generate_random_data(image_width: u32, image_height: u32) -> Vec<Vector4<f32>> {
    let mut image_datas: Vec<Vector4<f32>> = Vec::new();
    image_datas.resize((image_width * image_height) as usize, Vector4::zeros());
    for image_data in image_datas.iter_mut() {
        image_data.x = rand::random::<f32>();
        image_data.y = rand::random::<f32>();
        image_data.z = rand::random::<f32>();
        image_data.w = rand::random::<f32>();
    }
    image_datas
}

pub fn generate_random_normals(image_width: u32, image_height: u32) -> Vec<Vector4<f32>> {
    let mut image_datas: Vec<Vector4<f32>> = Vec::new();
    image_datas.resize((image_width * image_height) as usize, Vector4::zeros());
    for image_data in image_datas.iter_mut() {
        let scale = rand::random::<f32>();
        let normal = Vector3::new(
            rand::random::<f32>() * 2.0 - 1.0,
            rand::random::<f32>() * 2.0 - 1.0,
            rand::random::<f32>()
        ).normalize() * scale;
        image_data.x = normal.x;
        image_data.y = normal.y;
        image_data.z = normal.z;
        image_data.w = 1.0;
    }
    image_datas
}

pub fn generate_images(texture_source_directory: &PathBuf) {
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_none.png", 2, 2, [0, 0, 0, 0]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_black.png", 2, 2, [0, 0, 0, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_gray.png", 2, 2, [128, 128, 128, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_white.png", 2, 2, [255, 255, 255, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_red.png", 2, 2, [255, 0, 0, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_green.png", 2, 2, [0, 255, 0, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_blue.png", 2, 2, [0, 0, 255, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_normal.png", 2, 2, [128, 128, 255, 255]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_white_no_alpha.png", 2, 2, [255, 255, 255, 0]);
    generate_flat_color_image_rgba8(texture_source_directory, "common/flat_normal_no_alpha.png", 2, 2, [128, 128, 255, 0]);
}

pub fn generate_textures(renderer_context: &RendererContext) -> Vec<TextureData> {
    let white = vulkan_context::get_color32(255, 255, 255, 255);
    let black = vulkan_context::get_color32(0, 0, 0, 255);
    let red = vulkan_context::get_color32(255, 0, 0, 255);
    let green = vulkan_context::get_color32(0, 255, 0, 255);
    let blue = vulkan_context::get_color32(0, 0, 255, 255);
    let yellow = vulkan_context::get_color32(255, 255, 0, 255);

    let default_3d_data = generate_3d_data(64);
    let texture_default_3d = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/default_3d"),
        _texture_width: 64,
        _texture_height: 64,
        _texture_layers: 64,
        _texture_format: vk::Format::R8G8B8A8_UNORM,
        _texture_view_type: vk::ImageViewType::TYPE_3D,
        _texture_min_filter: vk::Filter::NEAREST,
        _texture_mag_filter: vk::Filter::NEAREST,
        _texture_initial_datas: default_3d_data,
        _enable_mipmap: true,
        ..Default::default()
    });

    let default_2d_array_data = generate_3d_data(64);
    let texture_default_2d_array = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/default_2d_array"),
        _texture_width: 64,
        _texture_height: 64,
        _texture_layers: 64,
        _texture_format: vk::Format::R8G8B8A8_UNORM,
        _texture_view_type: vk::ImageViewType::TYPE_2D_ARRAY,
        _texture_min_filter: vk::Filter::NEAREST,
        _texture_mag_filter: vk::Filter::NEAREST,
        _texture_initial_datas: default_2d_array_data,
        _enable_mipmap: true,
        ..Default::default()
    });

    let random_data = generate_random_data(512, 512);
    let texture_random = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/random"),
        _texture_width: 512,
        _texture_height: 512,
        _texture_format: vk::Format::R32G32B32A32_SFLOAT,
        _texture_view_type: vk::ImageViewType::TYPE_2D,
        _texture_initial_datas: random_data,
        ..Default::default()
    });

    let random_normals = generate_random_normals(unsafe { constants::SSAO_NOISE_DIM as u32 }, unsafe { constants::SSAO_NOISE_DIM as u32 });
    let texture_random_normal = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/random_normal"),
        _texture_width: unsafe { constants::SSAO_NOISE_DIM as u32 },
        _texture_height: unsafe { constants::SSAO_NOISE_DIM as u32 },
        _texture_format: vk::Format::R32G32B32A32_SFLOAT,
        _texture_initial_datas: random_normals,
        ..Default::default()
    });
    let texture_check = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/checker"),
        _texture_width: 2,
        _texture_height: 2,
        _texture_min_filter: vk::Filter::NEAREST,
        _texture_mag_filter: vk::Filter::NEAREST,
        _texture_initial_datas: vec![white, black, black, white],
        ..Default::default()
    });
    let texture_color_cube = renderer_context.create_texture(&TextureCreateInfo {
        _texture_name: String::from("common/color_cube"),
        _texture_width: 1,
        _texture_height: 1,
        _texture_view_type: vk::ImageViewType::CUBE,
        _texture_initial_datas: vec![ white, black, red, green, blue, yellow ],
        ..Default::default()
    });

    vec![
        texture_default_3d,
        texture_default_2d_array,
        texture_random,
        texture_random_normal,
        texture_check,
        texture_color_cube
    ]
}