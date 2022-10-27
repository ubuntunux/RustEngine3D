use std::mem;
use std::collections::HashMap;

use serde::{ Serialize, Deserialize };
use ash::{ vk, Device };
use nalgebra::{ self, Vector2, Vector3, Vector4 };

use crate::renderer::mesh::{ MeshDataCreateInfo };
use crate::vulkan_context::buffer;
use crate::vulkan_context::vulkan_context::{ get_color32, get_format_size };
use crate::utilities::math;
use crate::utilities::bounding_box::{ BoundingBox, calc_bounding_box };

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct StaticVertexData {
    pub _position: Vector3<f32>,
    pub _normal: Vector3<f32>,
    pub _tangent: Vector3<f32>,
    pub _color: u32,
    pub _texcoord: Vector2<f32>
}

impl Default for StaticVertexData {
    fn default() -> StaticVertexData {
        StaticVertexData {
            _position: Vector3::new(0.0, 0.0, 0.0),
            _normal: Vector3::new(0.0, 0.0, 0.0),
            _tangent: Vector3::new(0.0, 0.0, 0.0),
            _color: 0,
            _texcoord: Vector2::new(0.0, 0.0)
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct SkeletalVertexData {
    pub _position: Vector3<f32>,
    pub _normal: Vector3<f32>,
    pub _tangent: Vector3<f32>,
    pub _color: u32,
    pub _texcoord: Vector2<f32>,
    pub _bone_indices: Vector4<u32>,
    pub _bone_weights: Vector4<f32>,
}

impl Default for SkeletalVertexData {
    fn default() -> SkeletalVertexData {
        SkeletalVertexData {
            _position: Vector3::new(0.0, 0.0, 0.0),
            _normal: Vector3::new(0.0, 0.0, 0.0),
            _tangent: Vector3::new(0.0, 0.0, 0.0),
            _color: 0,
            _texcoord: Vector2::new(0.0, 0.0),
            _bone_indices: Vector4::new(0, 0, 0, 0),
            _bone_weights: Vector4::new(0.0, 0.0, 0.0, 0.0),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct GeometryCreateInfo {
    pub _vertex_datas: Vec<StaticVertexData>,
    pub _skeletal_vertex_datas: Vec<SkeletalVertexData>,
    pub _indices: Vec<u32>,
    pub _bounding_box: BoundingBox,
}

impl Default for GeometryCreateInfo {
    fn default() -> GeometryCreateInfo {
        GeometryCreateInfo {
            _vertex_datas: Vec::new(),
            _skeletal_vertex_datas: Vec::new(),
            _indices: Vec::new(),
            _bounding_box: BoundingBox::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GeometryData {
    pub _geometry_name: String,
    pub _vertex_buffer_data: buffer::BufferData,
    pub _index_buffer_data: buffer::BufferData,
    pub _vertex_index_count: u32,
    pub _geometry_bounding_box: BoundingBox
}

pub fn add_vertex_input_attribute_description(
    vertex_input_attribute_descriptions: &mut Vec<vk::VertexInputAttributeDescription>,
    binding: u32,
    format: vk::Format
) {
    let location: u32 = vertex_input_attribute_descriptions.len() as u32;
    let offset: u32 = if 0 == location {
        0
    } else {
        let last_description = vertex_input_attribute_descriptions.last().unwrap();
        last_description.offset + get_format_size(last_description.format)
    };

    vertex_input_attribute_descriptions.push(vk::VertexInputAttributeDescription {
        location,
        binding,
        format,
        offset,
    });
}

pub trait VertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription>;
    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription>;
}

impl StaticVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const NORMAL: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const TANGENT: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const COLOR: vk::Format = vk::Format::R8G8B8A8_UNORM;
    const TEXCOORD: vk::Format = vk::Format::R32G32_SFLOAT;
}

impl VertexData for StaticVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, StaticVertexData::POSITION);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, StaticVertexData::NORMAL);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, StaticVertexData::TANGENT);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, StaticVertexData::COLOR);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, StaticVertexData::TEXCOORD);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![vk::VertexInputBindingDescription {
            binding: 0,
            stride: mem::size_of::<StaticVertexData>() as u32,
            input_rate: vk::VertexInputRate::VERTEX
        }]
    }
}

impl SkeletalVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const NORMAL: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const TANGENT: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const COLOR: vk::Format = vk::Format::R8G8B8A8_UNORM;
    const TEXCOORD: vk::Format = vk::Format::R32G32_SFLOAT;
    const BONE_INDICES: vk::Format = vk::Format::R32G32B32A32_UINT;
    const BONE_WEIGHTS: vk::Format = vk::Format::R32G32B32A32_SFLOAT;
}

impl VertexData for SkeletalVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::POSITION);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::NORMAL);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::TANGENT);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::COLOR);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::TEXCOORD);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::BONE_INDICES);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, SkeletalVertexData::BONE_WEIGHTS);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![vk::VertexInputBindingDescription {
            binding: 0,
            stride: mem::size_of::<SkeletalVertexData>() as u32,
            input_rate: vk::VertexInputRate::VERTEX
        }]
    }
}

pub fn create_geometry_data(
    device: &Device,
    command_pool: vk::CommandPool,
    command_queue: vk::Queue,
    device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    geometry_name: &String,
    geometry_create_info: &GeometryCreateInfo
) -> GeometryData {
    log::trace!("create_geometry_data: {:?}", geometry_name);

    let vertex_buffer_data = if false == geometry_create_info._skeletal_vertex_datas.is_empty() {
        buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &geometry_create_info._skeletal_vertex_datas,
        )
    } else {
        buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &geometry_create_info._vertex_datas,
        )
    };

    let index_buffer_data = buffer::create_buffer_data_with_uploads(
        device,
        command_pool,
        command_queue,
        device_memory_properties,
        vk::BufferUsageFlags::INDEX_BUFFER,
        &geometry_create_info._indices
    );

    GeometryData {
        _geometry_name: geometry_name.clone(),
        _vertex_buffer_data: vertex_buffer_data,
        _index_buffer_data: index_buffer_data,
        _vertex_index_count: geometry_create_info._indices.len() as u32,
        _geometry_bounding_box: geometry_create_info._bounding_box.clone()
    }
}

pub fn destroy_geometry_data(device: &Device, geometry_data: &GeometryData) {
    log::trace!("destroy_geometry_data");
    buffer::destroy_buffer_data(device, &geometry_data._vertex_buffer_data);
    buffer::destroy_buffer_data(device, &geometry_data._index_buffer_data);
}

//
// {-
//     Note: This point can also be considered as the vector starting from the origin to pi.
//     Writting this equation for the points p1, p2 and p3 give :
//         p1 = u1 * T + v1 * B
//         p2 = u2 * T + v2 * B
//         p3 = u3 * T + v3 * B
//     Texture/World space relation
//
//     With equation manipulation (equation subtraction), we can write :
//         p2 - p1 = (u2 - u1) * T + (v2 - v1) * B
//         p3 - p1 = (u3 - u1) * T + (v3 - v1) * B
//
//     By resolving this system :
//         Equation of Tangent:
//             (v3 - v1) * (p2 - p1) = (v3 - v1) * (u2 - u1) * T + (v3 - v1) * (v2 - v1) * B
//             (v2 - v1) * (p3 - p1) = (v2 - v1) * (u3 - u1) * T + (v2 - v1) * (v3 - v1) * B
//
//         Equation of Binormal:
//             (u3 - u1) * (p2 - p1) = (u3 - u1) * (u2 - u1) * T + (u3 - u1) * (v2 - v1) * B
//             (u2 - u1) * (p3 - p1) = (u2 - u1) * (u3 - u1) * T + (u2 - u1) * (v3 - v1) * B
//
//
//     And we finally have the formula of T and B :
//         T = ((v3 - v1) * (p2 - p1) - (v2 - v1) * (p3 - p1)) / ((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))
//         B = ((u3 - u1) * (p2 - p1) - (u2 - u1) * (p3 - p1)) / -((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))
//
//     Equation of N:
//         N = cross(T, B)
// -}
pub fn compute_tangent(
    positions: &Vec<Vector3<f32>>,
    normals: &Vec<Vector3<f32>>,
    texcoords: &Vec<Vector2<f32>>,
    indices: &Vec<u32>
) -> Vec<Vector3<f32>> {
    let vertex_count = positions.len();
    let index_count = indices.len();
    assert_eq!(0, index_count as u32 % 3);
    let mut tangent_map: HashMap<usize, Vector3<f32>> = HashMap::new();
    for i in (0..index_count).step_by(3) {
        let i0 = indices[i] as usize;
        let i1 = indices[i + 1] as usize;
        let i2 = indices[i + 2] as usize;
        let delta_pos_0_1 = &positions[i1] - &positions[i0];
        let delta_pos_0_2 = &positions[i2] - &positions[i0];
        let delta_uv_0_1 = &texcoords[i1] - &texcoords[i0];
        let delta_uv_0_2 = &texcoords[i2] - &texcoords[i0];
        let mut r: f32 = (delta_uv_0_1.x * delta_uv_0_2.y) - (delta_uv_0_1.y * delta_uv_0_2.x);
        if 0.0 != r {
            r = 1.0 / r;
        }
        let tangent_sq: Vector3<f32> = ((delta_pos_0_1 * delta_uv_0_2.y) - (delta_pos_0_2 * delta_uv_0_1.y)) * r;
        let result_tangent = if 0.0 == tangent_sq.dot(&tangent_sq) {
            let avg_normal: Vector3<f32> = (&normals[i0] + &normals[i1] + &normals[i2]).normalize();
            avg_normal.cross(&math::get_world_up())
        } else {
            tangent_sq.normalize()
        };
        tangent_map.insert(i0, result_tangent.clone());
        tangent_map.insert(i1, result_tangent.clone());
        tangent_map.insert(i2, result_tangent.clone());
    }
    let vertex_count_indices: Vec<usize> = (0..vertex_count).collect();
    let tangents: Vec<Vector3<f32>> = vertex_count_indices
        .iter()
        .map(|index| {
            tangent_map.get(index).unwrap().clone()
        }).collect();
    tangents
}

pub fn quad_mesh_create_info() -> MeshDataCreateInfo {
    let positions: Vec<Vector3<f32>> = vec![(-1.0, 1.0, 0.0), (1.0, 1.0, 0.0), (1.0, -1.0, 0.0), (-1.0, -1.0, 0.0)]
        .iter()
        .map(|(x, y, z)| {
            Vector3::new(*x, *y, *z)
        }).collect();
    let vertex_count = positions.len();
    let normals: Vec<Vector3<f32>> = vec![Vector3::new(0.0, 1.0, 0.0); vertex_count];
    let vertex_color = get_color32(255, 255, 255, 255);
    let texcoords: Vec<Vector2<f32>> = vec![Vector2::new(0.0, 0.0), Vector2::new(1.0, 0.0), Vector2::new(1.0, 1.0), Vector2::new(0.0, 1.0)];
    let indices: Vec<u32> = vec![0, 3, 2, 2, 1, 0];
    let tangents = compute_tangent(&positions, &normals, &texcoords, &indices);
    let vertex_datas = positions
        .iter()
        .enumerate()
        .map(|(index, __position)| {
            StaticVertexData {
                _position: positions[index].clone() as Vector3<f32>,
                _normal: normals[index].clone() as Vector3<f32>,
                _tangent: tangents[index].clone() as Vector3<f32>,
                _color: vertex_color,
                _texcoord: texcoords[index].clone() as Vector2<f32>,
            }
        }).collect();

    MeshDataCreateInfo::create_mesh_data_crate_info(MeshDataCreateInfo {
        _geometry_create_infos: vec![GeometryCreateInfo {
            _vertex_datas: vertex_datas,
            _indices: indices,
            _bounding_box: calc_bounding_box(&positions),
            ..Default::default()
        }],
        ..Default::default()
    })
}

pub fn cube_mesh_create_info() -> MeshDataCreateInfo {
    let positions: Vec<Vector3<f32>> =
        vec![(-0.5, 0.5, -0.5), (-0.5, -0.5, -0.5), (0.5, -0.5, -0.5), (0.5, 0.5, -0.5),
             (-0.5, 0.5, 0.5), (-0.5, -0.5, 0.5), (-0.5, -0.5, -0.5), (-0.5, 0.5, -0.5),
             (0.5, 0.5, 0.5), (0.5, -0.5, 0.5), (-0.5, -0.5, 0.5), (-0.5, 0.5, 0.5),
             (0.5, 0.5, -0.5), (0.5, -0.5, -0.5), (0.5, -0.5, 0.5), (0.5, 0.5, 0.5),
             (-0.5, -0.5, -0.5), (-0.5, -0.5, 0.5), (0.5, -0.5, 0.5), (0.5, -0.5, -0.5),
             (-0.5, 0.5, 0.5), (-0.5, 0.5, -0.5), (0.5, 0.5, -0.5), (0.5, 0.5, 0.5)]
            .iter()
            .map(|(x, y, z)| { Vector3::new(*x, *y, *z) }).collect();
    let normals: Vec<Vector3<f32>> =
        vec![(0.0, 0.0, -1.0), (0.0, 0.0, -1.0), (0.0, 0.0, -1.0), (0.0, 0.0, -1.0),
             (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0),
             (0.0, 0.0, 1.0), (0.0, 0.0, 1.0), (0.0, 0.0, 1.0), (0.0, 0.0, 1.0),
             (1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 0.0, 0.0),
             (0.0, -1.0, 0.0), (0.0, -1.0, 0.0), (0.0, -1.0, 0.0), (0.0, -1.0, 0.0),
             (0.0, 1.0, 0.0), (0.0, 1.0, 0.0), (0.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
            .iter()
            .map(|(x, y, z)| { Vector3::new(*x, *y, *z) }).collect();
    let texcoords: Vec<Vector2<f32>> =
        vec![(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0),
             (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0),
             (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0),
             (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0),
             (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0),
             (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
            .iter()
            .map(|(x, y)| Vector2::new(*x, *y)).collect();
    let vertex_color = get_color32(255, 255, 255, 255);
    let indices: Vec<u32> = vec![ 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6, 8, 10, 9, 8, 11, 10, 12, 14, 13, 12, 15, 14, 16, 18, 17, 16, 19, 18, 20, 22, 21, 20, 23, 22 ];
    let tangents = compute_tangent(&positions, &normals, &texcoords, &indices);
    let vertex_datas = positions
        .iter()
        .enumerate()
        .map(|(index, __position)| {
            StaticVertexData {
                _position: positions[index].clone() as Vector3<f32>,
                _normal: normals[index].clone() as Vector3<f32>,
                _tangent: tangents[index].clone() as Vector3<f32>,
                _color: vertex_color,
                _texcoord: texcoords[index].clone() as Vector2<f32>,
                ..Default::default()
            }
        }).collect();

    MeshDataCreateInfo::create_mesh_data_crate_info(MeshDataCreateInfo {
        _geometry_create_infos: vec![GeometryCreateInfo {
            _vertex_datas: vertex_datas,
            _indices: indices,
            _bounding_box: calc_bounding_box(&positions),
            ..Default::default()
        }],
        ..Default::default()
    })
}

pub fn plane_mesh_create_info(width: u32, height: u32, xz_plane: bool) -> MeshDataCreateInfo {
    let width_points: u32 = width + 1;
    let height_points: u32 = height + 1;
    let width_step: f32 = 1.0 / width as f32;
    let height_step: f32 = 1.0 / height as f32;
    let array_count: u32 = width_points * height_points;
    let vertex_count: u32 = 6; // quad
    let mut positions: Vec<Vector3<f32>> = vec![Vector3::zeros(); array_count as usize];
    let normals: Vec<Vector3<f32>> = vec![Vector3::new(0.0, 1.0, 0.0); array_count as usize];
    let mut texcoords: Vec<Vector2<f32>> = vec![Vector2::zeros(); array_count as usize];
    let mut indices: Vec<u32> = vec![0; (width * height * vertex_count) as usize];

    // generate positions, texcoords
    let mut array_index: usize = 0;
    for y in 0..height_points {
        let y: f32 = y as f32 * height_step;
        for x in 0..width_points {
            let x: f32 = x as f32 * width_step;
            if xz_plane {
                positions[array_index] = Vector3::new(x * 2.0 - 1.0, 0.0, 1.0 - y * 2.0);
            } else {
                positions[array_index] = Vector3::new(x * 2.0 - 1.0, 1.0 - y * 2.0, 0.0);
            }
            texcoords[array_index] = Vector2::new(x, 1.0 - y);
            array_index += 1;
        }
    }

    // generate indices
    let mut array_index: usize = 0;
    for y in 0..height {
        for x in 0..width {
            let i = y * width_points + x;
            indices[array_index + 0] = i;
            indices[array_index + 1] = i + 1;
            indices[array_index + 2] = i + 1 + width_points;
            indices[array_index + 3] = i;
            indices[array_index + 4] = i + 1 + width_points;
            indices[array_index + 5] = i + width_points;
            array_index += vertex_count as usize;
        }
    }

    let vertex_color = get_color32(255, 255, 255, 255);
    let tangents = compute_tangent(&positions, &normals, &texcoords, &indices);
    let vertex_datas = positions
        .iter()
        .enumerate()
        .map(|(index, __position)| {
            StaticVertexData {
                _position: positions[index].clone() as Vector3<f32>,
                _normal: normals[index].clone() as Vector3<f32>,
                _tangent: tangents[index].clone() as Vector3<f32>,
                _color: vertex_color,
                _texcoord: texcoords[index].clone() as Vector2<f32>,
                ..Default::default()
            }
        }).collect();

    MeshDataCreateInfo::create_mesh_data_crate_info(MeshDataCreateInfo {
        _geometry_create_infos: vec![GeometryCreateInfo {
            _vertex_datas: vertex_datas,
            _indices: indices,
            _bounding_box: calc_bounding_box(&positions),
            ..Default::default()
        }],
        ..Default::default()
    })
}