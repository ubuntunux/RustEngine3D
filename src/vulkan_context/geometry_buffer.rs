use std::mem;
use std::collections::HashMap;

use ash::{
    vk,
    Device,
};
use nalgebra;
use nalgebra::{
    Vector2,
    Vector3,
};

use crate::vulkan_context::buffer;
use crate::vulkan_context::vulkan_context::{
    get_color32,
    get_format_size,
};
use crate::utilities::bounding_box::{
    BoundingBox,
    calc_bounding_box
};


#[derive(Debug, Clone, Copy)]
pub struct VertexData {
    _position: Vector3<f32>,
    _normal: Vector3<f32>,
    _tangent: Vector3<f32>,
    _color: u32,
    _texcoord: Vector2<f32>
}

#[derive(Debug, Clone)]
pub struct GeometryCreateInfo {
    _vertex_datas: Vec<VertexData>,
    _indices: Vec<u32>,
    _bounding_box: BoundingBox
}

pub struct GeometryData {
    _geometry_name: String,
    _vertex_buffer_data: buffer::BufferData,
    _index_buffer_data: buffer::BufferData,
    _vertex_index_count: u32,
    _geometry_bounding_box: BoundingBox
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
        location: location,
        binding: binding,
        format: format,
        offset: offset,
    });
}

impl Default for VertexData {
    fn default() -> VertexData {
        VertexData {
            _position: Vector3::new(0.0, 0.0, 0.0),
            _normal: Vector3::new(0.0, 0.0, 0.0),
            _tangent: Vector3::new(0.0, 0.0, 0.0),
            _color: 0,
            _texcoord: Vector2::new(0.0, 0.0)
        }
    }
}

impl VertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const NORMAL: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const TANGENT: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const COLOR: vk::Format = vk::Format::R8G8B8A8_UNORM;
    const TEXCOORD: vk::Format = vk::Format::R32G32_SFLOAT;

    pub fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, VertexData::POSITION);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, VertexData::NORMAL);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, VertexData::TANGENT);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, VertexData::COLOR);
        add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, VertexData::TEXCOORD);
        vertex_input_attribute_descriptions
    }

    pub fn get_vertex_input_binding_descriptions() -> [vk::VertexInputBindingDescription; 1] {
        [vk::VertexInputBindingDescription {
            binding: 0,
            stride: mem::size_of::<VertexData>() as u32,
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
    log::info!("createGeometryBuffer: {:?}", geometry_name);
    let vertex_buffer_data = buffer::create_buffer_data_with_uploads(
        device,
        command_pool,
        command_queue,
        device_memory_properties,
        vk::BufferUsageFlags::VERTEX_BUFFER,
        &geometry_create_info._vertex_datas
    );
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
    log::info!("destroyGeometryData");
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
    let world_up: Vector3<f32> = Vector3::new(0.0, 1.0, 0.0);
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
        let tangent: Vector3<f32> = (((delta_pos_0_1 * delta_uv_0_2.y) - (delta_pos_0_2 * delta_uv_0_1.y)) * r).normalize();
        let avg_normal: Vector3<f32> = (&normals[i0] + &normals[i1] + &normals[i2]).normalize();
        let result_tangent = if 0.0 == tangent.dot(&tangent) {
            avg_normal.cross(&world_up)
        } else {
            tangent
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

pub fn quad_geometry_create_infos() -> Vec<GeometryCreateInfo> {
    let positions: Vec<Vector3<f32>> = vec![(-1.0, -1.0, 0.0), (1.0, -1.0, 0.0), (1.0, 1.0, 0.0), (-1.0, 1.0, 0.0)]
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
            VertexData {
                _position: positions[index].clone() as Vector3<f32>,
                _normal: normals[index].clone() as Vector3<f32>,
                _tangent: tangents[index].clone() as Vector3<f32>,
                _color: vertex_color,
                _texcoord: texcoords[index].clone() as Vector2<f32>,
            }
        }).collect();

    vec![GeometryCreateInfo {
        _vertex_datas: vertex_datas,
        _indices: indices,
        _bounding_box: calc_bounding_box(&positions)
    }]
}

pub fn cube_geometry_create_infos() -> Vec<GeometryCreateInfo> {
    let positions: Vec<Vector3<f32>> =
        vec![(-0.5, 0.5, 0.5), (-0.5, -0.5, 0.5), (0.5, -0.5, 0.5), (0.5, 0.5, 0.5),
             (0.5, 0.5, 0.5), (0.5, -0.5, 0.5), (0.5, -0.5, -0.5), (0.5, 0.5, -0.5),
             (0.5, 0.5, -0.5), (0.5, -0.5, -0.5), (-0.5, -0.5, -0.5), (-0.5, 0.5, -0.5),
             (-0.5, 0.5, -0.5), (-0.5, -0.5, -0.5), (-0.5, -0.5, 0.5), (-0.5, 0.5, 0.5),
             (-0.5, 0.5, -0.5), (-0.5, 0.5, 0.5), (0.5, 0.5, 0.5), (0.5, 0.5, -0.5),
             (-0.5, -0.5, 0.5), (-0.5, -0.5, -0.5), (0.5, -0.5, -0.5), (0.5, -0.5, 0.5)]
            .iter()
            .map(|(x, y, z)| { Vector3::new(*x, *y, *z) }).collect();
    let normals: Vec<Vector3<f32>> =
        vec![(0.0, 0.0, 1.0), (0.0, 0.0, 1.0), (0.0, 0.0, 1.0), (0.0, 0.0, 1.0),
             (1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 0.0, 0.0),
             (0.0, 0.0, -1.0), (0.0, 0.0, -1.0), (0.0, 0.0, -1.0), (0.0, 0.0, -1.0),
             (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0), (-1.0, 0.0, 0.0),
             (0.0, 1.0, 0.0), (0.0, 1.0, 0.0), (0.0, 1.0, 0.0), (0.0, 1.0, 0.0),
             (0.0, -1.0, 0.0), (0.0, -1.0, 0.0), (0.0, -1.0, 0.0), (0.0, -1.0, 0.0)]
            .iter()
            .map(|(x, y, z)| { Vector3::new(*x, *y, *z) }).collect();
    let texcoords: Vec<Vector2<f32>> =
        vec![(0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0),
             (0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0),
             (0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0),
             (0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0),
             (0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0),
             (0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0)]
            .iter()
            .map(|(x, y)| Vector2::new(*x, *y)).collect();
    let vertex_color = get_color32(255, 255, 255, 255);
    let indices: Vec<u32> = vec![ 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6, 8, 10, 9, 8, 11, 10, 12, 14, 13, 12, 15, 14, 16, 18, 17, 16, 19, 18, 20, 22, 21, 20, 23, 22 ];
    let tangents = compute_tangent(&positions, &normals, &texcoords, &indices);
    let vertex_datas = positions
        .iter()
        .enumerate()
        .map(|(index, __position)| {
            VertexData {
                _position: positions[index].clone() as Vector3<f32>,
                _normal: normals[index].clone() as Vector3<f32>,
                _tangent: tangents[index].clone() as Vector3<f32>,
                _color: vertex_color,
                _texcoord: texcoords[index].clone() as Vector2<f32>,
            }
        }).collect();

    vec![GeometryCreateInfo {
        _vertex_datas: vertex_datas,
        _indices: indices,
        _bounding_box: calc_bounding_box(&positions)
    }]
}