use std::mem;

use ash::{
    vk,
    Device,
};
use ash::version::DeviceV1_0;
use nalgebra::{
    Vector2,
    Vector3,
    Matrix4
};

use crate::constants;
use crate::vulkan_context::buffer;
use crate::vulkan_context::vulkan_context::{
    get_color32,
    get_format_size,
    run_commands_once,
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
    _tex_coord: Vector2<f32>
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
    mut vertex_input_attribute_descriptions: &mut Vec<vk::VertexInputAttributeDescription>,
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
            _tex_coord: Vector2::new(0.0, 0.0)
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

    fn get_vertex_input_binding_description() -> vk::VertexInputBindingDescription {
        vk::VertexInputBindingDescription {
            binding: 0,
            stride: mem::size_of::<VertexData>() as u32,
            input_rate: vk::VertexInputRate::VERTEX
        }
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
// computeTangent :: Vector.Vector Vector3<f32> -> Vector.Vector Vector3<f32> -> Vector.Vector Vector2<f32> -> Vector.Vector u32 -> (Vector.Vector Vector3<f32>)
// computeTangent positions normals texcoords indices =
//     let vertexCount = length positions
//         indexCount = length indices
//         tangentList = DList.foldr (\i acc -> DList.append (computeTangent' i positions texcoords normals) acc) DList.empty (DList.fromList [0,3..(indexCount - 1)])
//         tangentMap = Map.fromList $ DList.toList tangentList
//         keys = DList.map (\(index, tangent) -> index) tangentList
//     in
//         Vector.fromList . DList.toList $ DList.map (\key -> fromJust $ Map.lookup key tangentMap) keys
//     where
//         computeTangent' :: Int -> Vector.Vector Vector3<f32> -> Vector.Vector Vector2<f32> -> Vector.Vector Vector3<f32> -> DList.DList (Int, Vector3<f32>)
//         computeTangent' i positions texcoords normals =
//             let i0 = fromIntegral $ indices Vector.! i
//                 i1 = fromIntegral $ indices Vector.! (i + 1)
//                 i2 = fromIntegral $ indices Vector.! (i + 2)
//                 deltaPos_0_1 = (positions Vector.! i1) - (positions Vector.! i0)
//                 deltaPos_0_2 = (positions Vector.! i2) - (positions Vector.! i0)
//                 deltaUV_0_1 = (texcoords Vector.! i1) - (texcoords Vector.! i0)
//                 deltaUV_0_2 = (texcoords Vector.! i2) - (texcoords Vector.! i0)
//                 S r = (deltaUV_0_1 .! Idx 0) * (deltaUV_0_2 .! Idx 1) - (deltaUV_0_1 .! Idx 1) * (deltaUV_0_2 .! Idx 0)
//                 r' = if r /= 0.0 then (1.0 / r) else 0.0
//                 tangent = safeNormalize $ (deltaPos_0_1 * (fromScalar $ deltaUV_0_2 .! Idx 1) - deltaPos_0_2 * (fromScalar $ deltaUV_0_1 .! Idx 1)) * (fromScalar $ scalar r')
//                 avg_normal = safeNormalize $ (normals Vector.! i0 + normals Vector.! i1 + normals Vector.! i2)
//                 resultTangent =
//                     if 0.0 == (dot tangent tangent) then
//                         cross avg_normal world_up
//                     else
//                         tangent
//             in
//                 DList.fromList [(i0, resultTangent), (i1, resultTangent), (i2, resultTangent)]
//

pub fn quad_geometry_createInfos() -> Vec<GeometryCreateInfo> {
    let positions: Vec<Vector3<f32>> = vec![Vector3::new(-1.0, -1.0, 0.0), Vector3::new(1.0, -1.0, 0.0), Vector3::new(1.0, 1.0, 0.0), Vector3::new(-1.0, 1.0, 0.0)];
    let vertex_count = positions.len() as u32;
    let normals: Vec<Vector3<f32>> = vec![Vector3::new(0.0, 1.0, 0.0); vertex_count as usize];
    let vertex_color = get_color32(255, 255, 255, 255);
    let tex_coords: Vec<Vector2<f32>> = vec![Vector2::new(0.0, 0.0), Vector2::new(1.0, 0.0), Vector2::new(1.0, 1.0), Vector2::new(0.0, 1.0)];
    let indeces: Vec<u32> = vec![0, 3, 2, 2, 1, 0];
    // TODO : taqngets = computeTangent positions normals texCoords indices
    let tangents = vec![Vector3::new(0.0, 1.0, 0.0); vertex_count as usize];
    let vertex_datas = positions
        .iter()
        .enumerate()
        .map(|(index, position)| {
            VertexData {
                _position: positions[index].clone(),
                _normal: normals[index].clone(),
                _tangent: tangents[index].clone(),
                _color: vertex_color,
                _tex_coord: tex_coords[index].clone(),
            }
        }).collect();

    vec![GeometryCreateInfo {
        _vertex_datas: vertex_datas,
        _indices: indeces,
        _bounding_box: calc_bounding_box(&positions)
    }]
}
//
// cubeGeometryCreateInfos :: [GeometryCreateInfo]
// cubeGeometryCreateInfos =
//     let vertexColor = getColor32 255 255 255 255
//         positionList = [(vec3 x y z) * 0.5 | (x, y, z) <- [
//             (-1, 1, 1), (-1, -1, 1), (1, -1, 1), (1, 1, 1),
//             (1, 1, 1), (1, -1, 1), (1, -1, -1), (1, 1, -1),
//             (1, 1, -1), (1, -1, -1), (-1, -1, -1), (-1, 1, -1),
//             (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (-1, 1, 1),
//             (-1, 1, -1), (-1, 1, 1), (1, 1, 1), (1, 1, -1),
//             (-1, -1, 1), (-1, -1, -1), (1, -1, -1), (1, -1, 1)]]
//         positions = Vector.fromList positionList
//         normals = Vector.fromList [vec3 x y z | (x, y, z) <- [
//             (0, 0, 1), (0, 0, 1), (0, 0, 1), (0, 0, 1),
//             (1, 0, 0), (1, 0, 0), (1, 0, 0), (1, 0, 0),
//             (0, 0, -1), (0, 0, -1), (0, 0, -1), (0, 0, -1),
//             (-1, 0, 0), (-1, 0, 0), (-1, 0, 0), (-1, 0, 0),
//             (0, 1, 0), (0, 1, 0), (0, 1, 0), (0, 1, 0),
//             (0, -1, 0), (0, -1, 0), (0, -1, 0), (0, -1, 0)]]
//         texCoords = Vector.fromList [vec2 x y | (x, y) <- [
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1)]]
//         indexList = [ 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6, 8, 10, 9, 8, 11, 10, 12, 14, 13, 12, 15, 14, 16, 18, 17, 16, 19, 18, 20, 22, 21, 20, 23, 22 ] :: [u32]
//         indices = Vector.fromList indexList
//         tangents = computeTangent positions normals texCoords indices
//         vertexCount = length positions
//         vertices = [VertexData (positions Vector.! i) (normals Vector.! i) (tangents Vector.! i) vertexColor (texCoords Vector.! i) | i <- [0..(vertexCount - 1)]]
//     in
//         [ GeometryCreateInfo
//             { _geometry_create_info_vertices = SVector.fromList vertices
//             , _geometry_create_info_indices = SVector.fromList indexList
//             , _geometry_create_info_bounding_box = calcBoundingBox positionList
//             }
//         ]