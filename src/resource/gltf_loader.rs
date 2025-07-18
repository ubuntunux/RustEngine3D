use std::path::PathBuf;

use gltf;
use nalgebra::{Matrix4, Vector2, Vector3, Vector4};

use crate::constants;
use crate::scene::animation::{
    AnimationNodeCreateInfo, SkeletonDataCreateInfo, SkeletonHierarchyTree,
};
use crate::scene::mesh::MeshDataCreateInfo;
use crate::scene::bounding_box::BoundingBox;
use crate::utilities::math;
use crate::utilities::system;
use crate::vulkan_context::{geometry_buffer, vulkan_context};
use crate::vulkan_context::geometry_buffer::{GeometryCreateInfo, SkeletalVertexData, VertexData};

pub const SHOW_GLTF_LOG: bool = false;

type Point3 = [u32; 3];

pub struct MeshObject {
    pub name: String,
    pub group_name: String,
    pub mtl_name: String,
    pub indices: Vec<(Point3, Point3, Point3)>,
}

pub struct GLTF {
    pub meshes: Vec<MeshObject>,
    pub positions: Vec<Vector3<f32>>,
    pub normals: Vec<Vector3<f32>>,
    pub texcoords: Vec<Vector2<f32>>,
    pub filename: PathBuf,
}

pub fn log_assesor_view(prefix: &str, accessor: &gltf::Accessor, view: &gltf::buffer::View) {
    if SHOW_GLTF_LOG {
        log::info!("{} Type: {:?}, Index: {:?}, Dimensions: {:?}, Stride: {:?}, Count: {:?}, ByteLength: {:?}, Offset: {:?}, Target: {:?}",
            prefix,
            accessor.data_type(),
            accessor.index(),
            accessor.dimensions(),
            accessor.size(),
            accessor.count(),
            view.length(),
            view.offset(),
            view.target()
        );
    }
}

pub fn parsing_index_buffer(
    primitive: &gltf::Primitive,
    buffers: &Vec<gltf::buffer::Data>,
) -> Vec<u32> {
    let accessor = primitive.indices().unwrap();
    let view = accessor.view().unwrap();
    log_assesor_view("\t\t\tIndices", &accessor, &view);

    // convert index buffer to data
    let buffer_offset = view.offset();
    let buffer_length = view.length();
    let bytes: Vec<u8> = buffers[0].0[buffer_offset..(buffer_offset + buffer_length)].to_vec();
    let data_type = accessor.data_type();
    let stride = match data_type {
        gltf::accessor::DataType::U16 => 2,
        gltf::accessor::DataType::U32 => 4,
        _ => panic!("unimplemented type: {:?}", data_type)
    };
    let index_count = bytes.len() / stride;
    let mut indices: Vec<u32> = vec![0; index_count];
    for i in 0..index_count {
        let base_index = i * stride;
        let index_value =
            match accessor.data_type() {
                gltf::accessor::DataType::U16 => (bytes[base_index] as u32) | ((bytes[base_index + 1] as u32) << 8),
                gltf::accessor::DataType::U32 => (bytes[base_index] as u32) | ((bytes[base_index + 1] as u32) << 8) | ((bytes[base_index + 2] as u32) << 16) | ((bytes[base_index + 3] as u32) << 24),
                _ => panic!("unimplemented type: {:?}", accessor.data_type())
            };

        let remainder = i % 3;
        let mut index_of_indices = i;
        if constants::CONVERT_COORDINATE_SYSTEM_RIGHT_HANDED_TO_LEFT_HANDED {
            if 1 == remainder {
                index_of_indices = i + 1;
            } else if 2 == remainder {
                index_of_indices = i - 1;
            }
        }
        indices[index_of_indices] = index_value;
    }

    return indices;
}

pub fn parsing_vertex_buffer(
    is_skeletal_mesh: bool,
    parent_transform: &Matrix4<f32>,
    primitive: &gltf::Primitive,
    buffers: &Vec<gltf::buffer::Data>,
) -> (Vec<VertexData>, Vec<SkeletalVertexData>, bool) {
    let is_identity: bool = *parent_transform == Matrix4::identity();
    let mut find_tangent_data: bool = false;
    let mut vertex_data_list: Vec<VertexData> = Vec::new();
    let mut skeletal_vertex_data_list: Vec<SkeletalVertexData> = Vec::new();

    for attribute in primitive.attributes() {
        let semantic = &attribute.0;
        let accessor = &attribute.1;
        let view = accessor.view().unwrap();
        let vertex_count = accessor.count();

        // convert buffer to data
        let buffer_offset = view.offset();
        let buffer_length = view.length();
        let bytes: Vec<u8> = buffers[0].0[buffer_offset..(buffer_offset + buffer_length)].to_vec();

        if is_skeletal_mesh {
            if skeletal_vertex_data_list.is_empty() {
                skeletal_vertex_data_list.resize(vertex_count, SkeletalVertexData::default());
            }
        } else {
            if vertex_data_list.is_empty() {
                vertex_data_list.resize(vertex_count, VertexData::default());
            }
        }

        match semantic {
            gltf::Semantic::Positions => {
                assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
                assert_eq!(gltf::accessor::Dimensions::Vec3, accessor.dimensions());
                let mut buffer_data: Vec<Vector3<f32>> = system::convert_vec(bytes);
                for (i, data) in buffer_data.iter_mut().enumerate() {
                    if false == is_identity {
                        *data = math::apply_matrix_to_vector(parent_transform, &data, 1.0);
                    }

                    if is_skeletal_mesh {
                        skeletal_vertex_data_list[i]._position.clone_from(&data);
                    } else {
                        vertex_data_list[i]._position.clone_from(&data);
                    }
                }
            }
            gltf::Semantic::Normals => {
                assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
                assert_eq!(gltf::accessor::Dimensions::Vec3, accessor.dimensions());
                let mut buffer_data: Vec<Vector3<f32>> = system::convert_vec(bytes);
                for (i, data) in buffer_data.iter_mut().enumerate() {
                    if false == is_identity {
                        *data = math::apply_matrix_to_vector(parent_transform, &data, 0.0);
                        *data = math::safe_normalize(&data);
                    }

                    if is_skeletal_mesh {
                        skeletal_vertex_data_list[i]._normal.clone_from(&data);
                    } else {
                        vertex_data_list[i]._normal.clone_from(&data);
                    }
                }
            }
            gltf::Semantic::Tangents => {
                find_tangent_data = true;
                assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
                assert_eq!(gltf::accessor::Dimensions::Vec4, accessor.dimensions());
                let mut buffer_data: Vec<Vector4<f32>> = system::convert_vec(bytes);
                for (i, data) in buffer_data.iter_mut().enumerate() {
                    let mut data = Vector3::new(data.x, data.y, data.z);
                    if false == is_identity {
                        data = math::apply_matrix_to_vector(parent_transform, &data, 0.0);
                        data = math::safe_normalize(&data);
                    }

                    if is_skeletal_mesh {
                        skeletal_vertex_data_list[i]._tangent = data;
                    } else {
                        vertex_data_list[i]._tangent = data;
                    }
                }
            }
            gltf::Semantic::Colors(color_index) => {
                if 0 == *color_index {
                    if gltf::accessor::Dimensions::Vec3 != accessor.dimensions() && gltf::accessor::Dimensions::Vec4 != accessor.dimensions() {
                        panic!("unimplemented dimension: {:?}", accessor.dimensions());
                    }

                    if gltf::accessor::DataType::U16 == accessor.data_type() {
                        let buffer_data: Vec<Vector4<u16>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            let mut linear_color: Vector4<u32> = Vector4::new(255, 255, 255, 255);
                            if gltf::accessor::DataType::U16 == accessor.data_type() {
                                linear_color.x = (data.x >> 8) as u32;
                                linear_color.y = (data.y >> 8) as u32;
                                linear_color.z = (data.z >> 8) as u32;
                            }
                            if gltf::accessor::Dimensions::Vec4 == accessor.dimensions() {
                                linear_color.w = (data.w >> 8) as u32;
                            }

                            let color = vulkan_context::get_color32(linear_color.x, linear_color.y, linear_color.z, linear_color.w);
                            if is_skeletal_mesh {
                                skeletal_vertex_data_list[i]._color = color;
                            } else {
                                vertex_data_list[i]._color = color;
                            }
                        }
                    }
                    else if gltf::accessor::DataType::F32 == accessor.data_type() {
                        let buffer_data: Vec<Vector4<f32>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            let mut linear_color: Vector4<u32> = Vector4::new(255, 255, 255, 255);
                            if gltf::accessor::DataType::U16 == accessor.data_type() {
                                linear_color.x = (data.x * 255.0) as u32;
                                linear_color.y = (data.y * 255.0) as u32;
                                linear_color.z = (data.z * 255.0) as u32;
                            }
                            if gltf::accessor::Dimensions::Vec4 == accessor.dimensions() {
                                linear_color.w = (data.w * 255.0) as u32;
                            }

                            let color = vulkan_context::get_color32(linear_color.x, linear_color.y, linear_color.z, linear_color.w);
                            if is_skeletal_mesh {
                                skeletal_vertex_data_list[i]._color = color;
                            } else {
                                vertex_data_list[i]._color = color;
                            }
                        }
                    }
                    else {
                        panic!("unimplemented data type: {:?}", accessor.data_type());
                    }
                }
            }
            gltf::Semantic::TexCoords(uv_index) => {
                if 0 == *uv_index {
                    assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
                    assert_eq!(gltf::accessor::Dimensions::Vec2, accessor.dimensions());
                    let buffer_data: Vec<Vector2<f32>> = system::convert_vec(bytes);
                    for (i, data) in buffer_data.iter().enumerate() {
                        if is_skeletal_mesh {
                            skeletal_vertex_data_list[i]._texcoord.clone_from(&data);
                        } else {
                            vertex_data_list[i]._texcoord.clone_from(&data);
                        }
                    }
                }
            }
            gltf::Semantic::Joints(joint_index) => {
                if 0 == *joint_index {
                    assert!(is_skeletal_mesh);
                    assert_eq!(gltf::accessor::DataType::U8, accessor.data_type());
                    assert_eq!(gltf::accessor::Dimensions::Vec4, accessor.dimensions());
                    let buffer_data: Vec<Vector4<u8>> = system::convert_vec(bytes);
                    for (i, data) in buffer_data.iter().enumerate() {
                        skeletal_vertex_data_list[i]._bone_indices = Vector4::new(
                            data.x as u32,
                            data.y as u32,
                            data.z as u32,
                            data.w as u32,
                        );
                    }
                }
            }
            gltf::Semantic::Weights(weight_index) => {
                if 0 == *weight_index {
                    assert!(is_skeletal_mesh);
                    assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
                    assert_eq!(gltf::accessor::Dimensions::Vec4, accessor.dimensions());
                    let buffer_data: Vec<Vector4<f32>> = system::convert_vec(bytes);
                    for (i, data) in buffer_data.iter().enumerate() {
                        skeletal_vertex_data_list[i]._bone_weights.clone_from(data);
                    }
                }
            }
            _ => panic!("Unimplemented Semantic: {:?}", semantic),
        }
    }

    return (
        vertex_data_list,
        skeletal_vertex_data_list,
        find_tangent_data,
    );
}

pub fn compute_tangent(
    is_skeletal_mesh: bool,
    indices: &Vec<u32>,
    vertex_data_list: &mut Vec<VertexData>,
    skeletal_vertex_data_list: &mut Vec<SkeletalVertexData>,
) {
    let mut positions: Vec<Vector3<f32>> = Vec::new();
    let mut normals: Vec<Vector3<f32>> = Vec::new();
    let mut texcoords: Vec<Vector2<f32>> = Vec::new();
    let vertex_count = if is_skeletal_mesh {
        skeletal_vertex_data_list.len()
    } else {
        vertex_data_list.len()
    };
    positions.reserve(vertex_count);
    normals.reserve(vertex_count);
    texcoords.reserve(vertex_count);

    // gather vertex data_list
    for vertex_data_index in 0..vertex_count {
        if is_skeletal_mesh {
            positions.push(skeletal_vertex_data_list[vertex_data_index]._position.clone());
            normals.push(skeletal_vertex_data_list[vertex_data_index]._normal.clone());
            texcoords.push(skeletal_vertex_data_list[vertex_data_index]._texcoord.clone());
        } else {
            positions.push(vertex_data_list[vertex_data_index]._position.clone());
            normals.push(vertex_data_list[vertex_data_index]._normal.clone());
            texcoords.push(vertex_data_list[vertex_data_index]._texcoord.clone());
        }
    }

    // insert tangent
    let tangents: Vec<Vector3<f32>> =
        geometry_buffer::compute_tangent(&positions, &normals, &texcoords, &indices);
    for (vertex_data_index, tangent) in tangents.iter().enumerate() {
        if is_skeletal_mesh {
            skeletal_vertex_data_list[vertex_data_index]._tangent.clone_from(&tangent);
        } else {
            vertex_data_list[vertex_data_index]._tangent.clone_from(&tangent);
        }
    }
}

pub fn parsing_inverse_bind_matrices(
    skin: &gltf::Skin,
    buffers: &Vec<gltf::buffer::Data>,
) -> Vec<Matrix4<f32>> {
    let accessor = skin.inverse_bind_matrices().unwrap();
    let view = accessor.view().unwrap();
    log_assesor_view("\t\t\tInverse Bind Matrices", &accessor, &view);

    let buffer_offset = view.offset();
    let buffer_length = view.length();
    let bytes: Vec<u8> = buffers[0].0[buffer_offset..(buffer_offset + buffer_length)].to_vec();
    assert_eq!(gltf::accessor::DataType::F32, accessor.data_type());
    assert_eq!(gltf::accessor::Dimensions::Mat4, accessor.dimensions());
    let inverse_bind_matrices: Vec<Matrix4<f32>> = system::convert_vec(bytes);
    return inverse_bind_matrices;
}

pub fn parsing_bone_hierarchy(bone_node: &gltf::Node, hierarchy: &mut SkeletonHierarchyTree) {
    for child_node in bone_node.children() {
        assert!(child_node.mesh().is_none());
        let mut child_hierarchy = SkeletonHierarchyTree::default();
        parsing_bone_hierarchy(&child_node, &mut child_hierarchy);
        hierarchy._children.insert(child_node.name().unwrap().to_string(), child_hierarchy);
    }
}

pub fn parsing_skins(
    nodes: gltf::iter::Nodes,
    skin: &gltf::Skin,
    buffers: &Vec<gltf::buffer::Data>,
    mesh_data_create_info: &mut MeshDataCreateInfo,
) {
    for armature_node in nodes {
        if armature_node.name().unwrap() == skin.name().unwrap() {
            let m: &[[f32; 4]; 4] = &armature_node.transform().matrix();
            let mut parent_transform: Matrix4<f32> = Matrix4::new(
                m[0][0], m[1][0], m[2][0], m[3][0],
                m[0][1], m[1][1], m[2][1], m[3][1],
                m[0][2], m[1][2], m[2][2], m[3][2],
                m[0][3], m[1][3], m[2][3], m[3][3]
            );

            // inverse bind matrices
            let mut inverse_bind_matrices = parsing_inverse_bind_matrices(&skin, buffers);

            if constants::CONVERT_COORDINATE_SYSTEM_RIGHT_HANDED_TO_LEFT_HANDED {
                parent_transform.set_column(2, &(-parent_transform.column(2)));
                for inverse_bind_matrix in inverse_bind_matrices.iter_mut() {
                    inverse_bind_matrix.set_column(2, &(-inverse_bind_matrix.column(2)));
                }
            }

            // all bone names
            let mut bone_names: Vec<String> = Vec::new();
            for joint in skin.joints() {
                bone_names.push(joint.name().unwrap().to_string());
            }

            // build hierarchy
            let mut hierarchy = SkeletonHierarchyTree::default();
            for child_node in armature_node.children() {
                if child_node.mesh().is_none() {
                    let mut child_hierarchy = SkeletonHierarchyTree::default();
                    parsing_bone_hierarchy(&child_node, &mut child_hierarchy);
                    hierarchy
                        ._children
                        .insert(child_node.name().unwrap().to_string(), child_hierarchy);
                }
            }

            // skeletone create info
            mesh_data_create_info
                ._skeleton_create_infos
                .push(SkeletonDataCreateInfo {
                    _name: armature_node.name().unwrap().to_string(),
                    _transform: parent_transform,
                    _hierarchy: hierarchy,
                    _bone_names: bone_names,
                    _inv_bind_matrices: inverse_bind_matrices,
                });
            return;
        }
    }
    panic!("not found armature node");
}

pub fn parsing_animation(
    animations: gltf::iter::Animations,
    skin: &gltf::Skin,
    buffers: &Vec<gltf::buffer::Data>,
    mesh_data_create_info: &mut MeshDataCreateInfo,
) {
    let skeleton_create_info = &mesh_data_create_info._skeleton_create_infos.last().unwrap();
    let mut animation_node_data_list: Vec<AnimationNodeCreateInfo> = Vec::new();
    for bone_name in skeleton_create_info._bone_names.iter() {
        let mut animation_node_data = AnimationNodeCreateInfo {
            _name: format!("{}_{}", skin.name().unwrap(), bone_name),
            _target: bone_name.clone(),
            ..Default::default()
        };

        for animation in animations.clone() {
            if SHOW_GLTF_LOG {
                log::info!(
                    "\tAnimation: {:?}, Index: {:?}",
                    animation.name(),
                    animation.index()
                );
            }
            for channel in animation.channels() {
                let target = channel.target();
                let target_node = target.node();
                if bone_name == target_node.name().unwrap() {
                    let target_index = target_node.index();
                    let property = target.property();
                    let sampler = channel.sampler();
                    let input_accesor = sampler.input();
                    let input_view = input_accesor.view().unwrap();
                    let output_accesor = sampler.output();
                    let output_view = output_accesor.view().unwrap();
                    let _interpolation = sampler.interpolation();

                    if SHOW_GLTF_LOG {
                        log::info!(
                            "\tTarget {:?} Index: {:?}. Property: {:?}",
                            target_node.name().unwrap(),
                            target_index,
                            property
                        );
                        log::info!(
                            "        Sampler Interpolation: {:?}",
                            sampler.interpolation()
                        );
                        log_assesor_view(
                            "        Sampler Input",
                            &sampler.input(),
                            sampler.input().view().as_ref().unwrap(),
                        );
                        log_assesor_view(
                            "        Sampler Output",
                            &sampler.output(),
                            sampler.output().view().as_ref().unwrap(),
                        );
                    }

                    // input data
                    if animation_node_data._times.is_empty() {
                        let input_buffer_offset = input_view.offset();
                        let input_buffer_length = input_view.length();
                        let input_bytes: Vec<u8> = buffers[0].0
                            [input_buffer_offset..(input_buffer_offset + input_buffer_length)]
                            .to_vec();
                        assert_eq!(gltf::accessor::DataType::F32, input_accesor.data_type());
                        assert_eq!(
                            gltf::accessor::Dimensions::Scalar,
                            input_accesor.dimensions()
                        );
                        animation_node_data._times = system::convert_vec(input_bytes);
                    }

                    // output data
                    let output_buffer_offset = output_view.offset();
                    let output_buffer_length = output_view.length();
                    let output_bytes: Vec<u8> = buffers[0].0
                        [output_buffer_offset..(output_buffer_offset + output_buffer_length)]
                        .to_vec();
                    match property {
                        gltf::animation::Property::Translation => {
                            assert_eq!(gltf::accessor::DataType::F32, output_accesor.data_type());
                            assert_eq!(
                                gltf::accessor::Dimensions::Vec3,
                                output_accesor.dimensions()
                            );
                            animation_node_data._locations = system::convert_vec(output_bytes);
                        }
                        gltf::animation::Property::Rotation => {
                            assert_eq!(gltf::accessor::DataType::F32, output_accesor.data_type());
                            assert_eq!(
                                gltf::accessor::Dimensions::Vec4,
                                output_accesor.dimensions()
                            );
                            animation_node_data._rotations = system::convert_vec(output_bytes);
                        }
                        gltf::animation::Property::Scale => {
                            assert_eq!(gltf::accessor::DataType::F32, output_accesor.data_type());
                            assert_eq!(
                                gltf::accessor::Dimensions::Vec3,
                                output_accesor.dimensions()
                            );
                            animation_node_data._scales = system::convert_vec(output_bytes);
                        }
                        _ => panic!("not implementation!"),
                    }
                }
            }
        }
        let num_keyframes = animation_node_data._times.len();
        assert_eq!(num_keyframes, animation_node_data._locations.len());
        assert_eq!(num_keyframes, animation_node_data._rotations.len());
        assert_eq!(num_keyframes, animation_node_data._scales.len());
        animation_node_data_list.push(animation_node_data);
    }

    if 0 < animation_node_data_list.len() {
        let num_keyframes = animation_node_data_list[0]._times.len();
        for animation_node_data in animation_node_data_list.iter() {
            assert_eq!(num_keyframes, animation_node_data._times.len());
        }
    }
    mesh_data_create_info
        ._animation_node_create_infos
        .push(animation_node_data_list);
}

pub fn precompute_animation(
    frame_index: usize,
    _parent_transform: &Matrix4<f32>,
    bone_index: usize,
    hierarchy: &SkeletonHierarchyTree,
    bone_names: &Vec<String>,
    inv_bind_matrices: &Vec<Matrix4<f32>>,
    animation_node_data_list: &mut Vec<AnimationNodeCreateInfo>,
) {
    // precompute bone animation matrix with ancestor bone matrices
    let animation_node_data = &mut animation_node_data_list[bone_index];
    let transform: Matrix4<f32> = math::combinate_matrix(
        &animation_node_data._locations[frame_index],
        &math::quaternion_to_matrix(&animation_node_data._rotations[frame_index]),
        &animation_node_data._scales[frame_index],
    );

    let combined_transform = &transform * &inv_bind_matrices[bone_index];
    animation_node_data._locations[frame_index] = math::extract_location(&combined_transform);
    animation_node_data._rotations[frame_index] = math::extract_quaternion(&combined_transform);
    animation_node_data._scales[frame_index] = math::extract_scale(&combined_transform);

    for (child_bone_name, child_hierarchy) in hierarchy._children.iter() {
        let child_bone_index = bone_names.iter().position(|bone_name| bone_name == child_bone_name).unwrap();
        precompute_animation(
            frame_index,
            &transform,
            child_bone_index,
            &child_hierarchy,
            bone_names,
            inv_bind_matrices,
            animation_node_data_list,
        );
    }
}

pub fn parsing_meshes(
    node: &gltf::Node,
    buffers: &Vec<gltf::buffer::Data>,
    depth: i32,
    mesh_data_create_info: &mut MeshDataCreateInfo,
) {
    if SHOW_GLTF_LOG {
        let mut text: String = String::from("    ");
        for _i in 0..depth {
            text += "    ";
        }
        log::info!("{}Node {:?} Index: {:?}, Mesh: {:?}, Skin: {:?}, Weight: {:?}, Extras: {:?}, Transform: {:?}",
            text,
            node.name().unwrap(),
            node.index(),
            node.mesh().is_some(),
            node.skin().is_some(),
            node.weights().is_some(),
            node.extras(),
            node.transform()
        );
    }

    if node.mesh().is_some() {
        let is_skeletal_mesh: bool = node.skin().is_some();
        let mesh = node.mesh().unwrap();
        let m: &[[f32; 4]; 4] = &node.transform().matrix();
        let mut parent_transform: Matrix4<f32> = Matrix4::new(
            m[0][0], m[1][0], m[2][0], m[3][0],
            m[0][1], m[1][1], m[2][1], m[3][1],
            m[0][2], m[1][2], m[2][2], m[3][2],
            m[0][3], m[1][3], m[2][3], m[3][3]
        );

        if constants::CONVERT_COORDINATE_SYSTEM_RIGHT_HANDED_TO_LEFT_HANDED {
            parent_transform.set_column(2, &(-parent_transform.column(2)));
        }

        if SHOW_GLTF_LOG {
            log::info!(
                "\tMesh {:?}, Index: {:?}, Transform: {:?}",
                mesh.name(),
                mesh.index(),
                parent_transform
            );
        }
        for primitive in mesh.primitives() {
            if SHOW_GLTF_LOG {
                log::info!(
                    "\t\tPrimitive Index: {:?}, BoundBox: {:?}",
                    primitive.index(),
                    primitive.bounding_box()
                );
            }

            // bound box
            let bound_min = math::apply_matrix_to_vector(
                &parent_transform,
                &Vector3::new(
                    primitive.bounding_box().min[0],
                    primitive.bounding_box().min[1],
                    primitive.bounding_box().min[2],
                ),
                1.0,
            );
            let bound_max = math::apply_matrix_to_vector(
                &parent_transform,
                &Vector3::new(
                    primitive.bounding_box().max[0],
                    primitive.bounding_box().max[1],
                    primitive.bounding_box().max[2],
                ),
                1.0,
            );
            let bounding_box: BoundingBox = BoundingBox::create_bounding_box(&bound_min, &bound_max);

            // parsing vertex buffer
            let indices = parsing_index_buffer(&primitive, buffers);
            let (mut vertex_data_list, mut skeletal_vertex_data_list, find_tangent_data) =
                parsing_vertex_buffer(is_skeletal_mesh, &parent_transform, &primitive, buffers);

            // compute tangent normal
            if false == find_tangent_data {
                compute_tangent(
                    is_skeletal_mesh,
                    &indices,
                    &mut vertex_data_list,
                    &mut skeletal_vertex_data_list,
                );
            }

            // insert GeometryCreateInfo
            mesh_data_create_info
                ._geometry_create_infos
                .push(GeometryCreateInfo {
                    _vertex_data_list: vertex_data_list,
                    _skeletal_vertex_data_list: skeletal_vertex_data_list,
                    _indices: indices,
                    _bounding_box: bounding_box,
                });
        }
    }

    for child in node.children() {
        parsing_meshes(&child, buffers, depth + 1, mesh_data_create_info);
    }
}

impl GLTF {
    pub fn get_mesh_data_create_infos(filename: &PathBuf) -> MeshDataCreateInfo {
        //log::info!("GLTF: {:?}", filename);
        let (document, buffers, _images) = gltf::import(filename).unwrap();
        let mut mesh_data_create_info = MeshDataCreateInfo::default();
        for skin in document.skins() {
            parsing_skins(
                document.nodes(),
                &skin,
                &buffers,
                &mut mesh_data_create_info,
            );

            parsing_animation(
                document.animations(),
                &skin,
                &buffers,
                &mut mesh_data_create_info,
            );

            if constants::COMBINED_INVERSE_BIND_MATRIX {
                let n = mesh_data_create_info._animation_node_create_infos.len();
                let animation_node_data_list = &mut mesh_data_create_info._animation_node_create_infos[n - 1];
                let skeleton_create_info = &mesh_data_create_info._skeleton_create_infos.last().unwrap();
                let inv_bind_matrices = &skeleton_create_info._inv_bind_matrices;
                let bone_names = &skeleton_create_info._bone_names;
                let parent_transform: Matrix4<f32> = skeleton_create_info._transform.clone();
                for (child_bone_name, child_hierarchy) in skeleton_create_info._hierarchy._children.iter() {
                    let child_bone_index = bone_names.iter().position(|bone_name| bone_name == child_bone_name).unwrap();
                    let total_frame_count = animation_node_data_list[child_bone_index]._times.len();
                    for frame_index in 0..total_frame_count {
                        precompute_animation(
                            frame_index,
                            &parent_transform,
                            child_bone_index,
                            &child_hierarchy,
                            bone_names,
                            inv_bind_matrices,
                            animation_node_data_list,
                        );
                    }
                }
            }
        }

        for scene in document.scenes() {
            for node in scene.nodes() {
                parsing_meshes(&node, &buffers, 0, &mut mesh_data_create_info);
            }
        }

        mesh_data_create_info._bound_box = MeshDataCreateInfo::calc_mesh_bounding_box(
            &mesh_data_create_info._geometry_create_infos,
        );
        mesh_data_create_info
    }
}
