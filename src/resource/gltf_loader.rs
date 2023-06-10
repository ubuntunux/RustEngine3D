use std::cmp::min;
use std::collections::HashMap;
use std::ops::Add;
use std::path::PathBuf;
use std::{fs, io};
use std::boxed::Box;
use std::error::Error as StdError;
use byteorder::WriteBytesExt;

use gltf;
use gltf::scene::Transform;
use nalgebra::{
    self,
    Vector2,
    Vector3,
    Vector4,
    Matrix4,
};
use nalgebra_glm as glm;

use crate::renderer::mesh::{ MeshDataCreateInfo };
use crate::renderer::animation::{ AnimationNodeCreateInfo, SkeletonHierachyTree, SkeletonDataCreateInfo };
use crate::utilities::bounding_box::{ self, BoundingBox, };
use crate::utilities::math;
use crate::utilities::system;
use crate::utilities::xml::{ self, XmlTree, };
use crate::vulkan_context::vulkan_context;
use crate::vulkan_context::geometry_buffer::{ self, GeometryCreateInfo, VertexData, SkeletalVertexData };
use crate::constants;
use crate::renderer::push_constants::PushConstantSize;

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
    pub filename: PathBuf
}

pub fn display_gltf(node: &gltf::Node, buffers: &Vec<gltf::buffer::Data>, depth: i32, mesh_data_create_info: &mut MeshDataCreateInfo) {
    // let mut text: String = String::from("    ");
    // for i in 0..depth {
    //     text += "    ";
    // }
    // text += node.name().unwrap();
    // log::info!("\tNode {:?}, Index: {:?}, Mesh: {:?}, Skin: {:?}, Weight: {:?}, Extras: {:?}, Transform: {:?}",
    //     text,
    //     node.index(),
    //     node.mesh().is_some(),
    //     node.skin().is_some(),
    //     node.weights().is_some(),
    //     node.extras(),
    //     node.transform()
    // );

    if node.mesh().is_some() {
        let mesh = node.mesh().unwrap();
        //log::info!("\tMesh Index: {:?}", mesh.index());
        for primitive in mesh.primitives() {
            //log::info!("\t\tPrimitive Index: {:?}, BoundBox: {:?}", primitive.index(), primitive.bounding_box());
            let bounding_box: BoundingBox = BoundingBox::create_bounding_box(
                &Vector3::new (primitive.bounding_box().min[0], primitive.bounding_box().min[1], primitive.bounding_box().min[2]),
                &Vector3::new (primitive.bounding_box().max[0], primitive.bounding_box().max[1], primitive.bounding_box().max[2])
            );

            let indices_accesor = primitive.indices().unwrap();
            let indices_view = indices_accesor.view().unwrap();
            //log::info!("\t\t\tIndices Type: {:?}, Index: {:?}, Dimensions: {:?}, Stride: {:?}, Count: {:?}, ByteLength: {:?}, Offset: {:?}, Target: {:?}",
            //     indices_accesor.data_type(),
            //     indices_accesor.index(),
            //     indices_accesor.dimensions(),
            //     indices_accesor.size(),
            //     indices_accesor.count(),
            //     indices_view.length(),
            //     indices_view.offset(),
            //     indices_view.target()
            // );

            // convert buffer to data
            let index_buffer_offset = indices_view.offset();
            let index_buffer_length = indices_view.length();
            let index_bytes: Vec<u8> = buffers[0].0[index_buffer_offset..(index_buffer_offset + index_buffer_length)].to_vec();
            assert_eq!(gltf::accessor::DataType::U16, indices_accesor.data_type());
            let index_count = index_bytes.len() / 2;
            let mut indices: Vec<u32> = vec![0; index_count];
            for i in 0..index_count {
                indices[i] = (index_bytes[i * 2] as u32) | ((index_bytes[i * 2 + 1] as u32) << 8);
            }

            let is_skeletal_mesh: bool = false;
            let mut vertex_datas: Vec<VertexData> = Vec::new();
            let mut skeletal_vertex_datas: Vec<SkeletalVertexData> = Vec::new();
            for attribute in primitive.attributes() {
                let semantic = &attribute.0;
                let accesor = &attribute.1;
                let view = accesor.view().unwrap();
                let vertex_count = accesor.count();

                //log::info!("\t\t\tAttribute {:?}, Type: {:?}, Index: {:?}, Dimensions: {:?}, Stride: {:?}, Count: {:?}, ByteLength: {:?}, Offset: {:?}, Target: {:?}",
                //     semantic,
                //     accesor.data_type(),
                //     accesor.index(),
                //     accesor.dimensions(),
                //     accesor.size(),
                //     accesor.count(),
                //     view.length(),
                //     view.offset(),
                //     view.target()
                // );

                // convert buffer to data
                let buffer_offset = view.offset();
                let buffer_length = view.length();
                let bytes: Vec<u8> = buffers[0].0[buffer_offset..(buffer_offset + buffer_length)].to_vec();

                if is_skeletal_mesh {
                    if skeletal_vertex_datas.is_empty() {
                        skeletal_vertex_datas.resize(vertex_count, SkeletalVertexData::default());
                    }
                } else {
                    if vertex_datas.is_empty() {
                        vertex_datas.resize(vertex_count, VertexData::default());
                    }
                }

                match semantic {
                    gltf::Semantic::Positions => {
                        assert_eq!(gltf::accessor::DataType::F32, accesor.data_type());
                        assert_eq!(gltf::accessor::Dimensions::Vec3, accesor.dimensions());

                        let m: &[[f32; 4]; 4] = &node.transform().matrix();
                        let transform: Matrix4<f32> = Matrix4::new(
                            m[0][0], m[1][0], m[2][0], m[3][0],
                            m[0][1], m[1][1], m[2][1], m[3][1],
                            m[0][2], m[1][2], m[2][2], m[3][2],
                            m[0][3], m[1][3], m[2][3], m[3][3]
                        );
                        let is_identity: bool = transform == Matrix4::identity();

                        let mut buffer_data: Vec<Vector3<f32>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter_mut().enumerate() {
                            if false == is_identity {
                                let mut position: Vector4<f32> = Vector4::new(data.x, data.y, data.z, 1.0);
                                position = &transform * &position;
                                data.x = position.x;
                                data.y = position.y;
                                data.z = position.z;
                            }

                            if is_skeletal_mesh {
                                skeletal_vertex_datas[i]._position.clone_from(&data);
                            } else {
                                vertex_datas[i]._position.clone_from(&data);
                            }
                        }
                    },
                    gltf::Semantic::Normals => {
                        assert_eq!(gltf::accessor::DataType::F32, accesor.data_type());
                        assert_eq!(gltf::accessor::Dimensions::Vec3, accesor.dimensions());
                        let buffer_data: Vec<Vector3<f32>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            if is_skeletal_mesh {
                                skeletal_vertex_datas[i]._normal.clone_from(&data);
                            } else {
                                vertex_datas[i]._normal.clone_from(&data);
                            }
                        }
                    },
                    gltf::Semantic::Tangents => {
                        assert_eq!(gltf::accessor::DataType::F32, accesor.data_type());
                        assert_eq!(gltf::accessor::Dimensions::Vec4, accesor.dimensions());
                        let buffer_data: Vec<Vector4<f32>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            if is_skeletal_mesh {
                                skeletal_vertex_datas[i]._tangent = Vector3::new(data.x, data.y, data.z);
                            } else {
                                vertex_datas[i]._tangent = Vector3::new(data.x, data.y, data.z);
                            }
                        }
                    },
                    gltf::Semantic::Colors(_color_index) => {
                        assert_eq!(gltf::accessor::DataType::U16, accesor.data_type());
                        assert_eq!(gltf::accessor::Dimensions::Vec4, accesor.dimensions());
                        let buffer_data: Vec<Vector4<u16>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            // U16 -> U8
                            let color = vulkan_context::get_color32(
                                (data.x >> 8) as u32,
                                (data.y >> 8) as u32,
                                (data.z >> 8) as u32,
                                (data.w >> 8) as u32
                            );
                            if is_skeletal_mesh {
                                skeletal_vertex_datas[i]._color = color;
                            } else {
                                vertex_datas[i]._color = color;
                            }
                        }
                    },
                    gltf::Semantic::TexCoords(_uv_index) => {
                        assert_eq!(gltf::accessor::DataType::F32, accesor.data_type());
                        assert_eq!(gltf::accessor::Dimensions::Vec2, accesor.dimensions());
                        let buffer_data: Vec<Vector2<f32>> = system::convert_vec(bytes);
                        for (i, data) in buffer_data.iter().enumerate() {
                            if is_skeletal_mesh {
                                skeletal_vertex_datas[i]._texcoord.clone_from(&data);
                            } else {
                                vertex_datas[i]._texcoord.clone_from(&data);
                            }
                        }
                    },
                    _ => panic!("Unimplemented Semantic: {:?}", semantic)
                }
            }

            // insert GeometryCreateInfo
            mesh_data_create_info._geometry_create_infos.push(
                GeometryCreateInfo {
                    _vertex_datas: vertex_datas,
                    _skeletal_vertex_datas: skeletal_vertex_datas,
                    _indices: indices,
                    _bounding_box: bounding_box
                }
            );
        }
    }

    for child in node.children() {
        display_gltf(&child, buffers, depth + 1, mesh_data_create_info);
    }
}

impl GLTF {
    pub fn get_mesh_data_create_infos(filename: &PathBuf) -> MeshDataCreateInfo {
        let mut mesh_data_create_info = MeshDataCreateInfo::default();

        //log::info!("GLTF: {:?}", filename);
        let (document, buffers, images) = gltf::import(filename).unwrap();
        for scene in document.scenes() {
            for node in scene.nodes() {
                display_gltf(&node, &buffers, 0, &mut mesh_data_create_info);
            }
        }

        for (i, buffer) in document.buffers().enumerate() {
            //log::info!("\tBuffer Index: {:?}, Length: {:?}, Buffer Length: {:?}", buffer.index(), buffer.length(), buffers[i].0.len());
        }

        for (i, image) in document.images().enumerate() {
            //log::info!("\tImage Index: {:?}", image.index());
        }

        // pub struct VertexData {
        //     pub _position: Vector3<f32>,
        //     pub _normal: Vector3<f32>,
        //     pub _tangent: Vector3<f32>,
        //     pub _color: u32,
        //     pub _texcoord: Vector2<f32>
        // }
        //
        // pub struct GeometryCreateInfo {
        //     pub _vertex_datas: Vec<VertexData>,
        //     pub _skeletal_vertex_datas: Vec<SkeletalVertexData>,
        //     pub _indices: Vec<u32>,
        //     pub _bounding_box: BoundingBox,
        // }
        //
        // pub struct MeshDataCreateInfo {
        //     pub _bound_box: BoundingBox,
        //     pub _skeleton_create_infos: Vec<SkeletonDataCreateInfo>,
        //     pub _animation_node_create_infos: Vec<Vec<AnimationNodeCreateInfo>>,
        //     pub _geometry_create_infos: Vec<GeometryCreateInfo>,
        // }

        mesh_data_create_info._bound_box = MeshDataCreateInfo::calc_mesh_bounding_box(&mesh_data_create_info._geometry_create_infos);
        mesh_data_create_info
    }
}