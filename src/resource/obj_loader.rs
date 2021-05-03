use std::collections::HashMap;
use std::path::PathBuf;
use std::io::Read;

use nalgebra::{
    self,
    Vector2,
    Vector3,
};

use crate::renderer::mesh::{ MeshDataCreateInfo };
use crate::vulkan_context::vulkan_context;
use crate::vulkan_context::geometry_buffer::{
    self,
    GeometryCreateInfo,
    StaticVertexData,
};
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system;

type Point3 = [u32; 3];

pub struct MeshObject {
    pub name: String,
    pub group_name: String,
    pub mtl_name: String,
    pub indices: Vec<(Point3, Point3, Point3)>,
}

pub struct WaveFrontOBJ {
    pub meshes: Vec<MeshObject>,
    pub positions: Vec<Vector3<f32>>,
    pub normals: Vec<Vector3<f32>>,
    pub texcoords: Vec<Vector2<f32>>,
    pub filename: PathBuf
}

impl WaveFrontOBJ {
    fn initialize(filename: &PathBuf) -> WaveFrontOBJ {
        WaveFrontOBJ {
            meshes: Vec::new(),
            positions: Vec::new(),
            normals: Vec::new(),
            texcoords: Vec::new(),
            filename: filename.clone(),
        }
    }

    fn parse(&mut self, filename: &PathBuf, scale: f32, invert_texcoord_y: bool) {
        // load OBJ file
        let default_name: String = String::from(filename.file_stem().unwrap().to_str().unwrap());
        let mut pre_fix: &str = "";
        let mut load_contents = system::load(filename);
        let mut contents: String = String::new();
        load_contents.read_to_string(&mut contents).expect("failed to parse");
        let lines = contents.lines();
        for content in lines {
            // is comment?
            if content.is_empty() || content.starts_with("#") {
                continue;
            }

            let values: Vec<&str> = content.split(" ").into_iter().filter(|x| { false == x.is_empty() }).collect();
            if values.len() < 2 {
                continue;
            }

            // start to paring a new mesh.
            let head: &str = values[0];
            let values: Vec<&str> = values[1..].to_vec();
            if self.meshes.is_empty() || ("f" == pre_fix && "f" != head && "s" != head) {
                self.meshes.push(MeshObject {
                    name: default_name.clone(),
                    group_name: String::new(),
                    mtl_name: String::new(),
                    indices: Vec::new(),
                });
            }
            let mesh_count: usize = self.meshes.len();
            let mut mesh_object: &mut MeshObject = &mut self.meshes[mesh_count - 1];

            // first strings
            pre_fix = head;

            if "o" == pre_fix {
                mesh_object.name = String::from(values.join(" "));
            } else if "g" == pre_fix {
                mesh_object.group_name = String::from(values.join(" "));
                if mesh_object.name.is_empty() {
                    mesh_object.name = mesh_object.group_name.clone();
                }
            } else if pre_fix == "mtllib" {
                // TODO : Parsing mtllib
            } else if "v" == pre_fix && 3 <= values.len() {
                // vertex position, apply scale
                self.positions.push(Vector3::new(
                    values[0].parse::<f32>().unwrap() * scale,
                    values[1].parse::<f32>().unwrap() * scale,
                    values[2].parse::<f32>().unwrap() * scale,
                ));
            } else if "vn" == pre_fix && 3 <= values.len() {
                // vertex normal
                self.normals.push(Vector3::new(
                    values[0].parse::<f32>().unwrap(),
                    values[1].parse::<f32>().unwrap(),
                    values[2].parse::<f32>().unwrap(),
                ));
            } else if "vt" == pre_fix && 2 <= values.len() {
                // texture coordinate
                let texcoord_y = values[1].parse::<f32>().unwrap();
                self.texcoords.push(Vector2::new(
                    values[0].parse::<f32>().unwrap(),
                    if invert_texcoord_y { 1.0 - texcoord_y } else { texcoord_y },
                ));
            } else if "usemtl" == pre_fix || "usemat" == pre_fix {
                // material name
                mesh_object.mtl_name = String::from(values.join(" "));
                if mesh_object.name.is_empty() {
                    mesh_object.name = mesh_object.mtl_name.clone();
                }
            } else if pre_fix == "f" {
                // faces
                let mut pos_indices: Vec<u32> = Vec::new();
                let mut normal_indices: Vec<u32> = Vec::new();
                let mut tex_indices: Vec<u32> = Vec::new();

                // parsing index data
                for indices in values.iter() {
                    let indices: Vec<u32> = indices
                        .split("/")
                        .into_iter()
                        .map(|x| if x.is_empty() { 0 } else { x.trim().parse::<u32>().unwrap() - 1 })
                        .collect();
                    // insert vertex, texcoord, normal index
                    let len_index = indices.len();
                    pos_indices.push(indices[0]);

                    if 1 < len_index {
                        tex_indices.push(indices[1]);
                    } else {
                        tex_indices.push(0);
                    }

                    if 2 < len_index {
                        normal_indices.push(indices[2]);
                    } else {
                        normal_indices.push(0);
                    }
                }

                // push face list
                if 3 == pos_indices.len() {
                    mesh_object.indices.push((
                        [pos_indices[0], pos_indices[1], pos_indices[2]],
                        [normal_indices[0], normal_indices[1], normal_indices[2]],
                        [tex_indices[0], tex_indices[1], tex_indices[2]],
                    ));
                }
                // Quad to Two Triangles.
                else if 4 == pos_indices.len() {
                    mesh_object.indices.push((
                        [pos_indices[0], pos_indices[1], pos_indices[2]],
                        [normal_indices[0], normal_indices[1], normal_indices[2]],
                        [tex_indices[0], tex_indices[1], tex_indices[2]],
                    ));
                    mesh_object.indices.push((
                        [pos_indices[2], pos_indices[3], pos_indices[0]],
                        [normal_indices[2], normal_indices[3], normal_indices[0]],
                        [tex_indices[2], tex_indices[3], tex_indices[0]],
                    ));
                }
            }
        }
    }

    fn generate_geometry_datas(&mut self) -> Vec<GeometryCreateInfo> {
        // If texcoord is empty, add the default texcoord.
        if self.texcoords.len() < 1 {
            self.texcoords.push(Vector2::new(0.0, 0.0));
        }
        // If normal is empty, add the default normal.
        if self.normals.len() < 1 {
            self.normals.push(Vector3::new(0.0, 0.0, 0.0));
        }

        let mut geometry_datas: Vec<GeometryCreateInfo> = Vec::new();
        for mesh in self.meshes.iter() {
            let mut positions: Vec<Vector3<f32>> = Vec::new();
            let mut normals: Vec<Vector3<f32>> = Vec::new();
            let mut texcoords: Vec<Vector2<f32>> = Vec::new();
            let mut indices: Vec<u32> = Vec::new();
            let mut index_map: HashMap<(u32, u32, u32), u32> = HashMap::new();

            let mut bound_min: Vector3<f32> = Vector3::new(std::f32::MAX, std::f32::MAX, std::f32::MAX);
            let mut bound_max: Vector3<f32> = Vector3::new(std::f32::MIN, std::f32::MIN, std::f32::MIN);
            for mesh_indices in mesh.indices.iter() {
                // exclude material
                let (postion_indices, normal_indices, texcoord_indices) = mesh_indices;
                for i in 0..postion_indices.len() {
                    let index_key = (postion_indices[i], normal_indices[i], texcoord_indices[i]);
                    match index_map.get(&index_key) {
                        Some(index) => indices.push(*index),
                        None => {
                            let index: u32 = index_map.len() as u32;
                            indices.push(index);
                            index_map.insert(index_key, index);
                            positions.push(self.positions[postion_indices[i] as usize].clone() as Vector3<f32>);
                            normals.push(self.normals[normal_indices[i] as usize].clone() as Vector3<f32>);
                            texcoords.push(self.texcoords[texcoord_indices[i] as usize].clone() as Vector2<f32>);
                            // bounding box
                            let position: &Vector3<f32> = positions.last().unwrap();
                            for j in 0..3 {
                                if bound_min[j] > position[j] {
                                    bound_min[j] = position[j];
                                }
                                if bound_max[j] < position[j] {
                                    bound_max[j] = position[j];
                                }
                            }
                        }
                    }
                }
            }

            if 0 == positions.len() {
                log::error!("%s has a empty mesh. {} {}", self.filename.to_str().unwrap(), mesh.name);
                continue;
            }

            let tangents = geometry_buffer::compute_tangent(&positions, &normals, &texcoords, &indices);
            let vertex_color = vulkan_context::get_color32(255, 255, 255, 255);
            let vertex_datas: Vec<StaticVertexData> = positions
                .iter()
                .enumerate()
                .map(|(index, position)| {
                    StaticVertexData {
                        _position: position.clone() as Vector3<f32>,
                        _normal: normals[index].clone() as Vector3<f32>,
                        _tangent: tangents[index].clone() as Vector3<f32>,
                        _color: vertex_color,
                        _texcoord: texcoords[index].clone() as Vector2<f32>,
                    }
                }).collect();

            geometry_datas.push(GeometryCreateInfo {
                _vertex_datas: vertex_datas,
                _indices: indices,
                _bounding_box: BoundingBox {
                    _min: bound_min,
                    _max: bound_max,
                    _center: &bound_max * 0.5 + &bound_min * 0.5,
                    _size: &bound_max - &bound_min,
                    _radius: (&bound_max * 0.5 - &bound_min * 0.5).norm()
                },
                ..Default::default()
            });
        }
        geometry_datas
    }

    // use tobj library
    // fn parse_using_library(&mut self, filename: &PathBuf) {
    //     use std::io::Cursor;
    //     use std::fs::File;
    //     use std::io::Read;
    //
    //     let mut buf = Vec::new();
    //     let mut file = File::open(filename).unwrap();
    //     file.read_to_end(&mut buf).unwrap();
    //     let mut cursor = Cursor::new(buf);
    //     let (models, _) = tobj::load_obj_buf(&mut cursor, true, |_| {
    //         Ok((vec![], std::collections::HashMap::new()))
    //     }).unwrap();
    //
    //     let mut index_offset: u32 = 0;
    //     for model in models.iter() {
    //         let mesh = &model.mesh;
    //         let mut mesh_object = MeshObject {
    //             name: model.name.clone(),
    //             group_name: String::from("default"),
    //             mtl_name: String::from("default"),
    //             indices: Vec::new(),
    //         };
    //
    //         let positions = mesh.positions.as_slice();
    //         let coords = mesh.texcoords.as_slice();
    //         let normals = mesh.normals.as_slice();
    //         let vertex_count = mesh.positions.len() / 3;
    //
    //         for index in (0..mesh.indices.len()).step_by(3) {
    //             let point = [mesh.indices[index] + index_offset, mesh.indices[index+1] + index_offset, mesh.indices[index+2] + index_offset];
    //             mesh_object.indices.push((
    //                 point.clone(),
    //                 if normals.is_empty() { [0,0,0] } else { point.clone() },
    //                 point.clone())
    //             );
    //         }
    //         index_offset += mesh.positions.len() as u32;
    //
    //         for i in 0..vertex_count {
    //             let x = positions[i * 3];
    //             let y = positions[i * 3 + 1];
    //             let z = positions[i * 3 + 2];
    //             let u = coords[i * 2];
    //             let v = coords[i * 2 + 1];
    //             self.positions.push(Vector3::new(x,y,z));
    //             self.texcoords.push(Vector2::new(u,v));
    //             if false == normals.is_empty() {
    //                 self.normals.push(
    //                     Vector3::new(
    //                         normals[i * 3],
    //                         normals[i * 3 + 1],
    //                         normals[i * 3 + 2]
    //                     )
    //                 );
    //             }
    //         }
    //         self.meshes.push(mesh_object);
    //     }
    // }

    pub fn get_mesh_data_create_infos(filename: &PathBuf) -> MeshDataCreateInfo {
        let mut obj = WaveFrontOBJ::initialize(filename);
        //obj.parse_using_library(filename);
        let texcoord_y = true;
        obj.parse(filename, 1.0, texcoord_y);
        let geometry_create_infos = obj.generate_geometry_datas();

        MeshDataCreateInfo::create_mesh_data_crate_info(MeshDataCreateInfo {
            _geometry_create_infos: geometry_create_infos,
            ..Default::default()
        })
    }
}