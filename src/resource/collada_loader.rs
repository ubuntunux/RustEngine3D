#![allow(clippy::unnecessary_cast)]

use std::cmp::min;
use std::collections::HashMap;
use std::ops::Add;
use std::path::PathBuf;

use nalgebra::{self, Matrix4, Vector2, Vector3, Vector4};
use nalgebra_glm as glm;

use crate::constants;
use crate::scene::animation::{
    AnimationNodeCreateInfo, SkeletonDataCreateInfo, SkeletonHierarchyTree,
};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::mesh::MeshDataCreateInfo;
use crate::utilities::math;
use crate::utilities::xml::{self, XmlTree};
use crate::vulkan_context::geometry_buffer::{
    self, GeometryCreateInfo, SkeletalVertexData, VertexData,
};
use crate::vulkan_context::vulkan_context;

#[derive(Debug, Clone)]
pub struct Collada {
    pub _name: String,
    pub _collada_version: String,
    pub _author: String,
    pub _authoring_tool: String,
    pub _created: String,
    pub _modified: String,
    pub _unit_name: String,
    pub _unit_meter: f32,
    pub _up_axis: String,
    pub _nodes: Vec<ColladaNode>,
    pub _node_name_map: HashMap<String, String>,
    pub _geometries: Vec<ColladaGeometry>,
    pub _controllers: Vec<ColladaController>,
    pub _animations: Vec<ColladaAnimation>,
}

#[derive(Debug, Clone)]
pub struct ColladaNode {
    pub _valid: bool,
    pub _name: String,
    pub _id: String,
    pub _type: String,
    pub _matrix: Matrix4<f32>,
    pub _parent: *const ColladaNode,
    pub _children: Vec<ColladaNode>,
    pub _instance_controller: String,
    pub _instance_geometry: String,
}

#[derive(Debug, Clone)]
pub struct ColladaController {
    pub _valid: bool,
    pub _name: String,
    pub _id: String,
    pub _skin_source: String,
    pub _bind_shape_matrix: Matrix4<f32>,
    pub _bone_names: Vec<String>,
    pub _bone_indices: Vec<Vector4<u32>>,
    pub _bone_weights: Vec<Vector4<f32>>,
    pub _inv_bind_matrices: Vec<Matrix4<f32>>,
}

#[derive(Debug, Clone)]
pub struct SemanticInfo {
    pub _source: String,
    pub _offset: usize,
    pub _set_number: String,
}

#[derive(Debug, Clone)]
pub struct ColladaAnimation {
    pub _valid: bool,
    pub _id: String,
    pub _target: String,
    pub _type: String,
    pub _inputs: Vec<f32>,
    pub _outputs: Vec<Matrix4<f32>>,
    pub _interpolations: Vec<String>,
    pub _in_tangents: Vec<Vec<f32>>,
    pub _out_tangents: Vec<Vec<f32>>,
}

#[derive(Debug, Clone)]
pub struct ColladaGeometry {
    pub _valid: bool,
    pub _name: String,
    pub _id: String,
    pub _positions: Vec<Vector3<f32>>,
    pub _bone_indices: Vec<Vector4<u32>>,
    pub _bone_weights: Vec<Vector4<f32>>,
    pub _normals: Vec<Vector3<f32>>,
    pub _colors: Vec<u32>,
    pub _texcoords: Vec<Vector2<f32>>,
    pub _indices: Vec<u32>,
    pub _bind_shape_matrix: Matrix4<f32>,
    pub _controller: *const ColladaController,
}

#[derive(Debug, Clone)]
pub enum ColladaSourceData {
    FloatArray(Vec<f32>),
    VectorArray(Vec<Vec<f32>>),
    NameArray(Vec<String>),
}

pub fn parse_value<T: std::str::FromStr>(data: &str, default_value: T) -> T {
    data.parse::<T>().unwrap_or_else(|_| default_value)
}

pub fn parse_list<T: std::str::FromStr + std::fmt::Debug>(data_list: &str) -> Vec<T> {
    let data_list = data_list.replace("[", "").replace("]", "");
    let data_list = data_list
        .trim()
        .split(" ")
        .map(|data| data.trim().parse::<T>());

    let mut values: Vec<T> = Vec::new();
    for data in data_list {
        match data {
            Ok(value) => values.push(value),
            Err(_) => {}
        }
    }
    values
}

pub fn parse_vector_list<T: std::str::FromStr + Clone>(
    data_list: &str,
    component_count: usize,
) -> Vec<Vec<T>> {
    let data_list: Vec<Result<T, T::Err>> = data_list
        .replace("[", "")
        .replace("]", "")
        .trim()
        .split(" ")
        .map(|data| data.trim().parse::<T>())
        .collect();
    let mut values: Vec<Vec<T>> = Vec::new();
    let len = data_list.len() / component_count;
    for i in 0..len {
        let mut components: Vec<T> = Vec::new();
        for j in 0..component_count {
            match &data_list[i * component_count + j] {
                Ok(value) => components.push(value.clone()),
                Err(_) => {}
            }
        }
        values.push(components);
    }
    values
}

pub fn parsing_source_data(xml_element: &XmlTree) -> HashMap<String, ColladaSourceData> {
    let mut sources: HashMap<String, ColladaSourceData> = HashMap::new();
    let source_elements = xml_element.get_elements("source");
    if source_elements.is_some() {
        for xml_source in source_elements.unwrap().iter() {
            let source_id: &String = &xml_source.attributes.get("id").unwrap();
            let accessor_element = xml_source.get_element("technique_common/accessor").unwrap();
            let stride =
                parse_value::<usize>(&accessor_element.attributes.get("stride").unwrap(), 0);
            let float_array_elements = xml_source.get_elements("float_array");
            if float_array_elements.is_some() {
                let source_text = &float_array_elements.unwrap()[0].text;
                if false == source_text.is_empty() {
                    if 1 < stride {
                        let source_data = ColladaSourceData::VectorArray(parse_vector_list::<f32>(
                            source_text.as_str(),
                            stride,
                        ));
                        sources.insert(source_id.clone(), source_data);
                    } else {
                        let source_data =
                            ColladaSourceData::FloatArray(parse_list::<f32>(source_text.as_str()));
                        sources.insert(source_id.clone(), source_data);
                    }
                }
            }

            let name_array_elements = xml_source.get_elements("Name_array");
            if name_array_elements.is_some() {
                let source_text = &name_array_elements.unwrap()[0].text;
                if false == source_text.is_empty() {
                    let source_data =
                        ColladaSourceData::NameArray(parse_list::<String>(source_text.as_str()));
                    sources.insert(source_id.clone(), source_data);
                }
            }
        }
    }
    sources
}

//     :param xml_element:
//     :return: {'semantic':{'source', 'offset', 'set'}
pub fn parsing_semantic(xml_element: &XmlTree) -> HashMap<String, SemanticInfo> {
    let mut semantics: HashMap<String, SemanticInfo> = HashMap::new();
    let input_elements = xml_element.get_elements("input");
    if input_elements.is_some() {
        for xml_semantic in input_elements.unwrap().iter() {
            let set_number: &str = match xml_semantic.attributes.get("set") {
                None => "0",
                Some(set_number) => set_number.as_str(),
            };

            let semantic: String = match xml_semantic.attributes.get("semantic") {
                Some(semantic) => {
                    let mut semantic: String = semantic.clone();
                    if false == set_number.is_empty() && "0" != set_number {
                        semantic = semantic.add(set_number); // ex) VERTEX0, TEXCOORD0
                    }
                    semantic
                }
                None => String::from(""),
            };

            let source: &str = match xml_semantic.attributes.get("source") {
                Some(source) => {
                    if source.starts_with("#") {
                        source.get(1..).unwrap()
                    } else {
                        source.as_str()
                    }
                }
                None => "",
            };

            let offset: usize = match xml_semantic.attributes.get("offset") {
                Some(offset) => parse_value(offset, 0),
                None => 0,
            };

            semantics.insert(
                semantic,
                SemanticInfo {
                    _source: String::from(source),
                    _offset: offset,
                    _set_number: String::from(set_number),
                },
            );
        }
    }
    semantics
}

impl ColladaSourceData {
    pub fn is_empty(&self) -> bool {
        match self {
            ColladaSourceData::FloatArray(xs) => xs.is_empty(),
            ColladaSourceData::VectorArray(xs) => xs.is_empty(),
            ColladaSourceData::NameArray(xs) => xs.is_empty(),
        }
    }

    pub fn get_float_array(&self) -> Vec<f32> {
        if self.is_empty() {
            return Vec::new();
        }

        match self {
            ColladaSourceData::FloatArray(xs) => xs.clone(),
            _ => panic!("get_float_array error."),
        }
    }

    pub fn get_vector_array(&self) -> Vec<Vec<f32>> {
        if self.is_empty() {
            return Vec::new();
        }

        match self {
            ColladaSourceData::VectorArray(xs) => xs.clone(),
            _ => panic!("get_vector_array error."),
        }
    }

    pub fn get_name_array(&self) -> Vec<String> {
        if self.is_empty() {
            return Vec::new();
        }

        match self {
            ColladaSourceData::NameArray(xs) => xs.clone(),
            _ => panic!("get_name_array error."),
        }
    }
}

impl ColladaNode {
    pub fn create_collada_node(
        xml_node: &XmlTree,
        parent: *const ColladaNode,
        depth: usize,
    ) -> ColladaNode {
        let mut collada_node = ColladaNode {
            _valid: false,
            _name: xml_node.attributes.get("name").unwrap().replace(".", "_"),
            _id: xml_node.attributes.get("id").unwrap().replace(".", "_"),
            _type: xml_node.attributes.get("type").unwrap().clone(),
            _matrix: Matrix4::identity(),
            _parent: parent,
            _children: Vec::new(),
            _instance_controller: String::new(),
            _instance_geometry: String::new(),
        };

        let instance_controller = xml_node.get_elements("instance_controller");
        if instance_controller.is_some() {
            let url = instance_controller.unwrap()[0].attributes.get("url");
            if url.is_some() {
                let url = url.unwrap();
                if url.starts_with("#") {
                    collada_node._instance_controller = String::from(url.get(1..).unwrap());
                }
            }
        }

        let instance_geometry = xml_node.get_elements("instance_geometry");
        if instance_geometry.is_some() {
            let url = instance_geometry.unwrap()[0].attributes.get("url");
            if url.is_some() {
                let url = url.unwrap();
                if url.starts_with("#") {
                    collada_node._instance_geometry = String::from(url.get(1..).unwrap());
                }
            }
        }

        collada_node.parsing_matrix(xml_node);

        let xml_nodes = xml_node.get_elements("node");
        if xml_nodes.is_some() {
            for xml_child_node in xml_nodes.unwrap().iter() {
                let child =
                    ColladaNode::create_collada_node(xml_child_node, &collada_node, depth + 1);
                collada_node._children.push(child);
            }
        }
        collada_node
    }

    pub fn parsing_matrix(&mut self, xml_node: &XmlTree) {
        let xml_matrix = xml_node.get_elements("matrix");
        if xml_matrix.is_some() {
            // transform matrix
            let matrix: Vec<f32> = xml_matrix.unwrap()[0]
                .text
                .split(" ")
                .map(|x| parse_value::<f32>(x, 0.0))
                .collect();
            if 16 == matrix.len() {
                self._matrix.copy_from_slice(&matrix);
            } else {
                panic!("parsing matrix error.");
            }
        } else {
            // location, rotation, scale
            let xml_translate = xml_node.get_elements("translate");
            if xml_translate.is_some() {
                let translation: Vec<f32> = xml_translate.unwrap()[0]
                    .text
                    .split(" ")
                    .map(|x| parse_value::<f32>(x, 0.0))
                    .collect();
                if 3 == translation.len() {
                    math::translate_matrix(
                        &mut self._matrix,
                        translation[0],
                        translation[1],
                        translation[2],
                    );
                } else {
                    panic!("{} node has a invalid translate.", self._name);
                }
            }

            let xml_rotates = xml_node.get_elements("rotate");
            if xml_rotates.is_some() {
                for xml_rotate in xml_rotates.unwrap().iter() {
                    let rotation: Vec<f32> = xml_rotate
                        .text
                        .split(" ")
                        .map(|x| parse_value::<f32>(x, 0.0))
                        .collect();
                    if 4 == rotation.len() {
                        let axis = xml_rotate.attributes.get("sid").unwrap();
                        match axis.as_str() {
                            // rotation_axis : rotation[0], rotation[1], rotation[2], radian : rotation[3]
                            "rotationX" => math::matrix_rotate_x(&mut self._matrix, rotation[3]),
                            "rotationY" => math::matrix_rotate_y(&mut self._matrix, rotation[3]),
                            "rotationZ" => math::matrix_rotate_z(&mut self._matrix, rotation[3]),
                            _ => panic!("{} node has a invalid rotate.", self._name),
                        }
                    }
                }
            }

            let xml_scale = xml_node.get_elements("scale");
            if xml_scale.is_some() {
                let scale: Vec<f32> = xml_scale.unwrap()[0]
                    .text
                    .split(" ")
                    .map(|x| parse_value::<f32>(x, 1.0))
                    .collect();
                if 3 == scale.len() {
                    math::scale_matrix(&mut self._matrix, scale[0], scale[1], scale[2]);
                } else {
                    panic!("{} node has a invalid scale.", self._name);
                }
            }
        }
    }
}

impl ColladaController {
    pub fn create_collada_controller(xml_controller: &XmlTree) -> ColladaController {
        let mut collada_controller = ColladaController {
            _valid: false,
            _name: xml_controller
                .attributes
                .get("name")
                .unwrap()
                .replace(".", "_"),
            _id: xml_controller
                .attributes
                .get("id")
                .unwrap()
                .replace(".", "_"),
            _skin_source: String::new(),
            _bind_shape_matrix: Matrix4::identity(),
            _bone_names: Vec::new(),
            _bone_indices: Vec::new(),
            _bone_weights: Vec::new(),
            _inv_bind_matrices: Vec::new(),
        };

        collada_controller.parsing(xml_controller);
        collada_controller
    }

    pub fn parsing(&mut self, xml_controller: &XmlTree) {
        let xml_skin = xml_controller.get_elements("skin");
        if xml_skin.is_some() {
            let xml_skin = &xml_skin.unwrap()[0];

            self._skin_source = xml_skin.get_attribute("source", "");
            if false == self._skin_source.is_empty() && self._skin_source.starts_with("#") {
                self._skin_source = String::from(self._skin_source.get(1..).unwrap());
            }

            // parsing bind_shape_matrix
            let bind_shape_matrix = xml_skin.get_elements("bind_shape_matrix");
            if bind_shape_matrix.is_some() {
                let bind_shape_matrix = &bind_shape_matrix.unwrap()[0];
                let float_array = parse_list::<f32>(bind_shape_matrix.text.as_str());
                self._bind_shape_matrix.copy_from_slice(&float_array);
            } else {
                self._bind_shape_matrix = Matrix4::identity();
            }

            // parse sources
            let sources: HashMap<String, ColladaSourceData> = parsing_source_data(&xml_skin);

            // get vertex position source id
            let mut joins_semantics: HashMap<String, SemanticInfo> = HashMap::new();
            let xml_joints = xml_skin.get_elements("joints");
            if xml_joints.is_some() {
                joins_semantics = parsing_semantic(&xml_joints.unwrap()[0]);
            }

            // parse vertex weights
            let xml_vertex_weights = xml_skin.get_elements("vertex_weights");
            if xml_vertex_weights.is_some() {
                let xml_vertex_weights = &xml_vertex_weights.unwrap()[0];
                // parse semantic
                let weights_semantics = parsing_semantic(xml_vertex_weights);

                // parse vertex weights
                let vcount_text =
                    xml::get_elements_text(&xml_vertex_weights.get_elements("vcount"), "");
                let v_text = xml::get_elements_text(&xml_vertex_weights.get_elements("v"), "");
                let vcount_list = parse_list::<i32>(&vcount_text);
                let v_list = parse_list::<i32>(&v_text);

                // make geometry data
                self.build(
                    &sources,
                    &joins_semantics,
                    &weights_semantics,
                    &vcount_list,
                    &v_list,
                );
            }
        }
    }

    pub fn build(
        &mut self,
        sources: &HashMap<String, ColladaSourceData>,
        joins_semantics: &HashMap<String, SemanticInfo>,
        weights_semantics: &HashMap<String, SemanticInfo>,
        vcount_list: &Vec<i32>,
        v_list: &Vec<i32>,
    ) {
        let semantic_stride: usize = weights_semantics.len();
        // build weights and indices
        let max_bone: usize = 4; // max influence bone count per vertex
        let weight_source_id: &String = &weights_semantics.get("WEIGHT").unwrap()._source;
        let weight_sources: &Vec<f32> = match sources.get(weight_source_id).unwrap() {
            ColladaSourceData::FloatArray(weight_sources) => weight_sources,
            _ => panic!("weight_sources parsing error."),
        };

        let mut index: usize = 0;
        for vcount in vcount_list.iter() {
            let vcount: usize = (*vcount) as usize;
            let mut bone_indices: Vec<u32> = Vec::new();
            let mut bone_weights: Vec<f32> = Vec::new();
            let index_end: usize = index + vcount * semantic_stride;
            let indices: &[i32] = v_list.get(index..index_end).unwrap();
            index += vcount * semantic_stride;
            for v in 0..max_bone {
                let joint = weights_semantics.get("JOINT");
                if joint.is_some() {
                    let offset = joint.unwrap()._offset;
                    if v < vcount {
                        bone_indices.push(indices[offset + v * semantic_stride] as u32);
                    } else {
                        bone_indices.push(0);
                    }
                }
                let weight = weights_semantics.get("WEIGHT");
                if joint.is_some() {
                    let offset = weight.unwrap()._offset;
                    if v < vcount {
                        bone_weights
                            .push(weight_sources[indices[offset + v * semantic_stride] as usize]);
                    } else {
                        bone_weights.push(0.0);
                    }
                }
            }
            self._bone_indices.push(Vector4::new(
                bone_indices[0],
                bone_indices[1],
                bone_indices[2],
                bone_indices[3],
            ));
            self._bone_weights.push(Vector4::new(
                bone_weights[0],
                bone_weights[1],
                bone_weights[2],
                bone_weights[3],
            ));
        }

        // joints
        let joint = joins_semantics.get("JOINT");
        if joint.is_some() {
            let joints_source = sources.get(&joint.unwrap()._source);
            if joints_source.is_some() {
                let bone_names: &Vec<String> = match joints_source.unwrap() {
                    ColladaSourceData::NameArray(bone_names) => bone_names,
                    _ => panic!("bone_names parsing error."),
                };
                self._bone_names = bone_names.clone();
            }
        }

        // INV_BIND_MATRIX
        let inv_bind_matrix = joins_semantics.get("INV_BIND_MATRIX");
        if inv_bind_matrix.is_some() {
            let inv_bind_matrices = sources.get(&inv_bind_matrix.unwrap()._source);
            if inv_bind_matrices.is_some() {
                let inv_bind_matrices: &Vec<Vec<f32>> = match inv_bind_matrices.unwrap() {
                    ColladaSourceData::VectorArray(inv_bind_matrices) => inv_bind_matrices,
                    _ => panic!("inv_bind_matrices parsing error."),
                };
                self._inv_bind_matrices = inv_bind_matrices
                    .iter()
                    .map(|inv_bind_matrix_float_array| {
                        let mut inv_bind_matrix: Matrix4<f32> = Matrix4::identity();
                        inv_bind_matrix.copy_from_slice(&inv_bind_matrix_float_array);
                        inv_bind_matrix
                    })
                    .collect();
            }
        }
        self._valid = true;
    }
}

impl ColladaAnimation {
    pub fn create_collada_animation(
        xml_animation: &XmlTree,
        node_name_map: &HashMap<String, String>,
    ) -> ColladaAnimation {
        let mut collada_animation = ColladaAnimation {
            _valid: false,
            _id: xml_animation.get_attribute("id", "").replace(".", "_"),
            _target: String::new(), // target bone name
            _type: String::new(),   // transform(Matrix), location.X ... scale.z
            _inputs: Vec::new(),
            _outputs: Vec::new(),
            _interpolations: Vec::new(),
            _in_tangents: Vec::new(),
            _out_tangents: Vec::new(),
        };

        collada_animation.parsing(xml_animation, node_name_map);
        collada_animation
    }

    pub fn parsing(&mut self, xml_animation: &XmlTree, node_name_map: &HashMap<String, String>) {
        let sources = parsing_source_data(xml_animation);

        let mut joins_semantics: HashMap<String, SemanticInfo> = HashMap::new();
        let xml_sampler = xml_animation.get_elements("sampler");
        if xml_sampler.is_some() {
            joins_semantics = parsing_semantic(&xml_sampler.unwrap()[0]);
        }

        let xml_channel = xml_animation.get_elements("channel");
        let target: String = xml::get_elements_attribute(&xml_channel, "target", "");
        if target.contains("/") {
            let target: Vec<&str> = target.splitn(2, "/").collect();
            self._type = String::from(target[1]);
            self._target = match node_name_map.get(target[0]) {
                Some(target) => target.clone(),
                None => String::from(target[0]),
            };
        }

        if let Some(input) = joins_semantics.get("INPUT") {
            if let Some(input_source) = sources.get(&input._source) {
                self._inputs = input_source.get_float_array();
            }
        }

        if let Some(output) = joins_semantics.get("OUTPUT") {
            if let Some(output_source) = sources.get(&output._source) {
                match output_source {
                    ColladaSourceData::VectorArray(matrices) => {
                        for matrix in matrices.iter() {
                            self._outputs.push(Matrix4::identity());
                            self._outputs.last_mut().unwrap().copy_from_slice(&matrix);
                        }
                    }
                    ColladaSourceData::FloatArray(data) => {
                        log::debug!("ignore animation OUTPUT: {:?} {:?}", output._source, data);
                    }
                    _ => {}
                }
            }
        }

        if let Some(interpolation) = joins_semantics.get("INTERPOLATION") {
            if let Some(interpolation_source) = sources.get(&interpolation._source) {
                self._interpolations = interpolation_source.get_name_array();
            }
        }

        if let Some(in_tangent) = joins_semantics.get("IN_TANGENT") {
            if let Some(in_tangent_source) = sources.get(&in_tangent._source) {
                self._in_tangents = in_tangent_source.get_vector_array();
            }
        }

        if let Some(out_tangent) = joins_semantics.get("OUT_TANGENT") {
            if let Some(out_tangent_source) = sources.get(&out_tangent._source) {
                self._out_tangents = out_tangent_source.get_vector_array();
            }
        }

        if self._type.is_empty() || self._target.is_empty() || self._inputs.is_empty() {
            self._valid = false;
            panic!("{:?} has a invalid animation.\n{:?}", self._target, sources);
        } else {
            self._valid = true;
        }
    }
}

impl ColladaGeometry {
    pub fn create_collada_geometry(
        xml_geometry: &XmlTree,
        controllers: &Vec<ColladaController>,
        nodes: &Vec<ColladaNode>,
    ) -> ColladaGeometry {
        let mut collada_geometry = ColladaGeometry {
            _valid: false,
            _name: xml_geometry.get_attribute("name", "").replace(".", "_"),
            _id: xml_geometry.get_attribute("id", "").replace(".", "_"),
            _positions: Vec::new(),
            _bone_indices: Vec::new(),
            _bone_weights: Vec::new(),
            _normals: Vec::new(),
            _colors: Vec::new(),
            _texcoords: Vec::new(),
            _indices: Vec::new(),
            _bind_shape_matrix: Matrix4::identity(),
            _controller: std::ptr::null(),
        };

        // find matched controller
        for controller in controllers.iter() {
            if controller._skin_source == collada_geometry._id {
                collada_geometry._controller = controller;
                break;
            }
        }

        // find matrix
        for node in nodes.iter() {
            if node._name == collada_geometry._name {
                collada_geometry._bind_shape_matrix = node._matrix.clone() as Matrix4<f32>;
                break;
            }
        }

        if false == collada_geometry._controller.is_null() {
            unsafe {
                // precompute bind_shape_matrix as coulmn-major matrix calculation.
                collada_geometry._bind_shape_matrix = &collada_geometry._bind_shape_matrix
                    * &(*collada_geometry._controller)._bind_shape_matrix;
            }
        }

        collada_geometry.parsing(xml_geometry);
        collada_geometry
    }

    pub fn parsing(&mut self, xml_geometry: &XmlTree) {
        if let Some(xml_mesh) = xml_geometry.get_element("mesh") {
            // parse sources
            let sources = parsing_source_data(xml_mesh);

            // get vertex position source id
            let mut position_source_id = String::new();
            for xml_position in xml_mesh.get_elements("vertices/input").unwrap() {
                if "POSITION" == xml_position.get_attribute("semantic", "") {
                    position_source_id = xml_position.get_attribute("source", "");
                    if position_source_id.starts_with("#") {
                        position_source_id = String::from(position_source_id.get(1..).unwrap());
                    }
                    break;
                }
            }

            // parse polygons
            for tag in ["polygons", "polylist", "triangles"].iter() {
                if let Some(xml_polygons) = xml_mesh.get_element(tag) {
                    // parse semantic
                    let semantics: HashMap<String, SemanticInfo> = parsing_semantic(xml_polygons);
                    let semantic_stride: u32 = semantics.len() as u32;

                    // parse polygon indices
                    let mut vertex_index_list: Vec<u32> = Vec::new(); // flatten vertex list as triangle
                    #[allow(clippy::unnecessary_cast)]
                    if "triangles" == *tag {
                        if let Some(polygon) = xml_polygons.get_element("p") {
                            vertex_index_list = parse_list::<u32>(&polygon.text);
                        }
                    } else if "polylist" == *tag || "polygons" == *tag {
                        let mut vcount_list: Vec<u32> = Vec::new();
                        let mut polygon_index_list: Vec<u32> = Vec::new();
                        if "polylist" == *tag {
                            vcount_list = parse_list::<u32>(
                                xml::get_element_text(&xml_polygons.get_element("vcount"), "")
                                    .as_str(),
                            );
                            // flatten list
                            polygon_index_list = parse_list::<u32>(
                                xml::get_element_text(&xml_polygons.get_element("p"), "").as_str(),
                            )
                        } else if "polygons" == *tag {
                            for xml_p in xml_polygons.get_elements("p").unwrap() {
                                let polygon_indices = parse_list::<u32>(xml_p.text.as_str());
                                // flatten list
                                polygon_index_list.extend(polygon_indices.iter());
                                vcount_list.push(polygon_indices.len() as u32 / semantic_stride);
                            }
                        }
                        // triangulate
                        let mut elapsed_vindex = 0u32;
                        for vcount in vcount_list.iter() {
                            let index_end: usize =
                                (elapsed_vindex + vcount * semantic_stride) as usize;
                            let vertex_indices = Vec::from(
                                polygon_index_list
                                    .get(elapsed_vindex as usize..index_end)
                                    .unwrap(),
                            );
                            if 3 == (*vcount) {
                                vertex_index_list.extend(vertex_indices);
                            } else if 4 == (*vcount) {
                                vertex_index_list
                                    .extend(math::convert_triangulate(&vertex_indices));
                            } else {
                                panic!("too many polygon shape.")
                            }
                            elapsed_vindex += (*vcount) * semantic_stride;
                        }
                    }

                    // make geometry data
                    self.build(
                        &sources,
                        &position_source_id,
                        &semantics,
                        semantic_stride,
                        &vertex_index_list,
                    );
                    return;
                }
            }
        }
    }

    pub fn build(
        &mut self,
        sources: &HashMap<String, ColladaSourceData>,
        position_source_id: &String,
        semantics: &HashMap<String, SemanticInfo>,
        semantic_stride: u32,
        vertex_index_list: &Vec<u32>,
    ) {
        // check vertex count with bone weight count
        if false == self._controller.is_null() {
            let vertex_count = match sources.get(position_source_id) {
                Some(ColladaSourceData::VectorArray(positions)) => positions.len(),
                _ => 0,
            };
            unsafe {
                let bone_weight_count = (*self._controller)._bone_indices.len();
                if vertex_count != bone_weight_count {
                    panic!(
                        "Different count. vertex_count : {}, bone_weight_count : {}",
                        vertex_count, bone_weight_count
                    );
                }
            }
        }

        let mut index_map: HashMap<&[u32], u32> = HashMap::new();
        for i in 0..(vertex_index_list.len() / semantic_stride as usize) {
            let index_begin: usize = i * semantic_stride as usize;
            let index_end: usize = index_begin + semantic_stride as usize;
            let vert_indices: &[u32] = vertex_index_list.get(index_begin..index_end).unwrap();
            if index_map.contains_key(vert_indices) {
                self._indices.push(*index_map.get(vert_indices).unwrap());
            } else {
                let vertex_index = index_map.len() as u32;
                self._indices.push(vertex_index);
                index_map.insert(vert_indices, vertex_index);

                if let Some(vertex) = semantics.get("VERTEX") {
                    let vertex_index = vert_indices[vertex._offset];
                    let posisiton: Vector3<f32> = match sources.get(position_source_id).unwrap() {
                        ColladaSourceData::VectorArray(vector_array) => {
                            let v: &Vec<f32> = &vector_array[vertex_index as usize];
                            Vector3::new(v[0], v[1], v[2])
                        }
                        _ => panic!("VERTEX parsing error."),
                    };
                    self._positions.push(posisiton);

                    if false == self._controller.is_null() {
                        unsafe {
                            self._bone_indices.push(
                                (&*self._controller)._bone_indices[vertex_index as usize].into(),
                            );
                            self._bone_weights.push(
                                (&*self._controller)._bone_weights[vertex_index as usize].into(),
                            );
                        }
                    }
                }

                if let Some(normal) = semantics.get("NORMAL") {
                    let source_id = &normal._source;
                    let offset = normal._offset;
                    if let ColladaSourceData::VectorArray(normal) = sources.get(source_id).unwrap()
                    {
                        let normal = &normal[vert_indices[offset] as usize];
                        self._normals
                            .push(Vector3::new(normal[0], normal[1], normal[2]));
                    }
                }

                if let Some(color) = semantics.get("COLOR") {
                    let source_id = &color._source;
                    let offset = color._offset;
                    if let ColladaSourceData::VectorArray(color) = sources.get(source_id).unwrap() {
                        let color = &color[vert_indices[offset] as usize];
                        let r = min((color[0] * 255.0) as u32, 255);
                        let g = min((color[1] * 255.0) as u32, 255);
                        let b = min((color[2] * 255.0) as u32, 255);
                        let a = min((color[3] * 255.0) as u32, 255);
                        self._colors.push(vulkan_context::get_color32(r, g, b, a));
                    }
                }

                if let Some(texcoord) = semantics.get("TEXCOORD") {
                    let source_id = &texcoord._source;
                    let offset = texcoord._offset;
                    if let ColladaSourceData::VectorArray(texcoord) =
                        sources.get(source_id).unwrap()
                    {
                        let texcoord = &texcoord[vert_indices[offset] as usize];
                        self._texcoords
                            .push(Vector2::new(texcoord[0], 1.0 - texcoord[1]));
                    }
                }
            }
        }
        self._valid = true;
    }
}

impl Collada {
    fn create_collada(filepath: &PathBuf) -> Collada {
        let xml_tree = XmlTree::parse(filepath);
        let xml_root = &xml_tree.get_elements("COLLADA").unwrap()[0];
        let mut collada = Collada {
            _name: String::from(filepath.file_stem().unwrap().to_str().unwrap()),
            _collada_version: xml_tree.get_elements("COLLADA").unwrap()[0]
                .attributes
                .get("version")
                .unwrap()
                .clone(),
            _author: xml_tree
                .get_elements("COLLADA/asset/contributor/author")
                .unwrap()[0]
                .text
                .clone(),
            _authoring_tool: xml_tree
                .get_elements("COLLADA/asset/contributor/authoring_tool")
                .unwrap()[0]
                .text
                .clone(),
            _created: xml_tree.get_elements("COLLADA/asset/created").unwrap()[0]
                .text
                .clone(),
            _modified: xml_tree.get_elements("COLLADA/asset/modified").unwrap()[0]
                .text
                .clone(),
            _unit_name: match xml_tree.get_elements("COLLADA/asset/unit").unwrap()[0]
                .attributes
                .get("name")
            {
                Some(unit_name) => unit_name.clone(),
                None => String::from("meter"),
            },
            _unit_meter: match xml_tree.get_elements("COLLADA/asset/unit").unwrap()[0]
                .attributes
                .get("meter")
            {
                Some(unit_meter) => parse_value::<f32>(unit_meter, 1.0),
                None => 1.0,
            },
            _up_axis: xml_tree.get_elements("COLLADA/asset/up_axis").unwrap()[0]
                .text
                .clone(),
            _nodes: Vec::new(),
            _node_name_map: HashMap::new(),
            _geometries: Vec::new(),
            _controllers: Vec::new(),
            _animations: Vec::new(),
        };

        let nodes = xml_root.get_elements("library_visual_scenes/visual_scene/node");
        if nodes.is_some() {
            for xml_node in nodes.unwrap().iter() {
                collada._nodes.push(ColladaNode::create_collada_node(
                    xml_node,
                    std::ptr::null(),
                    0,
                ));
            }
            Collada::gather_node_name_map(&collada._nodes, &mut collada._node_name_map);
        }

        let xml_controllers = xml_root.get_elements("library_controllers/controller");
        if xml_controllers.is_some() {
            for xml_controller in xml_controllers.unwrap().iter() {
                let controller = ColladaController::create_collada_controller(xml_controller);
                collada._controllers.push(controller);
            }
        }

        let empty_xml_animations: Vec<XmlTree> = Vec::new();
        let xml_animations: &Vec<XmlTree> =
            match xml_root.get_elements("library_animations/animation") {
                Some(xml_animations) => xml_animations[0]
                    .get_elements("animation")
                    .unwrap_or_else(|| xml_animations),
                None => &empty_xml_animations,
            };

        for xml_animation in xml_animations.iter() {
            let animation =
                ColladaAnimation::create_collada_animation(xml_animation, &collada._node_name_map);
            if animation._valid {
                collada._animations.push(animation);
            }
        }

        for xml_geometry in xml_root
            .get_elements("library_geometries/geometry")
            .unwrap()
        {
            let geometry = ColladaGeometry::create_collada_geometry(
                xml_geometry,
                &collada._controllers,
                &collada._nodes,
            );
            collada._geometries.push(geometry);
        }

        collada
    }

    fn gather_node_name_map(nodes: &Vec<ColladaNode>, node_name_map: &mut HashMap<String, String>) {
        for node in nodes.iter() {
            node_name_map.insert(node._id.clone(), node._name.clone());
            Collada::gather_node_name_map(&node._children, node_name_map);
        }
    }

    pub fn build_hierarchy(
        controller: &ColladaController,
        parent_node: &ColladaNode,
        hierarchy_tree: &mut SkeletonHierarchyTree,
    ) {
        for child in parent_node._children.iter() {
            if controller._bone_names.contains(&child._name) {
                hierarchy_tree
                    ._children
                    .insert(child._name.clone(), SkeletonHierarchyTree::default());
                Collada::build_hierarchy(
                    controller,
                    child,
                    &mut hierarchy_tree._children.get_mut(&child._name).unwrap(),
                );
            }
        }
    }

    pub fn get_skeleton_data(&mut self) -> Vec<SkeletonDataCreateInfo> {
        let mut skeleton_data_list: Vec<SkeletonDataCreateInfo> = Vec::new();
        let mut check_duplicated: Vec<&str> = Vec::new();
        for controller in self._controllers.iter_mut() {
            if false == check_duplicated.contains(&controller._name.as_str()) {
                check_duplicated.push(controller._name.as_str());

                let mut hierarchy: SkeletonHierarchyTree = SkeletonHierarchyTree::default();
                let mut root_node: Option<&ColladaNode> = None;
                // find root armature
                for node in self._nodes.iter() {
                    if node._name.as_str() == controller._name.as_str() {
                        root_node = Some(node);
                        break;
                    }
                }

                if let Some(root_node) = root_node {
                    // recursive build hierarchy of bones
                    Collada::build_hierarchy(controller, &root_node, &mut hierarchy);
                }

                let skeleton_data = SkeletonDataCreateInfo {
                    _name: controller._name.clone(),
                    _transform: Matrix4::identity(),
                    _hierarchy: hierarchy, // bone names map as hierarchy
                    _bone_names: controller._bone_names.clone(), // bone name list ordered by index
                    _inv_bind_matrices: controller._inv_bind_matrices.clone(), // inverse matrix of bone
                };
                skeleton_data_list.push(skeleton_data);
            }
        }
        skeleton_data_list
    }

    pub fn precompute_animation(
        animations: &mut Vec<ColladaAnimation>,
        children_hierarchy: &SkeletonHierarchyTree,
        bone_names: &Vec<String>,
        inv_bind_matrices: &Vec<Matrix4<f32>>,
        _parent_matrix: &Matrix4<f32>,
        frame: usize,
    ) {
        for child in children_hierarchy._children.keys() {
            for i in 0..animations.len() {
                if *child == animations[i]._target {
                    // just Transpose child bones, no swap y-z.
                    let child_transform: Matrix4<f32> = animations[i]._outputs[frame].transpose();

                    if constants::COMBINED_INVERSE_BIND_MATRIX {
                        let child_bone_index = bone_names
                            .iter()
                            .position(|bone_name| *bone_name == animations[i]._target)
                            .unwrap();
                        let child_inv_bind_matrix = &inv_bind_matrices[child_bone_index];
                        animations[i]._outputs[frame] = child_transform * child_inv_bind_matrix;
                    } else {
                        animations[i]._outputs[frame] = child_transform;
                    }
                    // recursive precompute animation
                    Collada::precompute_animation(
                        animations,
                        children_hierarchy
                            ._children
                            .get(&animations[i]._target)
                            .unwrap(),
                        bone_names,
                        inv_bind_matrices,
                        &child_transform,
                        frame,
                    );
                    break;
                }
            }
        }
    }

    pub fn get_animation_data(
        &mut self,
        skeleton_data_list: &Vec<SkeletonDataCreateInfo>,
    ) -> Vec<Vec<AnimationNodeCreateInfo>> {
        // precompute_animation
        let mut animation_data_list: Vec<Vec<AnimationNodeCreateInfo>> = Vec::new();
        for skeleton_data in skeleton_data_list.iter() {
            let hierarchy = &skeleton_data._hierarchy;
            let bone_names = &skeleton_data._bone_names;
            let inv_bind_matrices = &skeleton_data._inv_bind_matrices;
            let animations: &mut Vec<ColladaAnimation> = &mut self._animations;
            for i in 0..animations.len() {
                // Currently, parsing only Transform Matrix. Future will parsing from location, rotation, scale.
                if "transform" != animations[i]._type.as_str() {
                    continue;
                }

                // Find root bone and skeleton data
                if hierarchy._children.contains_key(&animations[i]._target) {
                    // precompute all animation frames
                    for frame in 0..animations[i]._outputs.len() {
                        // only root bone adjust convert_matrix for swap Y-Z Axis
                        let transform: Matrix4<f32> =
                            animations[i]._outputs[frame].clone() as Matrix4<f32>;
                        if constants::COMBINED_INVERSE_BIND_MATRIX {
                            let bone_index = bone_names
                                .iter()
                                .position(|bone_name| *bone_name == animations[i]._target)
                                .unwrap();
                            let inv_bind_matrix: &Matrix4<f32> = &inv_bind_matrices[bone_index];
                            animations[i]._outputs[frame] = transform * inv_bind_matrix;
                        } else {
                            animations[i]._outputs[frame] = transform;
                        }
                        // recursive precompute animation
                        Collada::precompute_animation(
                            animations,
                            hierarchy._children.get(&animations[i]._target).unwrap(),
                            bone_names,
                            inv_bind_matrices,
                            &transform,
                            frame,
                        );
                    }
                }
            }

            // generate animation data
            let mut animation_data: Vec<AnimationNodeCreateInfo> = Vec::new(); // bone animation data list order by bone index
            for bone_name in bone_names.iter() {
                let mut find_animation_node: bool = false;
                for animation_node in self._animations.iter() {
                    if *bone_name == animation_node._target {
                        let animation_node_data = AnimationNodeCreateInfo {
                            _name: format!("{}_{}_{}", self._name, skeleton_data._name, bone_name),
                            _combined_inv_bind_matrix: constants::COMBINED_INVERSE_BIND_MATRIX,
                            _target: animation_node._target.clone(),
                            _times: animation_node._inputs.clone(),
                            _locations: animation_node
                                ._outputs
                                .iter()
                                .map(|output| math::extract_location(output))
                                .collect(),
                            _rotations: animation_node
                                ._outputs
                                .iter()
                                .map(|output| math::extract_quaternion(output))
                                .collect(),
                            _scales: animation_node
                                ._outputs
                                .iter()
                                .map(|output| math::extract_scale(output))
                                .collect(),
                            _interpolations: animation_node._interpolations.clone(),
                            _in_tangents: animation_node._in_tangents.clone(),
                            _out_tangents: animation_node._out_tangents.clone(),
                        };
                        animation_data.push(animation_node_data);
                        find_animation_node = true;
                        break;
                    }
                }

                if false == find_animation_node {
                    log::debug!("not found {} animation data_list", bone_name);
                    let animation_node_data = AnimationNodeCreateInfo {
                        _name: format!("{}_{}_{}", self._name, skeleton_data._name, bone_name),
                        _target: bone_name.clone(),
                        ..Default::default()
                    };
                    animation_data.push(animation_node_data);
                }
            }
            animation_data_list.push(animation_data);
        }
        animation_data_list
    }

    pub fn get_geometry_data(&mut self) -> Vec<GeometryCreateInfo> {
        let mut geometry_data_list: Vec<GeometryCreateInfo> = Vec::new();
        for i in 0..self._geometries.len() {
            let geometry: &mut ColladaGeometry = &mut self._geometries[i];

            // precompute bind_shape_matrix
            for position_index in 0..geometry._positions.len() {
                let position: Vector4<f32> = Vector4::new(
                    geometry._positions[position_index].x,
                    geometry._positions[position_index].y,
                    geometry._positions[position_index].z,
                    1.0,
                );
                geometry._positions[position_index] =
                    glm::vec4_to_vec3(&(&geometry._bind_shape_matrix * &position));
            }

            let bounding_box: BoundingBox = BoundingBox::calc_bounding_box(&geometry._positions);

            for normal_index in 0..geometry._normals.len() {
                let normal = Vector4::new(
                    geometry._normals[normal_index].x,
                    geometry._normals[normal_index].y,
                    geometry._normals[normal_index].z,
                    0.0,
                );
                geometry._normals[normal_index] =
                    glm::vec4_to_vec3(&(&geometry._bind_shape_matrix * &normal)).normalize();
            }

            let tangents = geometry_buffer::compute_tangent(
                &geometry._positions,
                &geometry._normals,
                &geometry._texcoords,
                &geometry._indices,
            );

            let vertex_color = vulkan_context::get_color32(255, 255, 255, 255);
            let vertex_count = geometry._positions.len();
            let mut vertex_data_list: Vec<VertexData> = Vec::new();
            let mut skeletal_vertex_data_list: Vec<SkeletalVertexData> = Vec::new();

            if geometry._bone_indices.is_empty() {
                vertex_data_list.reserve(vertex_count);
                for index in 0..vertex_count {
                    vertex_data_list.push(VertexData {
                        _position: geometry._positions[index].clone() as Vector3<f32>,
                        _normal: geometry._normals[index].clone() as Vector3<f32>,
                        _tangent: tangents[index].clone() as Vector3<f32>,
                        _color: vertex_color,
                        _texcoord: geometry._texcoords[index].clone() as Vector2<f32>,
                    });
                }
            } else {
                skeletal_vertex_data_list.reserve(vertex_count);
                for index in 0..vertex_count {
                    skeletal_vertex_data_list.push(SkeletalVertexData {
                        _position: geometry._positions[index].clone() as Vector3<f32>,
                        _normal: geometry._normals[index].clone() as Vector3<f32>,
                        _tangent: tangents[index].clone() as Vector3<f32>,
                        _color: vertex_color,
                        _texcoord: geometry._texcoords[index].clone() as Vector2<f32>,
                        _bone_indices: geometry._bone_indices[index].clone() as Vector4<u32>,
                        _bone_weights: geometry._bone_weights[index].clone() as Vector4<f32>,
                    });
                }
            }

            geometry_data_list.push(GeometryCreateInfo {
                _vertex_data_list: vertex_data_list,
                _skeletal_vertex_data_list: skeletal_vertex_data_list,
                _indices: geometry._indices.clone(),
                _bounding_box: bounding_box,
                ..Default::default()
            });
        }

        geometry_data_list
    }

    pub fn get_mesh_data_create_infos(filename: &PathBuf) -> MeshDataCreateInfo {
        log::info!("Collada: {:?}", filename);
        let mut collada = Collada::create_collada(filename);
        let geometry_data_list = collada.get_geometry_data();
        let skeleton_data_list = collada.get_skeleton_data();
        let animation_data_list = collada.get_animation_data(&skeleton_data_list);
        let bounding_box = MeshDataCreateInfo::calc_mesh_bounding_box(&geometry_data_list);

        MeshDataCreateInfo {
            _bound_box: bounding_box,
            _skeleton_create_infos: skeleton_data_list,
            _animation_node_create_infos: animation_data_list,
            _geometry_create_infos: geometry_data_list,
        }
    }
}
