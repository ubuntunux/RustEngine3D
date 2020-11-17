use std::collections::HashMap;
use std::ops::Add;
use std::path::PathBuf;

use nalgebra::{
    self,
    Vector2,
    Vector3,
    Matrix4,
};

use crate::vulkan_context::vulkan_context;
use crate::utilities::bounding_box::BoundingBox;
use crate::vulkan_context::geometry_buffer::{
    self,
    GeometryCreateInfo,
    VertexData,
};
use crate::utilities::xml::{
    self,
    XmlTree,
};
use crate::utilities::math;


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
    pub _node_name_map: HashMap::<String, String>,
    pub _geometries: Vec<ColladaGeometry>,
    pub _controllers: Vec<ColladaContoller>,
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
pub struct ColladaContoller {
    pub _valid: bool,
    pub _name: String,
    pub _id: String,
    pub _skin_source: String,
    pub _bind_shape_matrix: Matrix4<f32>,
    pub _bone_names: Vec<String>,
    pub _bone_indicies: Vec<Vec<u32>>,
    pub _bone_weights: Vec<Vec<f32>>,
    pub _inv_bind_matrices: Vec<Matrix4<f32>>,
}

#[derive(Debug, Clone)]
pub struct SematicInfo {
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
    pub _outputs: Vec<f32>,
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
    pub _bone_indicies: Vec<u32>,
    pub _bone_weights: Vec<f32>,
    pub _normals: Vec<Vector3<f32>>,
    pub _colors: Vec<u32>,
    pub _texcoords: Vec<Vector2<f32>>,
    pub _indices: Vec<u32>,
    pub _bind_shape_matrix: Matrix4<f32>,
    pub _controller: *const ColladaContoller,
}

#[derive(Debug, Clone)]
pub enum ColladaSourceData {
    FloatArray(Vec<f32>),
    VectorArray(Vec<Vec<f32>>),
    NameArray(Vec<String>),
}


pub fn parse_value<T: std::str::FromStr>(data: &str, default_value: T) -> T {
    match data.parse::<T>() {
        Ok(value) => value,
        Err(_) => default_value
    }
}

pub fn parse_list<T: std::str::FromStr>(datas: &str) -> Vec<T> {
    let datas = datas.replace("[", "").replace("]", "");
    let data_list = datas.split(" ").map(|data| data.trim().parse::<T>());
    let mut values: Vec<T> = Vec::new();
    for data in data_list {
        match data {
            Ok(value) => values.push(value),
            Err(_) => return Vec::new(),
        }
    }
    values
}

pub fn parse_vector_list<T: std::str::FromStr + Clone>(datas: &str, component_count: usize) -> Vec<Vec<T>> {
    let data_list: Vec<Result<T, T::Err>> = datas.replace("[", "").replace("]", "").split(",").map(|data| data.trim().parse::<T>()).collect();
    let mut values: Vec<Vec<T>> = Vec::new();
    let len = data_list.len() / component_count;
    for i in 0..len {
        let mut components: Vec<T> = Vec::new();
        for j in 0..component_count {
            match &data_list[i * component_count + j] {
                Ok(value) => components.push(value.clone()),
                Err(_) => return Vec::new(),
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
            let accessor_element = xml_source.get_element("technique_common/accessor");
            let stride = parse_value::<usize>(&accessor_element.attributes.get("stride").unwrap(), 0);
            let float_array_elements = xml_source.get_elements("float_array");
            if float_array_elements.is_some() {
                let source_text = &float_array_elements.unwrap()[0].text;
                if false == source_text.is_empty() {
                    if 1 < stride {
                        let source_data = ColladaSourceData::VectorArray(parse_vector_list::<f32>(source_text.as_str(), stride));
                        sources.insert(source_id.clone(), source_data);
                    } else {
                        let source_data = ColladaSourceData::FloatArray(parse_list::<f32>(source_text.as_str()));
                        sources.insert(source_id.clone(), source_data);
                    }
                }
            }

            let name_array_elements = xml_source.get_elements("Name_array");
            if name_array_elements.is_some() {
                let source_text = &name_array_elements.unwrap()[0].text;
                if false == source_text.is_empty() {
                    let source_data = ColladaSourceData::NameArray(parse_list::<String>(source_text.as_str()));
                    sources.insert(source_id.clone(), source_data);
                }
            }
        }
    }
    sources
}

//     :param xml_element:
//     :return: {'semantic':{'source', 'offset', 'set'}
pub fn parsing_sematic(xml_element: &XmlTree) -> HashMap<String, SematicInfo> {
    let mut semantics: HashMap<String, SematicInfo> = HashMap::new();
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
                },
                None => String::from(""),
            };

            let source: &str = match xml_semantic.attributes.get("source") {
                Some(source) => {
                    if source.starts_with("#") {
                        source.get(1..).unwrap()
                    } else {
                        source.as_str()
                    }
                },
                None => "",
            };

            let offset: usize = match xml_semantic.attributes.get("offset") {
                Some(offset) => parse_value(offset, 0),
                None => 0,
            };

            semantics.insert(semantic, SematicInfo {
                _source: String::from(source),
                _offset: offset,
                _set_number: String::from(set_number),
            });
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
    pub fn create_collada_node(xml_node: &XmlTree, parent: *const ColladaNode, depth: usize) -> ColladaNode {
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
                let child = ColladaNode::create_collada_node(xml_child_node, &collada_node, depth + 1);
                collada_node._children.push(child);
            }
        }
        collada_node
    }

    pub fn parsing_matrix(&mut self, xml_node: &XmlTree) {
        let xml_matrix = xml_node.get_elements("matrix");
        if xml_matrix.is_some() {
            // transform matrix
            let matrix:Vec<f32> = xml_matrix.unwrap()[0].text.split(" ").map(|x| {
                parse_value::<f32>(x, 0.0)
            }).collect();
            if 16 == matrix.len() {
                self._matrix.copy_from_slice(&matrix);
                self._matrix.transpose_mut();
            } else {
                panic!("parsing matrix error.");
            }
        } else {
            // location, rotation, scale
            let xml_translate = xml_node.get_elements("translate");
            if xml_translate.is_some() {
                let translation: Vec<f32> = xml_translate.unwrap()[0].text.split(" ").map(|x| {
                    parse_value::<f32>(x, 0.0)
                }).collect();
                if 3 == translation.len() {
                    math::matrix_translate(&mut self._matrix, translation[0], translation[1], translation[2]);
                } else {
                    panic!("{} node has a invalid translate.", self._name);
                }
            }

            let xml_rotates = xml_node.get_elements("rotate");
            if xml_rotates.is_some() {
                for xml_rotate in xml_rotates.unwrap().iter() {
                    let rotation: Vec<f32> = xml_rotate.text.split(" ").map(|x| {
                        parse_value::<f32>(x, 0.0)
                    }).collect();
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
                let scale: Vec<f32> = xml_scale.unwrap()[0].text.split(" ").map(|x| {
                    parse_value::<f32>(x, 1.0)
                }).collect();
                if 3 == scale.len() {
                    math::matrix_scale(&mut self._matrix, scale[0], scale[1], scale[2]);
                } else {
                    panic!("{} node has a invalid scale.", self._name);
                }
            }
        }
    }
}

impl ColladaContoller {
    pub fn create_collada_controller(xml_controller: &XmlTree) -> ColladaContoller {
        let mut collada_controlelr = ColladaContoller {
            _valid: false,
            _name: xml_controller.attributes.get("name").unwrap().replace(".", "_"),
            _id: xml_controller.attributes.get("id").unwrap().replace(".", "_"),
            _skin_source: String::new(),
            _bind_shape_matrix: Matrix4::identity(),
            _bone_names: Vec::new(),
            _bone_indicies: Vec::new(),
            _bone_weights: Vec::new(),
            _inv_bind_matrices: Vec::new(),
        };

        collada_controlelr.parsing(xml_controller);
        collada_controlelr
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
                self._bind_shape_matrix.transpose_mut();
            } else {
                self._bind_shape_matrix = Matrix4::identity();
            }

            // parse sources
            let sources: HashMap<String, ColladaSourceData> = parsing_source_data(&xml_skin);

            // get vertex position source id
            let mut joins_semantics: HashMap<String, SematicInfo> = HashMap::new();
            let xml_joints = xml_skin.get_elements("joints");
            if xml_joints.is_some() {
                joins_semantics = parsing_sematic(&xml_joints.unwrap()[0]);
            }

            // parse vertex weights
            let xml_vertex_weights = xml_skin.get_elements("vertex_weights");
            if xml_vertex_weights.is_some() {
                let xml_vertex_weights = &xml_vertex_weights.unwrap()[0];
                // parse semantic
                let weights_semantics = parsing_sematic(xml_vertex_weights);

                // parse vertex weights
                let vcount_text = xml::get_xml_text(&xml_vertex_weights.get_elements("vcount"), "");
                let v_text = xml::get_xml_text(&xml_vertex_weights.get_elements("v"), "");
                let vcount_list = parse_list::<i32>(&vcount_text);
                let v_list = parse_list::<i32>(&v_text);

                // make geomtry data
                self.build(&sources, &joins_semantics, &weights_semantics, &vcount_list, &v_list);
            }
        }
    }

    pub fn build(
        &mut self,
        sources: &HashMap<String, ColladaSourceData>,
        joins_semantics: &HashMap<String, SematicInfo>,
        weights_semantics: &HashMap<String, SematicInfo>,
        vcount_list: &Vec<i32>,
        v_list: &Vec<i32>,
    ) {
        let semantic_stride: usize = weights_semantics.len();
        // build weights and indicies
        let max_bone: usize = 4; // max influence bone count per vertex
        let weight_source_id: &String = &weights_semantics.get("WEIGHT").unwrap()._source;
        let weight_sources: &Vec<f32> = match sources.get(weight_source_id).unwrap() {
            ColladaSourceData::FloatArray(weight_sources) => weight_sources,
            _ => panic!("weight_sources parsing error."),
        };
        let mut index: usize = 0;
        for vcount in vcount_list.iter() {
            let vcount: usize = (*vcount) as usize;
            let mut bone_indicies: Vec<u32> = Vec::new();
            let mut bone_weights: Vec<f32> = Vec::new();
            let index_end: usize = index + vcount * semantic_stride;
            let indicies: &[i32] = v_list.get(index..index_end).unwrap();
            index += vcount * semantic_stride;
            for v in 0..max_bone {
                let joint = weights_semantics.get("JOINT");
                if joint.is_some() {
                    let offset = joint.unwrap()._offset;
                    if v < vcount {
                        bone_indicies.push(indicies[offset + v * semantic_stride] as u32);
                    } else {
                        bone_indicies.push(0);
                    }
                }
                let weight = weights_semantics.get("WEIGHT");
                if joint.is_some() {
                    let offset = weight.unwrap()._offset;
                    if v < vcount {
                        bone_weights.push(weight_sources[indicies[offset + v * semantic_stride] as usize]);
                    } else {
                        bone_weights.push(0.0);
                    }
                }
            }
            self._bone_indicies.push(bone_indicies);
            self._bone_weights.push(bone_weights);
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
                self._inv_bind_matrices = inv_bind_matrices.iter().map(|inv_bind_matrix_float_array| {
                    let mut inv_bind_matrix: Matrix4<f32> = Matrix4::identity();
                    inv_bind_matrix.copy_from_slice(&inv_bind_matrix_float_array);
                    inv_bind_matrix
                }).collect();
            }
        }
        self._valid = true;
    }
}


impl ColladaAnimation {
    pub fn create_collada_animation(xml_animation: &XmlTree, node_name_map: &HashMap<String, String>) -> ColladaAnimation {
        let mut collada_animation = ColladaAnimation {
            _valid: false,
            _id: xml_animation.get_attribute("id", "").replace(".", "_"),
            _target: String::new(),  // target bone name
            _type: String::new(),  // transform(Matrix), location.X ... scale.z
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

        let mut joins_semantics: HashMap<String, SematicInfo> = HashMap::new();
        let xml_sampler = xml_animation.get_elements("sampler");
        if xml_sampler.is_some() {
            joins_semantics = parsing_sematic(&xml_sampler.unwrap()[0]);
        }

        let xml_channel = xml_animation.get_elements("channel");
        let target: String = xml::get_xml_attribute(&xml_channel, "target", "");
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
                self._outputs = output_source.get_float_array();
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
            log::error!("{:?} has a invalid animation.\n{:?}", self._target, sources);
        } else {
            self._valid = true;
        }
    }
}

impl ColladaGeometry {
    pub fn create_collada_geometry(xml_geometry: &XmlTree, controllers: &Vec<ColladaContoller>, nodes: &Vec<ColladaNode>) -> ColladaGeometry {
        let mut collada_geometry = ColladaGeometry {
            _valid: false,
            _name: xml_geometry.get_attribute("name", "").replace(".", "_"),
            _id: xml_geometry.get_attribute("id", "").replace(".", "_"),
            _positions: Vec::new(),
            _bone_indicies: Vec::new(),
            _bone_weights: Vec::new(),
            _normals: Vec::new(),
            _colors: Vec::new(),
            _texcoords: Vec::new(),
            _indices: Vec::new(),
            _bind_shape_matrix: Matrix4::identity(),
            _controller: std::ptr::null()
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
                collada_geometry._bind_shape_matrix = &collada_geometry._bind_shape_matrix * &(*collada_geometry._controller)._bind_shape_matrix;
            }
        }

        //collada_geometry.parsing(xml_geometry);
        collada_geometry
    }
//
//     def parsing(self, xml_geometry):
//         xml_mesh = xml_geometry.find('mesh')
//         if xml_mesh is not None:
//             # parse sources
//             sources = parsing_source_data(xml_mesh)
//
//             # get vertex position source id
//             position_source_id = ""
//             for xml_position in xml_mesh.findall('vertices/input'):
//                 if get_xml_attrib(xml_position, 'semantic') == 'POSITION':
//                     position_source_id = get_xml_attrib(xml_position, 'source')
//                     if position_source_id.startswith("#"):
//                         position_source_id = position_source_id[1:]
//                     break
//
//             # parse polygons
//             for tag in ('polygons', 'polylist', 'triangles'):
//                 xml_polygons = xml_mesh.find(tag)
//                 if xml_polygons is not None:
//                     # parse semantic
//                     semantics = parsing_sematic(xml_polygons)
//                     semantic_stride = len(semantics)
//
//                     # parse polygon indices
//                     vertex_index_list = []  # flatten vertex list as triangle
//                     if tag == 'triangles':
//                         vertex_index_list = get_xml_text(xml_polygons.find('p'))
//                         vertex_index_list = convert_list(vertex_index_list, int)
//                     elif tag == 'polylist' or tag == 'polygons':
//                         vcount_list = []
//                         polygon_index_list = []
//                         if tag == 'polylist':
//                             vcount_list = convert_list(get_xml_text(xml_polygons.find('vcount')), int)
//                             # flatten list
//                             polygon_index_list = convert_list(get_xml_text(xml_polygons.find('p')), int)
//                         elif tag == 'polygons':
//                             for xml_p in xml_polygons.findall('p'):
//                                 polygon_indices = convert_list(get_xml_text(xml_p), int)
//                                 # flatten list
//                                 polygon_index_list += polygon_indices
//                                 vcount_list.append(int(len(polygon_indices) / semantic_stride))
//                         # triangulate
//                         elapsed_vindex = 0
//                         for vcount in vcount_list:
//                             if vcount == 3:
//                                 vertex_index_list += polygon_index_list[
//                                                      elapsed_vindex: elapsed_vindex + vcount * semantic_stride]
//                             else:
//                                 polygon_indices = polygon_index_list[
//                                                   elapsed_vindex: elapsed_vindex + vcount * semantic_stride]
//                                 vertex_index_list += convert_triangulate(polygon_indices, vcount, semantic_stride)
//                             elapsed_vindex += vcount * semantic_stride
//                     # make geomtry data
//                     self.build(sources, position_source_id, semantics, semantic_stride, vertex_index_list)
//                     return  # done
//
//     def build(self, sources, position_source_id, semantics, semantic_stride, vertex_index_list):
//         # check vertex count with bone weight count
//         if self.controller:
//             vertex_count = len(sources[position_source_id]) if position_source_id  else 0
//             bone_weight_count = len(self.controller.bone_indicies)
//             if vertex_count != bone_weight_count:
//                 logger.error(
//                     "Different count. vertex_count : %d, bone_weight_count : %d" % (vertex_count, bone_weight_count))
//                 return
//
//         indexMap = {}
//         for i in range(int(len(vertex_index_list) / semantic_stride)):
//             vertIndices = tuple(vertex_index_list[i * semantic_stride: i * semantic_stride + semantic_stride])
//             if vertIndices in indexMap:
//                 self.indices.append(indexMap[vertIndices])
//             else:
//                 self.indices.append(len(indexMap))
//                 indexMap[vertIndices] = len(indexMap)
//
//                 if 'VERTEX' in semantics:
//                     source_id = position_source_id
//                     offset = semantics['VERTEX']['offset']
//                     posisiton = sources[source_id][vertIndices[offset]]
//                     self.positions.append(posisiton)
//                     if self.controller:
//                         self.bone_indicies.append(self.controller.bone_indicies[vertIndices[offset]])
//                         self.bone_weights.append(self.controller.bone_weights[vertIndices[offset]])
//
//                 if 'NORMAL' in semantics:
//                     source_id = semantics['NORMAL']['source']
//                     offset = semantics['NORMAL']['offset']
//                     normal = sources[source_id][vertIndices[offset]]
//                     self.normals.append(normal)
//
//                 if 'COLOR' in semantics:
//                     source_id = semantics['COLOR']['source']
//                     offset = semantics['COLOR']['offset']
//                     self.colors.append(sources[source_id][vertIndices[offset]])
//
//                 if 'TEXCOORD' in semantics:
//                     source_id = semantics['TEXCOORD']['source']
//                     offset = semantics['TEXCOORD']['offset']
//                     self.texcoords.append(sources[source_id][vertIndices[offset]])
//         self.valid = True
}

impl Collada {
    fn create_collada(filepath: &PathBuf) -> Collada {
        let xml_tree = XmlTree::parse(filepath);
        let xml_root = &xml_tree.get_elements("COLLADA").unwrap()[0];
        let mut collada = Collada {
            _name: String::from(filepath.file_stem().unwrap().to_str().unwrap()),
            _collada_version: xml_tree.get_elements("COLLADA").unwrap()[0].attributes.get("version").unwrap().clone(),
            _author: xml_tree.get_elements("COLLADA/asset/contributor/author").unwrap()[0].text.clone(),
            _authoring_tool: xml_tree.get_elements("COLLADA/asset/contributor/authoring_tool").unwrap()[0].text.clone(),
            _created: xml_tree.get_elements("COLLADA/asset/created").unwrap()[0].text.clone(),
            _modified: xml_tree.get_elements("COLLADA/asset/modified").unwrap()[0].text.clone(),
            _unit_name: match xml_tree.get_elements("COLLADA/asset/unit").unwrap()[0].attributes.get("name") {
                Some(unit_name) => unit_name.clone(),
                None => String::from("meter"),
            },
            _unit_meter: match xml_tree.get_elements("COLLADA/asset/unit").unwrap()[0].attributes.get("meter") {
                Some(unit_meter) => parse_value::<f32>(unit_meter, 1.0),
                None => 1.0,
            },
            _up_axis: xml_tree.get_elements("COLLADA/asset/up_axis").unwrap()[0].text.clone(),
            _nodes: Vec::new(),
            _node_name_map: HashMap::new(),
            _geometries: Vec::new(),
            _controllers: Vec::new(),
            _animations: Vec::new(),
        };

        for xml_node in xml_root.get_elements("library_visual_scenes/visual_scene/node").unwrap().iter() {
            collada._nodes.push(ColladaNode::create_collada_node(xml_node, std::ptr::null(), 0));
        }

        Collada::gather_node_name_map(&collada._nodes, &mut collada._node_name_map);

        let xml_controllers = xml_root.get_elements("library_controllers/controller");
        if xml_controllers.is_some() {
            for xml_controller in xml_controllers.unwrap().iter() {
                let controller = ColladaContoller::create_collada_controller(xml_controller);
                collada._controllers.push(controller);
            }
        }

        let emptry_xml_animations: Vec<XmlTree> = Vec::new();
        let xml_animations: &Vec<XmlTree> = match xml_root.get_elements("library_animations/animation") {
            Some(xml_animations) => {
                match xml_animations[0].get_elements("animation") {
                    Some(xml_animations) => xml_animations,
                    None => xml_animations,
                }
            },
            None => &emptry_xml_animations,
        };

        for xml_animation in xml_animations.iter() {
            let animation = ColladaAnimation::create_collada_animation(xml_animation, &collada._node_name_map);
            if animation._valid {
                collada._animations.push(animation);
            }
        }

        for xml_geometry in xml_root.get_elements("library_geometries/geometry").unwrap() {
            let geometry = ColladaGeometry::create_collada_geometry(xml_geometry, &collada._controllers, &collada._nodes);
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

//     def get_mesh_data(self):
//         geometry_datas = self.get_geometry_data()
//         skeleton_datas = self.get_skeleton_data()
//         animation_datas = self.get_animation_data(skeleton_datas)
//         mesh_data = dict(
//             geometry_datas=geometry_datas,
//             skeleton_datas=skeleton_datas,
//             animation_datas=animation_datas
//         )
//         return mesh_data
//
//     def get_skeleton_data(self):
//         skeleton_datas = []
//         check_duplicated = []
//         for controller in self.controllers:
//             if controller.name not in check_duplicated:
//                 check_duplicated.append(controller.name)
//
//                 hierachy = {}
//                 root_node = None
//                 # find root amature
//                 for node in self.nodes:
//                     if node.name == controller.name:
//                         root_node = node
//                         break
//
//                 def build_hierachy(parent_node, hierachy_tree):
//                     for child in parent_node.children:
//                         if child.name in controller.bone_names:
//                             hierachy_tree[child.name] = dict()
//                             build_hierachy(child, hierachy_tree[child.name])
//
//                 if root_node:
//                     # recursive build hierachy of bones
//                     build_hierachy(root_node, hierachy)
//
//                 inv_bind_matrices = [swap_up_axis_matrix(matrix, True, True, self.up_axis) for matrix in controller.inv_bind_matrices]
//
//                 skeleton_data = dict(
//                     name=controller.name,
//                     hierachy=hierachy,  # bone names map as hierachy
//                     bone_names=controller.bone_names,  # bone name list ordered by index
//                     inv_bind_matrices=inv_bind_matrices  # inverse matrix of bone
//                 )
//                 skeleton_datas.append(skeleton_data)
//         return skeleton_datas
//
//     def get_animation_data(self, skeleton_datas):
//         precompute_parent_matrix = True
//         precompute_inv_bind_matrix = True
//
//         def get_empty_animation_node_data(animation_node_name, bone_name):
//             return dict(
//                 name=animation_node_name,
//                 target=bone_name
//             )
//
//         def get_animation_node_data(animation_node_name, animation_node):
//             return dict(
//                 name=animation_node_name,
//                 precompute_parent_matrix=precompute_parent_matrix,
//                 precompute_inv_bind_matrix=precompute_inv_bind_matrix,
//                 target=animation_node.target,
//                 times=animation_node.inputs,
//                 # transforms=[matrix for matrix in transforms],
//                 locations=[extract_location(np.array(matrix, dtype=np.float32).reshape(4, 4)) for matrix in animation_node.outputs],
//                 rotations=[extract_quaternion(np.array(matrix, dtype=np.float32).reshape(4, 4)) for matrix in animation_node.outputs],
//                 scales=[np.array([1.0, 1.0, 1.0], dtype=np.float32) for matrix in animation_node.outputs],
//                 interpoations=animation_node.interpolations,
//                 in_tangents=animation_node.in_tangents,
//                 out_tangents=animation_node.out_tangents
//             )
//
//         def precompute_animation(children_hierachy, bone_names, inv_bind_matrices, parent_matrix, frame=0):
//             for child in children_hierachy:
//                 for child_anim in self.animations:
//                     if child_anim.target == child:
//                         # just Transpose child bones, no swap y-z.
//                         child_transform = np.array(child_anim.outputs[frame], dtype=np.float32).reshape(4, 4).T
//                         if precompute_parent_matrix:
//                             child_transform = np.dot(child_transform, parent_matrix)
//
//                         if precompute_inv_bind_matrix:
//                             child_bone_index = bone_names.index(child_anim.target)
//                             child_inv_bind_matrix = inv_bind_matrices[child_bone_index]
//                             child_anim.outputs[frame] = np.dot(child_inv_bind_matrix, child_transform)
//                         else:
//                             child_anim.outputs[frame] = child_transform
//                         # recursive precompute animation
//                         precompute_animation(children_hierachy[child_anim.target], bone_names, inv_bind_matrices, child_transform, frame)
//                         break
//
//         # precompute_animation
//         animation_datas = []
//         for skeleton_data in skeleton_datas:
//             hierachy = skeleton_data['hierachy']  # tree data
//             bone_names = skeleton_data['bone_names']
//             inv_bind_matrices = skeleton_data['inv_bind_matrices']
//
//             for animation in self.animations:
//                 # Currently, parsing only Transform Matrix. Future will parsing from location, rotation, scale.
//                 if animation.type != 'transform':
//                     continue
//
//                 # Find root bone and skeleton data
//                 if animation.target in hierachy:
//                     # precompute all animation frames
//                     for frame, transform in enumerate(animation.outputs):
//                         # only root bone adjust convert_matrix for swap Y-Z Axis
//                         transform = swap_up_axis_matrix(np.array(transform, dtype=np.float32).reshape(4, 4), True, False, self.up_axis)
//                         if precompute_inv_bind_matrix:
//                             bone_index = bone_names.index(animation.target)
//                             inv_bind_matrix = inv_bind_matrices[bone_index]
//                             animation.outputs[frame] = np.dot(inv_bind_matrix, transform)
//                         else:
//                             animation.outputs[frame] = transform
//                         # recursive precompute animation
//                         precompute_animation(hierachy[animation.target], bone_names, inv_bind_matrices, transform, frame)
//             # generate animation data
//             animation_data = []  # bone animation data list order by bone index
//             animation_datas.append(animation_data)
//             for bone_name in bone_names:
//                 for animation in self.animations:
//                     if animation.target == bone_name:
//                         animation_node_name = "%s_%s_%s" % (self.name, skeleton_data['name'], bone_name)
//                         animation_data.append(get_animation_node_data(animation_node_name, animation))
//                         break
//                 else:
//                     logger.warn('not found %s animation datas' % bone_name)
//                     animation_node_name = "%s_%s_%s" % (self.name, skeleton_data['name'], bone_name)
//                     animation_data.append(get_empty_animation_node_data(animation_node_name, bone_name))
//
//         return animation_datas
//
//     def get_geometry_data(self):
//         geometry_datas = []
//         for geometry in self.geometries:
//             skeleton_name = ""
//             bone_indicies = []
//             bone_weights = []
//
//             if geometry.controller:
//                 skeleton_name = geometry.controller.name
//                 bone_indicies = copy.deepcopy(geometry.bone_indicies)
//                 bone_weights = copy.deepcopy(geometry.bone_weights)
//
//             # swap y and z
//             geometry.bind_shape_matrix = swap_up_axis_matrix(geometry.bind_shape_matrix, True, False, self.up_axis)
//
//             # precompute bind_shape_matrix
//             bound_min = Float3(FLOAT32_MAX, FLOAT32_MAX, FLOAT32_MAX)
//             bound_max = Float3(FLOAT32_MIN, FLOAT32_MIN, FLOAT32_MIN)
//             for i, position in enumerate(geometry.positions):
//                 geometry.positions[i] = np.dot([position[0], position[1], position[2], 1.0], geometry.bind_shape_matrix)[:3]
//                 position = geometry.positions[i]
//                 for j in range(3):
//                     if bound_min[j] > position[j]:
//                         bound_min[j] = position[j]
//                     if bound_max[j] < position[j]:
//                         bound_max[j] = position[j]
//
//             for i, normal in enumerate(geometry.normals):
//                 geometry.normals[i] = np.dot([normal[0], normal[1], normal[2], 0.0], geometry.bind_shape_matrix)[:3]
//                 geometry.normals[i] = normalize(geometry.normals[i])
//
//             geometry_data = dict(
//                 name=geometry.name,
//                 positions=copy.deepcopy(geometry.positions),
//                 normals=copy.deepcopy(geometry.normals),
//                 colors=copy.deepcopy(geometry.colors),
//                 texcoords=copy.deepcopy(geometry.texcoords),
//                 indices=copy.deepcopy(geometry.indices),
//                 skeleton_name=skeleton_name,
//                 bone_indicies=copy.deepcopy(bone_indicies),
//                 bone_weights=copy.deepcopy(bone_weights),
//                 bound_min=copy.deepcopy(bound_min),
//                 bound_max=copy.deepcopy(bound_max),
//                 radius=length(bound_max - bound_min)
//             )
//
//             geometry_datas.append(geometry_data)
//         return geometry_datas

    pub fn get_geometry_datas(filename: &PathBuf) -> Vec<GeometryCreateInfo> {
        let mut obj = Collada::create_collada(filename);
        println!("{:#?}", obj);
        //obj.parse(filename, 1.0, texcoord_y);
        //obj.generate_geometry_datas()
        panic!("get_geometry_datas");
        Vec::new()
    }
}