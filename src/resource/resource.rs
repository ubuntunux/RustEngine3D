use std::fs::{ self, File };
use std::io::prelude::*;
use std::path::{ Path, PathBuf };
use std::collections::HashMap;

use serde_json::{ self, Value, json };
use bincode;
use image::{
    self,
    GenericImageView,
};
use ash::{
    vk
};

use crate::application::SceneManagerData;
use crate::constants;
use crate::resource::obj_loader::WaveFrontOBJ;
use crate::resource::texture_generator;
use crate::renderer::mesh::{ MeshData };
use crate::renderer::model::{ self, ModelData };
use crate::renderer::material::{ self, MaterialData };
use crate::renderer::material_instance::{ self, MaterialInstanceData };
use crate::renderer::renderer::{ RendererData };
use crate::renderer::render_pass_create_info;
use crate::vulkan_context::descriptor::{ self, DescriptorData };
use crate::vulkan_context::framebuffer::{self, FramebufferData };
use crate::vulkan_context::geometry_buffer::{ self, GeometryCreateInfo, GeometryData };
use crate::vulkan_context::render_pass::{
    self,
    PipelineDataCreateInfo,
    RenderPassData,
    RenderPassPipelineDataName,
    RenderPassPipelineData,
};
use crate::vulkan_context::texture::{ TextureData, TextureCreateInfo };
use crate::utilities::system::{ self, RcRefCell, newRcRefCell };

const GATHER_ALL_FILES: bool = false;
const USE_JSON_FOR_MESH: bool = false;

const MATERIAL_FILE_PATH: &str = "resource/materials";
const MATERIAL_INSTANCE_FILE_PATH: &str = "resource/material_instances";
const MESH_SOURCE_FILE_PATH: &str = "resource/externals/meshes";
const MESH_FILE_PATH: &str = "resource/meshes";
const MODEL_FILE_PATH: &str = "resource/models";
const TEXTURE_SOURCE_FILE_PATH: &str = "resource/externals/textures";
const TEXTURE_FILE_PATH: &str = "resource/textures";

const EXT_OBJ: &str = "obj";
const EXT_COLLADA: &str = "dae";
const MESH_SOURCE_EXTS: [&str; 2] = [EXT_OBJ, EXT_COLLADA];
const EXT_JSON: &str = "json";
const EXT_MATERIAL: &str = "mat";
const EXT_MATERIAL_INSTANCE: &str = "matinst";
const EXT_MESH: &str = "mesh";
const EXT_MODEL: &str = "model";
const IMAGE_SOURCE_EXTS: [&str; 4] = ["jpg", "png", "tga", "bmp"];
const EXT_TEXTURE: &str = "texture";

const DEFAULT_MESH_NAME: &str = "quad";
const DEFAULT_MODEL_NAME: &str = "quad";
const DEFAULT_TEXTURE_NAME: &str = "common/default";
const DEFAULT_MATERIAL_NAME: &str = "render_pass_static_opaque";
const DEFAULT_MATERIAL_INSTANCE_NAME: &str = "default";
const DEFAULT_RENDER_PASS_NAME: &str = "render_pass_static_opaque";

pub type ResourceDataMap<T> = HashMap<String, RcRefCell<T>>;
pub type FramebufferDataMap = ResourceDataMap<FramebufferData>;
pub type MaterialDataMap = ResourceDataMap<material::MaterialData>;
pub type MaterialInstanceDataMap = ResourceDataMap<material_instance::MaterialInstanceData>;
pub type SceneManagerDataMap = ResourceDataMap<SceneManagerData>;
pub type MeshDataMap = ResourceDataMap<MeshData>;
pub type ModelDataMap = ResourceDataMap<model::ModelData>;
pub type TextureDataMap = ResourceDataMap<TextureData>;
pub type RenderPassDataMap = ResourceDataMap<RenderPassData>;
pub type DescriptorDataMap = ResourceDataMap<descriptor::DescriptorData>;
pub type MetaDataMap = ResourceDataMap<MetaData>;


#[derive(Debug, Clone)]
pub struct MetaData {
    _is_engine_resource: bool,
    _meta_file_path: bool,
    _resource_data_type: ResourceData,
    _resource_version: i32,
    _resource_file_path: PathBuf,
    _resource_modify_time: String,
    _source_file_path: PathBuf,
    _source_modify_time: String,
    _source_changed: bool
}

#[derive(Clone, Debug, Copy)]
pub enum ResourceData {
    ResourceDataMesh,
}

#[derive(Debug, Clone)]
pub struct Resources {
    pub _meta_data_map: MetaDataMap,
    pub _mesh_data_map: MeshDataMap,
    pub _model_data_map: ModelDataMap,
    pub _texture_data_map: TextureDataMap,
    pub _framebuffer_data_map: FramebufferDataMap,
    pub _render_pass_data_map: RenderPassDataMap,
    pub _material_data_map: MaterialDataMap,
    pub _material_instance_data_map: MaterialInstanceDataMap,
    pub _descriptor_data_map: DescriptorDataMap
}

pub fn create_resources() -> RcRefCell<Resources> {
    newRcRefCell(Resources {
        _meta_data_map: MetaDataMap::new(),
        _mesh_data_map: MeshDataMap::new(),
        _model_data_map: ModelDataMap::new(),
        _texture_data_map: TextureDataMap::new(),
        _framebuffer_data_map: FramebufferDataMap::new(),
        _render_pass_data_map: RenderPassDataMap::new(),
        _material_data_map: MaterialDataMap::new(),
        _material_instance_data_map: MaterialInstanceDataMap::new(),
        _descriptor_data_map: DescriptorDataMap::new()
    })
}

fn get_resource_data<'a, T>(resource_data_map: &'a ResourceDataMap<T>, resource_name: &String, default_resource_name: &str) -> &'a RcRefCell<T> {
    let maybe_data = resource_data_map.get(resource_name);
    match maybe_data {
        None => resource_data_map.get(default_resource_name).unwrap(),
        _ => maybe_data.unwrap(),
    }
}

fn get_resource_name_from_file_path(resource_root_path: &PathBuf, resource_file_path: &PathBuf) -> String {
    let mut resource_name = PathBuf::from(resource_file_path.parent().unwrap());
    resource_name.push(resource_file_path.file_stem().unwrap());
    String::from(system::get_relative_path(resource_root_path, &resource_name).to_str().unwrap())
}

fn get_unique_resource_name<T>(resource_map: &ResourceDataMap<T>, resource_root_path: &PathBuf, resource_file_path: &PathBuf) -> String {
    let resource_name = get_resource_name_from_file_path(resource_root_path, resource_file_path);
    system::generate_unique_name(resource_map, &resource_name)
}

pub fn get_resource_file_path(resource_root_path: &PathBuf, resource_name: &String, resource_ext: &str) -> PathBuf {
    let mut resource_file_path: PathBuf = PathBuf::from(resource_root_path);
    resource_file_path.push(resource_name);
    resource_file_path.set_extension(resource_ext);
    resource_file_path
}

impl Resources {
    pub fn initialize_resources(&mut self, renderer_data: &RendererData) {
        log::info!("initialize_resources");
        self.load_texture_datas(renderer_data);
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.load_mesh_datas(renderer_data);
        self.load_model_datas(renderer_data);
    }

    pub fn destroy_resources(&mut self, renderer_data: &RendererData) {
        log::info!("destroy_resources");
        self.unload_model_datas(renderer_data);
        self.unload_mesh_datas(renderer_data);
        self.unload_material_instance_datas(renderer_data);
        self.unload_material_datas(renderer_data);
        self.unload_framebuffer_datas(renderer_data);
        self.unload_render_pass_datas(renderer_data);
        self.unload_texture_datas(renderer_data);
        self.unload_descriptor_datas(renderer_data);
    }

    // GraphicsDatas
    pub fn load_graphics_datas(&mut self, renderer_data: &RendererData) {
        log::info!("load_graphics_datas");
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.update_material_instance_datas(renderer_data);
    }

    pub fn unload_graphics_datas(&mut self, renderer_data: &RendererData) {
        log::info!("unload_graphics_datas");
        self.unload_material_instance_datas(renderer_data);
        self.unload_material_datas(renderer_data);
        self.unload_framebuffer_datas(renderer_data);
        self.unload_render_pass_datas(renderer_data);
        self.unload_descriptor_datas(renderer_data);
    }

    pub fn create_resource(&mut self) {
        // nothing..
    }

    pub fn regist_resource(&mut self) {
        // nothing..
    }

    pub fn unregist_resource(&mut self) {
        // nothing..
    }

    // SceneManagerData
    pub fn load_scene_manager_datas(&mut self, _renderer_data: &RendererData) {
        // nothing..
    }

    pub fn unload_scene_manager_datas(&mut self, _renderer_data: &RendererData) {
        // nothing..
    }

    // ModelData
    pub fn load_model_datas(&mut self, renderer_data: &RendererData) {
        let model_file_path = PathBuf::from(MODEL_FILE_PATH);
        let model_files: Vec<PathBuf> = system::walk_directory(&model_file_path, &[EXT_MODEL]);
        for model_file in model_files {
            let model_name = get_unique_resource_name(&self._model_data_map, &model_file_path, &model_file);
            let contents = fs::read_to_string(model_file).expect("Something went wrong reading the file");
            // let mut lines = contents.lines();
            // let line = lines.next();
            // while None != line {
            // }
        }
        // forM_ modelFiles $ \modelFile -> do
        //     modelName <- getUniqueResourceName (_modelDataMap resources) modelPathBuf modelFile
        //     contents <- ByteString.readFile modelFile
        //     registModelData (_modelDataMap resources) modelName contents
        // where
        //     registModelData modelDataMap modelName contents = do
        //         let Just (Aeson.Object modelCreateInfoMap) = Aeson.decodeStrict contents
        //             Just (Aeson.Array materialInstanceNames) = HashMap.lookup "material_instances" modelCreateInfoMap
        //             materialInstanceCount = Vector.length materialInstanceNames
        //             Just (Aeson.String meshName) = HashMap.lookup "mesh" modelCreateInfoMap
        //         meshData <- getMeshData resources meshName
        //         geometryDataCount <- getGeometryDataCount meshData
        //         let materialInstanceNameList = (Vector.take geometryDataCount materialInstanceNames) Vector.++ (Vector.replicate (max 0 (geometryDataCount - materialInstanceCount)) (Aeson.String defaultMaterialInstanceName))
        //         materialInstanceDatas <- forM (Vector.toList materialInstanceNameList) $ \(Aeson.String materialInstanceName) ->
        //             getMaterialInstanceData resources materialInstanceName
        //         modelData <- Model.newModelData modelName meshData materialInstanceDatas
        //         HashTable.insert modelDataMap modelName modelData
    }

    pub fn unload_model_datas(&mut self, _renderer_data: &RendererData) {
        for model_data in self._model_data_map.values() {
            model_data.borrow().destroy_model_data();
        }
        self._model_data_map.clear();
    }

    pub fn get_model_data(&self, resource_name: &String) -> &RcRefCell<ModelData> {
        get_resource_data(&self._model_data_map, resource_name, DEFAULT_MODEL_NAME)
    }

    // Mesh Loader
    pub fn regist_mesh_data(
        &mut self,
        renderer_data: &RendererData,
        mesh_name: &String,
        geometry_create_infos: &Vec<GeometryCreateInfo>,
    ) {
        let mut geometry_datas: Vec<GeometryData> = Vec::new();
        for (i, geometry_create_info) in geometry_create_infos.iter().enumerate() {
            let geomtery_name: String = format!("{}_{}", mesh_name, i);
            let geometry_data = renderer_data.create_geometry_buffer(&geomtery_name, geometry_create_info);
            geometry_datas.push(geometry_data);
        }
        let mesh_data = MeshData::new_mesh_data(&mesh_name, geometry_datas);
        self._mesh_data_map.insert(mesh_name.clone(), newRcRefCell(mesh_data));
    }

    pub fn load_mesh_datas(&mut self, renderer_data: &RendererData) {
        self.regist_mesh_data(renderer_data, &String::from("quad"), &geometry_buffer::quad_geometry_create_infos());
        self.regist_mesh_data(renderer_data, &String::from("cube"), &geometry_buffer::cube_geometry_create_infos());
        let mesh_directory = PathBuf::from(MESH_FILE_PATH);
        let mesh_source_directory = PathBuf::from(MESH_SOURCE_FILE_PATH);
        let resource_ext = if USE_JSON_FOR_MESH { EXT_JSON } else { EXT_MESH };
        let mesh_files = system::walk_directory(mesh_directory.as_path(), &[resource_ext]);
        let mut mesh_file_map: HashMap<String, PathBuf> = HashMap::new();
        for mesh_file in mesh_files.iter() {
            let mesh_name = get_resource_name_from_file_path(&mesh_directory, &mesh_file);
            mesh_file_map.insert(mesh_name, mesh_file.clone());
        }
        let mesh_source_files = system::walk_directory(mesh_source_directory.as_path(), &MESH_SOURCE_EXTS);
        for mesh_source_file in mesh_source_files {
            let mesh_name = get_unique_resource_name(&self._mesh_data_map, &mesh_source_directory, &mesh_source_file);
            let src_file_ext: String = String::from(mesh_source_file.extension().unwrap().to_str().unwrap());
            let geometry_create_infos = match mesh_file_map.get(&mesh_name) {
                Some(mesh_file) => {
                    // Load mesh
                    let loaded_contents = fs::File::open(mesh_file).expect("Failed to create file");
                    let geometry_create_infos: Vec<GeometryCreateInfo>;
                    if USE_JSON_FOR_MESH {
                        geometry_create_infos = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
                    } else {
                        geometry_create_infos = bincode::deserialize_from(loaded_contents).unwrap();
                    }
                    geometry_create_infos
                },
                None => {
                    // Convert to mesh from source
                    let geometry_create_infos = match src_file_ext.as_str() {
                        EXT_OBJ => WaveFrontOBJ::get_geometry_datas(&mesh_source_file),
                        EXT_COLLADA => Vec::new(), // ColladaLoader.loadCollada meshSourceFile
                        _ => panic!("error")
                    };
                    // Save mesh
                    let mut mesh_file_path: PathBuf = mesh_directory.clone();
                    mesh_file_path.push(&mesh_name);
                    mesh_file_path.set_extension(resource_ext);
                    fs::create_dir_all(mesh_file_path.parent().unwrap()).expect("Failed to create directories.");
                    let mut write_file = File::create(mesh_file_path).expect("Failed to create file");
                    if USE_JSON_FOR_MESH {
                        let write_contents: String = serde_json::to_string(&geometry_create_infos).expect("Failed to serialize.");
                        write_file.write(write_contents.as_bytes()).expect("Failed to write");
                    } else {
                        let write_contents: Vec<u8> = bincode::serialize(&geometry_create_infos).unwrap();
                        write_file.write(&write_contents).expect("Failed to write");
                    }
                    geometry_create_infos
                },
            };
            self.regist_mesh_data(renderer_data, &mesh_name, &geometry_create_infos);
        }
    }

    pub fn unload_mesh_datas(&mut self, renderer_data: &RendererData) {
        for mesh_data in self._mesh_data_map.values() {
            for geometry_data in (*mesh_data).borrow().get_geomtry_datas() {
                renderer_data.destroy_geomtry_buffer(geometry_data);
            }
        }
        self._mesh_data_map.clear();
    }

    pub fn get_mesh_data(&self, resource_name: &String) -> &RcRefCell<MeshData> {
        get_resource_data(&self._mesh_data_map, resource_name, DEFAULT_MESH_NAME)
    }

    // TextureLoader
    pub fn get_texture_data_name(
        texture_source_directory: &PathBuf,
        texture_file: &PathBuf
    ) -> (String, Vec<PathBuf>) {
        let texture_data_name = get_resource_name_from_file_path(texture_source_directory, texture_file);
        let split_texture_file_names: Vec<&str> = texture_file.to_str().unwrap().rsplitn(2, "_").collect();
        if 1 < split_texture_file_names.len() {
            let texture_file_name: &str = split_texture_file_names[1];
            let file_ext: &str = texture_file.extension().unwrap().to_str().unwrap();
            let cube_face_files: Vec<PathBuf> = constants::CUBE_TEXTURE_FACES
                .iter()
                .map(|face| { PathBuf::from(format!("{}_{}.{}", texture_file_name, face, file_ext)) })
                .filter(|cube_texture_file| { cube_texture_file.is_file() })
                .collect();
            if constants::CUBE_TEXTURE_FACES.len() == cube_face_files.len() {
                return (texture_data_name, cube_face_files);
            }
        }
        (texture_data_name, Vec::new())
    }

    pub fn load_image_data(texture_file: &PathBuf) -> (u32, u32, Vec<u8>, vk::Format) {
        let image_file = image::io::Reader::open(texture_file).unwrap().decode();
        if image_file.is_err() {
            log::error!("load_image_data error: {:?}", texture_file);
            return (0, 0, Vec::new(), vk::Format::UNDEFINED);
        }
        let dynamic_image: image::DynamicImage = image_file.unwrap();
        let (image_width, image_height) = dynamic_image.dimensions();
        let (image_data_raw, image_format): (Vec<u8>, vk::Format) = match dynamic_image {
            image::DynamicImage::ImageRgba8(_) => (dynamic_image.to_rgba().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageRgb8(_) => (dynamic_image.to_rgba().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageLuma8(_) => (dynamic_image.to_rgba().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageRgb16(_) => (dynamic_image.to_rgba().into_raw(), vk::Format::R16G16B16A16_UNORM),
            _ => {
                log::error!("Unkown format: {:?}", texture_file);
                (Vec::new(), vk::Format::UNDEFINED)
            }
        };
        (image_width, image_height, image_data_raw, image_format)
    }

    pub fn load_image_datas(texture_files: &Vec<PathBuf>) -> (u32, u32, Vec<u8>, vk::Format) {
        let mut image_width: u32 = 0;
        let mut image_height: u32 = 0;
        let mut image_format: vk::Format = vk::Format::UNDEFINED;
        let mut image_datas: Vec<Vec<u8>> = Vec::new();
        for texture_file in texture_files.iter() {
            let (width, height, image_data, format): (u32, u32, Vec<u8>, vk::Format) = Resources::load_image_data(texture_file);
            image_width = width;
            image_height = height;
            image_format = format;
            image_datas.push(image_data.clone());
        }
        let xs: Vec<u8> = image_datas.concat();
        (image_width, image_height, xs, image_format)
    }

    pub fn load_texture_datas(&mut self, renderer_data: &RendererData) {
        //let texture_directory = PathBuf::from(TEXTURE_FILE_PATH);
        let texture_source_directory = PathBuf::from(TEXTURE_SOURCE_FILE_PATH);

        let texture_datas: Vec<TextureData> = texture_generator::generate_textures(renderer_data);
        for texture_data in texture_datas {
            self._texture_data_map.insert(texture_data._texture_data_name.clone(), newRcRefCell(texture_data));
        }

        // generate necessary texture datas
        texture_generator::generate_images(&texture_source_directory);

        // load texture from files
        let texture_src_files = system::walk_directory(texture_source_directory.as_path(), &IMAGE_SOURCE_EXTS);
        for texture_src_file in texture_src_files.iter() {
            let (texture_data_name, cube_texture_files) = Resources::get_texture_data_name(
                &texture_source_directory,
                &texture_src_file
            );
            let is_cube_texture: bool = false == cube_texture_files.is_empty();
            let existing_resource_data: Option<&RcRefCell<TextureData>> = self._texture_data_map.get(&texture_data_name);
            if existing_resource_data.is_none() {
                let (image_width, image_height, image_data, image_format): (u32, u32, Vec<u8>, vk::Format) =
                    if is_cube_texture {
                        Resources::load_image_datas(&cube_texture_files)
                    } else {
                        Resources::load_image_data(texture_src_file)
                    };
                let image_view_type: vk::ImageViewType = if is_cube_texture {
                    vk::ImageViewType::CUBE
                } else {
                    vk::ImageViewType::TYPE_2D
                };
                if vk::Format::UNDEFINED != image_format {
                    let texture_create_info = TextureCreateInfo {
                        _texture_width: image_width,
                        _texture_height: image_height,
                        _texture_format: image_format,
                        _texture_view_type: image_view_type,
                        _texture_initial_datas: image_data,
                        ..Default::default()
                    };
                    let texture_data = renderer_data.create_texture(&texture_data_name, &texture_create_info);
                    self._texture_data_map.insert(texture_data_name, newRcRefCell(texture_data));
                }
            }
        }
    }

    pub fn unload_texture_datas(&mut self, renderer_data: &RendererData) {
        for texture_data in self._texture_data_map.values() {
            renderer_data.destroy_texture(&(*texture_data).borrow());
        }
        self._texture_data_map.clear();
    }

    pub fn get_texture_data(&mut self, resource_name: &String) -> &RcRefCell<TextureData> {
        get_resource_data(&self._texture_data_map, resource_name, DEFAULT_TEXTURE_NAME)
    }

    // Framebuffer
    pub fn load_framebuffer_datas(&mut self, renderer_data: &RendererData) {
        let render_pass_data_create_infos = render_pass_create_info::get_render_pass_data_create_infos(renderer_data);
        for render_pass_data in self._render_pass_data_map.values() {
            let render_pass_data = render_pass_data.borrow();
            let framebuffer_name = &render_pass_data._render_pass_framebuffer_name;

            for render_pass_data_create_info in render_pass_data_create_infos.iter() {
                if *framebuffer_name == render_pass_data_create_info._render_pass_create_info_name {
                    let framebuffer_data = newRcRefCell(framebuffer::create_framebuffer_data(
                        renderer_data.get_device(),
                        render_pass_data._render_pass,
                        &render_pass_data_create_info._render_pass_frame_buffer_create_info,
                    ));
                    self._framebuffer_data_map.insert(framebuffer_name.clone(), framebuffer_data);
                    break;
                }
            }
        }
    }

    pub fn unload_framebuffer_datas(&mut self, renderer_data: &RendererData) {
        for framebuffer_data in self._framebuffer_data_map.values() {
            framebuffer::destroy_framebuffer_data(renderer_data.get_device(), &(*framebuffer_data).borrow());
        }
        self._framebuffer_data_map.clear();
    }

    pub fn get_framebuffer_data(&self, resource_name: &String) -> &RcRefCell<FramebufferData> {
        get_resource_data(&self._framebuffer_data_map, resource_name, "")
    }

    // RenderPassLoader
    pub fn load_render_pass_datas(&mut self, renderer_data: &RendererData) {
        let render_pass_data_create_infos = render_pass_create_info::get_render_pass_data_create_infos(renderer_data);
        for render_pass_data_create_info in render_pass_data_create_infos.iter() {
            let descriptor_datas = render_pass_data_create_info._pipeline_data_create_infos
                .iter()
                .map(|pipeline_data_create_info| {
                    self.get_descriptor_data(renderer_data, &render_pass_data_create_info._render_pass_create_info_name, pipeline_data_create_info)
                }).collect();
            let default_render_pass_data = render_pass::create_render_pass_data(renderer_data.get_device(), render_pass_data_create_info, &descriptor_datas);
            self._render_pass_data_map.insert(default_render_pass_data.get_render_pass_data_name().clone(), newRcRefCell(default_render_pass_data));
        }
    }

    pub fn unload_render_pass_datas(&mut self, renderer_data: &RendererData) {
        for render_pass_data in self._render_pass_data_map.values() {
            render_pass::destroy_render_pass_data(renderer_data.get_device(), &(*render_pass_data).borrow());
        }
        self._render_pass_data_map.clear()
    }

    pub fn get_render_pass_data(&self, resource_name: &String) -> &RcRefCell<RenderPassData> {
        get_resource_data(&self._render_pass_data_map, resource_name, DEFAULT_RENDER_PASS_NAME)
    }

    pub fn get_default_render_pass_data(&self) -> &RcRefCell<RenderPassData> {
        self.get_render_pass_data(&DEFAULT_RENDER_PASS_NAME.to_string())
    }

    pub fn get_render_pass_pipeline_data(&self, render_pass_pipeline_data_name: &RenderPassPipelineDataName) -> RenderPassPipelineData {
        let render_pass_data_refcell = self.get_render_pass_data(&render_pass_pipeline_data_name._render_pass_data_name);
        let render_pass_data = render_pass_data_refcell.borrow();
        let pipeline_data = render_pass_data.get_pipeline_data(&render_pass_pipeline_data_name._pipeline_data_name);
        RenderPassPipelineData {
            _render_pass_data: render_pass_data_refcell.clone(),
             _pipieline_data: pipeline_data.clone(),
        }
    }

    // Material_datas
    pub fn load_material_datas(&mut self, _renderer_data: &RendererData) {
        let material_directory = PathBuf::from(MATERIAL_FILE_PATH);
        let material_files = system::walk_directory(&material_directory.as_path(), &[EXT_MATERIAL]);
        for material_file in material_files {
            let material_name = get_unique_resource_name(&self._material_data_map, &material_directory, &material_file);
            let loaded_contents = fs::File::open(material_file).expect("Failed to create file");
            let contents: Value = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_create_info = match contents {
                Value::Object(material_create_info) => material_create_info,
                _ => panic!("material parsing error"),
            };
            let pipeline_create_infos = material_create_info.get("pipelines").unwrap().as_array().unwrap();
            let empty_object = json!({});
            let material_parameter_map = match material_create_info.get("material_parameters") {
                Some(material_parameter_map) => material_parameter_map,
                _ => &empty_object,
            };
            let render_pass_pipeline_datas: Vec<RenderPassPipelineData> = pipeline_create_infos.iter().map(|pipeline_create_info| {
                let render_pass_data_name = match pipeline_create_info.get("render_pass").unwrap() {
                    Value::String(render_pass_data_name) => render_pass_data_name,
                    _ => panic!("failed to parsing render_pass"),
                };
                let pipeline_data_name = match pipeline_create_info.get("pipeline").unwrap() {
                    Value::String(pipeline_data_name) => pipeline_data_name,
                    _ => panic!("failed to parsing pipeline"),
                };
                self.get_render_pass_pipeline_data(&RenderPassPipelineDataName {
                    _render_pass_data_name: render_pass_data_name.clone(),
                    _pipeline_data_name: pipeline_data_name.clone(),
                })
            }).collect();
            let material_data = MaterialData::create_material(&material_name, &render_pass_pipeline_datas, material_parameter_map);
            self._material_data_map.insert(material_name.clone(), newRcRefCell(material_data));
        }
    }

    pub fn unload_material_datas(&mut self, _renderer_data: &RendererData) {
        for material_data in self._material_data_map.values() {
            material_data.borrow().destroy_material();
        }
        self._material_data_map.clear();
    }

    pub fn get_material_data(&self, resource_name: &String) -> &RcRefCell<MaterialData> {
        get_resource_data(&self._material_data_map, resource_name, DEFAULT_MATERIAL_NAME)
    }

    // MaterialInstance_datas
    pub fn load_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        // materialInstanceFiles <- walkDirectory materialInstancePathBuf ["matinst"]
        // forM_ materialInstanceFiles $ \materialInstanceFile -> do
        //     materialInstanceName <- getUniqueResourceName (_materialInstanceDataMap resources) materialInstancePathBuf materialInstanceFile
        //     contents <- ByteString.readFile materialInstanceFile
        //     registMaterialInstanceData rendererData (_materialInstanceDataMap resources) materialInstanceName contents
        // where
        //     registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
        //         let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
        //             Just (Aeson.String materialDataName) = HashMap.lookup "material_name" materialInstanceCreateInfoMap
        //             Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap
        //
        //         materialData <- getMaterialData resources materialDataName
        //         let defaultMaterialParameterMap = Material._materialParameterMap materialData
        //         pipelineBindingCreateInfoList <- forM (Map.toList $ Material._renderPassPipelineDataMap materialData) $ \(key, (renderPassData, pipelineData)) -> do
        //             let descriptorDataCreateInfoList = Descriptor._descriptorDataCreateInfoList $ _descriptorData pipelineData
        //             descriptorResourceInfosList <- forM Constants.swapChainImageIndices $ \swapChainIndex -> do
        //                 descriptorResourceInfos <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
        //                     let materialParameterName = Descriptor._descriptorName' descriptorDataCreateInfo
        //                         materialParameterType = Descriptor._descriptorType' descriptorDataCreateInfo
        //                         materialParameterResourceType = Descriptor._descriptorResourceType' descriptorDataCreateInfo
        //                         maybeMaterialParameter = lookupWithDefaultMap materialParameterName materialParameterMap defaultMaterialParameterMap
        //                     case (materialParameterType, materialParameterResourceType) of
        //                         (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, Descriptor.DescriptorResourceType_UniformBuffer) -> do
        //                             uniformBufferData <- getUniformBufferData rendererData (fromText materialParameterName)
        //                             return $ Descriptor.DescriptorBufferInfo (atSwapchainIndex swapChainIndex (_descriptorBufferInfos uniformBufferData))
        //                         (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_Texture) -> do
        //                             textureData <- case maybeMaterialParameter of
        //                                 Just (Aeson.String value) -> getTextureData resources value
        //                                 otherwise -> getTextureData resources defaultTextureName
        //                             return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
        //                         (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
        //                             textureData <- getRenderTarget rendererData (fromText materialParameterName)
        //                             return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
        //                         otherwise -> return Descriptor.InvalidDescriptorInfo
        //                 return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
        //             return (renderPassData, pipelineData, descriptorResourceInfosList)
        //         materialInstance <- MaterialInstance.createMaterialInstance (getDevice rendererData) materialInstanceName materialData pipelineBindingCreateInfoList
        //         HashTable.insert (_materialInstanceDataMap resources) materialInstanceName materialInstance
    }

    pub fn unload_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        for material_instance_data in self._material_instance_data_map.values() {
            (*material_instance_data).borrow().destroy_material_instance();
        }
    }

    pub fn update_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        // flip HashTable.mapM_ (_modelDataMap resources) $ \(k, modelData) -> do
        //     materialInstances <- Model.getMaterialInstanceDataList modelData
        //     newMaterialInstances <- forM materialInstances $ \materialInstance ->
        //         getMaterialInstanceData resources (MaterialInstance._materialInstanceDataName materialInstance)
        //     Model.setMaterialInstanceDataList modelData newMaterialInstances
    }

    pub fn get_material_instance_data(&self, resource_name: &String) ->  &RcRefCell<MaterialInstanceData> {
        get_resource_data(&self._material_instance_data_map, resource_name, DEFAULT_MATERIAL_INSTANCE_NAME)
    }

    // Descriptor_datas
    pub fn get_descriptor_data(
        &mut self,
        renderer_data: &RendererData,
        render_pass_name: &String,
        pipeline_data_create_info: &PipelineDataCreateInfo
    ) -> RcRefCell<DescriptorData> {
        let descriptor_name: String = format!("{}{}", render_pass_name, pipeline_data_create_info._pipeline_data_create_info_name);
        let descriptor_data_create_infos = &pipeline_data_create_info._descriptor_data_create_infos;
        let max_descriptor_pool_count: u32 = (constants::MAX_DESCRIPTOR_POOL_ALLOC_COUNT * constants::SWAPCHAIN_IMAGE_COUNT) as u32;
        let maybe_descriptor_data = self._descriptor_data_map.get(&descriptor_name);
        match maybe_descriptor_data {
            Some(descriptor_data) => descriptor_data.clone(),
            None => {
                let descriptor_data = newRcRefCell(
                    descriptor::create_descriptor_data(renderer_data.get_device(), descriptor_data_create_infos, max_descriptor_pool_count)
                );
                self._descriptor_data_map.insert(descriptor_name, descriptor_data.clone());
                descriptor_data
            }
        }
    }

    pub fn unload_descriptor_datas(&mut self, renderer_data: &RendererData) {
        for descriptor_data in self._descriptor_data_map.values() {
            descriptor::destroy_descriptor_data(renderer_data.get_device(), &(*descriptor_data).borrow());
        }
        self._descriptor_data_map.clear();
    }
}