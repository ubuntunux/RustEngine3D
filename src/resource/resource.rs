use std::fs::{ self, File };
use std::str::FromStr;
use std::io::prelude::*;
use std::path::{ Path, PathBuf };
use std::collections::HashMap;
use byteorder::{ LittleEndian };

use serde_json::{ self, Value, json };
use bincode;
use image::{ self, GenericImageView, };
use ash::{ vk };
use nalgebra::Vector2;

use crate::application::SceneManagerData;
use crate::constants;
use crate::resource::font_loader;
use crate::resource::collada_loader::Collada;
use crate::resource::obj_loader::WaveFrontOBJ;
use crate::resource::render_pass_create_info;
use crate::resource::texture_generator;
use crate::renderer::font::{ self, FontDataCreateInfo, FontData };
use crate::renderer::mesh::{ MeshData, MeshDataCreateInfo };
use crate::renderer::model::{ ModelData };
use crate::renderer::material::{ self, MaterialData };
use crate::renderer::material_instance::{ self, MaterialInstanceData };
use crate::renderer::renderer::{ RendererData };
use crate::renderer::render_target::{ RenderTargetType };
use crate::renderer::shader_buffer_datas::{ ShaderBufferDataType };
use crate::vulkan_context::descriptor::{ self, DescriptorData, DescriptorResourceType, DescriptorResourceInfo };
use crate::vulkan_context::framebuffer::{self, FramebufferData };
use crate::vulkan_context::geometry_buffer::{ self, GeometryData };
use crate::vulkan_context::render_pass::{
    self,
    PipelineDataCreateInfo,
    RenderPassData,
    RenderPassPipelineData,
};
use crate::vulkan_context::texture::{ TextureData, TextureCreateInfo };
use crate::utilities::system::{ self, RcRefCell, newRcRefCell };
use byteorder::ReadBytesExt;

const GATHER_ALL_FILES: bool = false;
const USE_JSON_FOR_MESH: bool = false;
const LOAD_FROM_EXTERNAL_FOR_MESH: bool = true;

const FONT_SOURCE_FILE_PATH: &str = "resource/externals/fonts";
const FONT_FILE_PATH: &str = "resource/fonts";
const FONT_TEXTURE_FILE_PATH: &str = "resource/externals/textures/fonts";
const MATERIAL_FILE_PATH: &str = "resource/materials";
const MATERIAL_INSTANCE_FILE_PATH: &str = "resource/material_instances";
const MESH_SOURCE_FILE_PATH: &str = "resource/externals/meshes";
const MESH_FILE_PATH: &str = "resource/meshes";
const MODEL_FILE_PATH: &str = "resource/models";
const TEXTURE_SOURCE_FILE_PATH: &str = "resource/externals/textures";
const TEXTURE_FILE_PATH: &str = "resource/textures";

const FONT_SOURCE_EXTS: [&str; 1] = ["ttf"];
const EXT_FONT: &str = "font";
const EXT_OBJ: &str = "obj";
const EXT_COLLADA: &str = "dae";
const MESH_SOURCE_EXTS: [&str; 2] = [EXT_OBJ, EXT_COLLADA];
const EXT_JSON: &str = "json";
const EXT_MATERIAL: &str = "mat";
const EXT_MATERIAL_INSTANCE: &str = "matinst";
const EXT_MESH: &str = "mesh";
const EXT_MODEL: &str = "model";
const IMAGE_SOURCE_EXTS: [&str; 4] = ["jpg", "png", "tga", "bmp"];
const EXT_TEXTURE_CUBE: &str = "cube";
const EXT_TEXTURE_2D_ARRAY: &str = "2darray";
const EXT_TEXTURE_3D: &str = "3d";
const EXT_TEXTURE: [&str; 1] = ["texture"];

const DEFAULT_FONT_NAME: &str = "NanumBarunGothic_Basic_Latin";
const DEFAULT_MESH_NAME: &str = "quad";
const DEFAULT_MODEL_NAME: &str = "quad";
const DEFAULT_TEXTURE_NAME: &str = "common/default";
const DEFAULT_MATERIAL_NAME: &str = "render_pass_static_opaque";
const DEFAULT_MATERIAL_INSTANCE_NAME: &str = "default";
const DEFAULT_RENDER_PASS_NAME: &str = "render_pass_static_opaque";

pub type ResourceDataMap<T> = HashMap<String, RcRefCell<T>>;
pub type FramebufferDatasMap = ResourceDataMap<FramebufferData>;
pub type MaterialDataMap = ResourceDataMap<material::MaterialData>;
pub type MaterialInstanceDataMap = ResourceDataMap<material_instance::MaterialInstanceData>;
pub type SceneManagerDataMap = ResourceDataMap<SceneManagerData>;
pub type FontDataMap = ResourceDataMap<FontData>;
pub type MeshDataMap = ResourceDataMap<MeshData>;
pub type ModelDataMap = ResourceDataMap<ModelData>;
pub type TextureDataMap = ResourceDataMap<TextureData>;
pub type RenderPassDataMap = ResourceDataMap<RenderPassData>;
pub type DescriptorDataMap = ResourceDataMap<descriptor::DescriptorData>;
pub type MetaDataMap = ResourceDataMap<MetaData>;
type LoadImageInfoType = (u32, u32, u32, Vec<u8>, vk::Format);

// TODO: ImageSamplerMap


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

#[derive(Clone)]
pub struct Resources {
    pub _resource_filenames: Vec<PathBuf>,
    pub _meta_data_map: MetaDataMap,
    pub _font_data_map: FontDataMap,
    pub _mesh_data_map: MeshDataMap,
    pub _model_data_map: ModelDataMap,
    pub _texture_data_map: TextureDataMap,
    pub _framebuffer_datas_map: FramebufferDatasMap,
    pub _render_pass_data_map: RenderPassDataMap,
    pub _material_data_map: MaterialDataMap,
    pub _material_instance_data_map: MaterialInstanceDataMap,
    pub _descriptor_data_map: DescriptorDataMap
}

pub fn create_resources() -> RcRefCell<Resources> {
    newRcRefCell(Resources {
        _resource_filenames: Vec::new(),
        _meta_data_map: MetaDataMap::new(),
        _font_data_map: FontDataMap::new(),
        _mesh_data_map: MeshDataMap::new(),
        _model_data_map: ModelDataMap::new(),
        _texture_data_map: TextureDataMap::new(),
        _framebuffer_datas_map: FramebufferDatasMap::new(),
        _render_pass_data_map: RenderPassDataMap::new(),
        _material_data_map: MaterialDataMap::new(),
        _material_instance_data_map: MaterialInstanceDataMap::new(),
        _descriptor_data_map: DescriptorDataMap::new()
    })
}

fn get_resource_data_must<'a, T>(resource_data_map: &'a ResourceDataMap<T>, resource_name: &str) -> &'a RcRefCell<T> {
    resource_data_map.get(resource_name).unwrap()
}

fn get_resource_data<'a, T>(resource_data_map: &'a ResourceDataMap<T>, resource_name: &str, default_resource_name: &str) -> &'a RcRefCell<T> {
    let maybe_data = resource_data_map.get(resource_name);
    match maybe_data {
        None => {
            log::error!("not found texture: {}", resource_name);
            resource_data_map.get(default_resource_name).unwrap()
        },
        _ => maybe_data.unwrap(),
    }
}

fn get_resource_name_from_file_path(resource_root_path: &PathBuf, resource_file_path: &PathBuf) -> String {
    let mut resource_name = PathBuf::from(resource_file_path.parent().unwrap());
    resource_name.push(resource_file_path.file_stem().unwrap());
    let resource_name = String::from(system::get_relative_path(resource_root_path, &resource_name).to_str().unwrap());
    resource_name.replace("\\", "/")
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
    pub fn initialize_resources(&mut self, renderer_data: &mut RendererData) {
        log::info!("initialize_resources");
        self.load_resource_filenames();
        self.load_texture_datas(renderer_data);
        self.load_font_datas(renderer_data);
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.load_mesh_datas(renderer_data);
        self.load_model_datas(renderer_data);
    }

    pub fn destroy_resources(&mut self, renderer_data: &mut RendererData) {
        log::info!("destroy_resources");
        self.unload_model_datas(renderer_data);
        self.unload_mesh_datas(renderer_data);
        self.unload_material_instance_datas(renderer_data);
        self.unload_material_datas(renderer_data);
        self.unload_framebuffer_datas(renderer_data);
        self.unload_render_pass_datas(renderer_data);
        self.unload_font_datas(renderer_data);
        self.unload_texture_datas(renderer_data);
        self.unload_descriptor_datas(renderer_data);
    }

    // GraphicsDatas
    pub fn load_graphics_datas(&mut self, renderer_data: &mut RendererData) {
        log::info!("load_graphics_datas");
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.update_material_instance_datas();
    }

    pub fn unload_graphics_datas(&mut self, renderer_data: &mut RendererData) {
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

    pub fn load_resource_filenames(&mut self) {
        let loaded_contents = system::load(&Path::new("resource/resources.txt"));
        let contents: String = String::from_utf8(loaded_contents.into_inner()).unwrap();
        for content in contents.split("\n") {
            self._resource_filenames.push(PathBuf::from(content));
        }
    }

    pub fn collect_resources_inner(&self, _dir: &Path, extensions: &[&str]) -> Vec<PathBuf> {
        let mut out_resources: Vec<PathBuf> = Vec::new();
        for resource_filename in self._resource_filenames.iter() {
            let ext = resource_filename.extension();
            if extensions.is_empty() || (ext.is_some() && extensions.contains(&ext.unwrap().to_str().unwrap())) {
                out_resources.push(PathBuf::from(resource_filename));
            }
        }
        out_resources
    }

    pub fn collect_resources(&self, dir: &Path, extensions: &[&str]) -> Vec<PathBuf> {
        #[cfg(target_os = "android")]
        return self.collect_resources_inner(dir, extensions);
        #[cfg(not(target_os = "android"))]
        return system::walk_directory(dir, extensions);
    }

    // FontData
    pub fn get_font_data_create_info(
        &self,
        font_directory: &PathBuf,
        font_texture_directory: &PathBuf,
        font_name: &String,
        font_source_file: &PathBuf,
        range_min: u32,
        range_max: u32
    ) -> FontDataCreateInfo {
        let mut font_file_path: PathBuf = font_directory.clone();
        font_file_path.push(font_name);
        font_file_path.set_extension(EXT_FONT);
        fs::create_dir_all(font_file_path.parent().unwrap()).expect("Failed to create directories.");

        let mut font_texture_file_path: PathBuf = font_texture_directory.clone();
        font_texture_file_path.push(&font_name);
        font_texture_file_path.set_extension("png");
        fs::create_dir_all(font_texture_file_path.parent().unwrap()).expect("Failed to create directories.");

        let mut write_file = File::create(&font_file_path).expect("Failed to create file");
        let font_data_create_info = font_loader::get_font_data_create_info(
            &font_source_file,
            font::FONT_SIZE as f32,
            font::FONT_PADDING as f32,
            &font_name,
            &font_texture_file_path,
            range_min,
            range_max
        );
        let write_contents: String = serde_json::to_string(&font_data_create_info).expect("Failed to serialize.");
        write_file.write(write_contents.as_bytes()).expect("Failed to write");
        font_data_create_info
    }

    pub fn load_font_datas(&mut self, renderer_data: &RendererData) {
        let mut unicode_blocks: HashMap<String, (u32, u32)> = HashMap::new();
        unicode_blocks.insert(String::from("Basic_Latin"), (0x20, 0x7F)); // 32 ~ 127
        //unicode_blocks.insert(String::from("Hangul_Syllables"), (0xAC00, 0xD7AF)); // 44032 ~ 55215

        let font_directory = PathBuf::from(FONT_FILE_PATH);
        let font_texture_directory = PathBuf::from(FONT_TEXTURE_FILE_PATH);
        let font_files = self.collect_resources(font_directory.as_path(), &[EXT_FONT]);
        let mut font_file_map: HashMap<String, PathBuf> = HashMap::new();
        for font_file in font_files.iter() {
            let font_name = get_resource_name_from_file_path(&font_directory, &font_file);
            font_file_map.insert(font_name, font_file.clone());
        }

        let font_source_directory = PathBuf::from(FONT_SOURCE_FILE_PATH);
        let font_source_files: Vec<PathBuf> = self.collect_resources(&font_source_directory, &FONT_SOURCE_EXTS);
        for font_source_file in font_source_files {
            for (unicode_block_key, (range_min, range_max))  in unicode_blocks.iter() {
                let font_name = get_unique_resource_name(&self._font_data_map, &font_source_directory, &font_source_file);
                let font_data_name = format!("{}_{}", font_name, unicode_block_key);
                let font_texture_name = format!("fonts/{}_{}", font_name, unicode_block_key);
                let font_data_create_info = match font_file_map.get(&font_data_name) {
                    Some(font_file) => {
                        let loaded_contents = system::load(font_file);
                        let font_data_create_info: FontDataCreateInfo = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
                        #[cfg(target_os = "android")]
                        let check_font_texture_file_exists = false;
                        #[cfg(not(target_os = "android"))]
                        let check_font_texture_file_exists = true;
                        if check_font_texture_file_exists && false == font_data_create_info._texture_file_path.is_file() {
                            self.get_font_data_create_info(&font_directory, &font_texture_directory, &font_data_name, &font_source_file, *range_min, *range_max)
                        } else {
                            font_data_create_info
                        }
                    },
                    _ => {
                        self.get_font_data_create_info(&font_directory, &font_texture_directory, &font_data_name, &font_source_file, *range_min, *range_max)
                    },
                };

                if false == self.has_texture_data(&font_texture_name) {
                    // regist font texture
                    let (image_width, image_height, image_layers, image_data, image_format): LoadImageInfoType = Resources::load_image_data(&font_data_create_info._texture_file_path);
                    assert_ne!(vk::Format::UNDEFINED, image_format);
                    let texture_create_info = TextureCreateInfo {
                        _texture_name: font_texture_name.clone(),
                        _texture_width: image_width,
                        _texture_height: image_height,
                        _texture_layers: image_layers,
                        _texture_format: image_format,
                        _texture_view_type: vk::ImageViewType::TYPE_2D,
                        _texture_initial_datas: image_data,
                        _enable_mipmap: false,
                        _enable_anisotropy: false,
                        ..Default::default()
                    };
                    let texture_data = newRcRefCell(renderer_data.create_texture(&texture_create_info));
                    self.regist_texture_data(font_texture_name.clone(), texture_data);
                };
                let texture_data = self.get_texture_data(&font_texture_name);

                // regist font data
                let font_data = newRcRefCell(FontData {
                    _font_data_name: font_data_create_info._font_data_name.clone(),
                    _range_min: font_data_create_info._range_min,
                    _range_max: font_data_create_info._range_max,
                    _text_count: font_data_create_info._text_count,
                    _count_of_side: font_data_create_info._count_of_side,
                    _font_size: Vector2::new(
                        (texture_data.borrow()._image_width / font_data_create_info._count_of_side) as f32,
                        (texture_data.borrow()._image_height / font_data_create_info._count_of_side) as f32
                    ),
                    _texture: texture_data.clone(),
                });
                self._font_data_map.insert(font_data_name, font_data);
            }
        }
    }

    pub fn unload_font_datas(&mut self, _renderer_data: &RendererData) {
        self._font_data_map.clear();
    }

    pub fn has_font_data(&self, resource_name: &str) -> bool {
        self._font_data_map.contains_key(resource_name)
    }

    pub fn get_default_font_data(&self) -> &RcRefCell<FontData> {
        get_resource_data_must(&self._font_data_map, DEFAULT_FONT_NAME)
    }

    pub fn get_font_data(&self, resource_name: &str) -> &RcRefCell<FontData> {
        get_resource_data_must(&self._font_data_map, resource_name)
    }

    // ModelData
    pub fn load_model_datas(&mut self, _renderer_data: &RendererData) {
        let model_directory = PathBuf::from(MODEL_FILE_PATH);
        let model_files: Vec<PathBuf> = self.collect_resources(&model_directory, &[EXT_MODEL]);
        for model_file in model_files {
            let model_name = get_unique_resource_name(&self._model_data_map, &model_directory, &model_file);
            let loaded_contents = system::load(&model_file);
            let contents = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let model_create_info = match contents {
                Value::Object(model_create_info) => model_create_info,
                _ => panic!("model parsing error.")
            };
            let material_instance_names = match model_create_info.get("material_instances").unwrap() {
                Value::Array(material_instance_names) => material_instance_names,
                _ => panic!("model material_instance_names parsing error.")
            };
            let material_instance_count = material_instance_names.len();
            let mesh_name = match model_create_info.get("mesh").unwrap() {
                Value::String(mesh_name) => mesh_name,
                _ => panic!("failed to parsing mesh_name"),
            };
            let mesh_data = self.get_mesh_data(mesh_name.as_str());
            let geometry_data_count = mesh_data.borrow().get_geometry_data_count();
            let material_instance_datas: Vec<RcRefCell<MaterialInstanceData>> = (0..geometry_data_count).map(|index| {
                if index < material_instance_count {
                    match &material_instance_names[index] {
                        Value::String(material_instance_name) => {
                            self.get_material_instance_data(&material_instance_name).clone()
                        },
                        _ => panic!("failed to parsing material_instance_names"),
                    }
                } else {
                    self.get_material_instance_data(DEFAULT_MATERIAL_INSTANCE_NAME).clone()
                }
            }).collect();
            let model_data = ModelData::new_model_data(&model_name, mesh_data.clone(), material_instance_datas);
            self._model_data_map.insert(model_name.clone(), newRcRefCell(model_data));
        }
    }

    pub fn unload_model_datas(&mut self, _renderer_data: &RendererData) {
        for model_data in self._model_data_map.values() {
            model_data.borrow().destroy_model_data();
        }
        self._model_data_map.clear();
    }

    pub fn has_model_data(&self, resource_name: &str) -> bool {
        self._model_data_map.contains_key(resource_name)
    }

    pub fn get_model_data(&self, resource_name: &str) -> &RcRefCell<ModelData> {
        get_resource_data(&self._model_data_map, resource_name, DEFAULT_MODEL_NAME)
    }

    // Mesh Loader
    pub fn regist_mesh_data(
        &mut self,
        renderer_data: &RendererData,
        mesh_name: &String,
        mesh_data_create_info: MeshDataCreateInfo,
    ) {
        let mut geometry_datas: Vec<RcRefCell<GeometryData>> = Vec::new();
        for (i, geometry_create_info) in mesh_data_create_info._geometry_create_infos.iter().enumerate() {
            let geomtery_name: String = format!("{}_{}", mesh_name, i);
            let geometry_data = renderer_data.create_geometry_buffer(&geomtery_name, geometry_create_info);
            geometry_datas.push(newRcRefCell(geometry_data));
        }

        let mesh_data = newRcRefCell(MeshData::create_mesh_data(&mesh_name, mesh_data_create_info, geometry_datas));
        self._mesh_data_map.insert(mesh_name.clone(), mesh_data.clone());
    }

    pub fn load_mesh_datas(&mut self, renderer_data: &RendererData) {
        self.regist_mesh_data(renderer_data, &String::from("quad"), geometry_buffer::quad_mesh_create_info());
        self.regist_mesh_data(renderer_data, &String::from("cube"), geometry_buffer::cube_mesh_create_info());
        let mesh_directory = PathBuf::from(MESH_FILE_PATH);
        let mesh_source_directory = PathBuf::from(MESH_SOURCE_FILE_PATH);
        let resource_ext = if USE_JSON_FOR_MESH { EXT_JSON } else { EXT_MESH };
        let mesh_files = self.collect_resources(mesh_directory.as_path(), &[resource_ext]);
        let mut mesh_file_map: HashMap<String, PathBuf> = HashMap::new();
        for mesh_file in mesh_files.iter() {
            let mesh_name = get_resource_name_from_file_path(&mesh_directory, &mesh_file);
            mesh_file_map.insert(mesh_name, mesh_file.clone());
        }
        let mesh_source_files = self.collect_resources(mesh_source_directory.as_path(), &MESH_SOURCE_EXTS);
        for mesh_source_file in mesh_source_files {
            let mesh_name = get_unique_resource_name(&self._mesh_data_map, &mesh_source_directory, &mesh_source_file);
            let src_file_ext: String = String::from(mesh_source_file.extension().unwrap().to_str().unwrap());
            let mesh_data_create_info = match (LOAD_FROM_EXTERNAL_FOR_MESH, mesh_file_map.get(&mesh_name)) {
                (false, Some(mesh_file)) => {
                    // Load mesh
                    let loaded_contents = system::load(mesh_file);
                    if USE_JSON_FOR_MESH {
                        serde_json::from_reader(loaded_contents).expect("Failed to deserialize.")
                    } else {
                        bincode::deserialize_from(loaded_contents).unwrap()
                    }
                },
                _ => {
                    // Convert to mesh from source
                    let mesh_data_create_info = match src_file_ext.as_str() {
                        EXT_OBJ => WaveFrontOBJ::get_mesh_data_create_infos(&mesh_source_file),
                        EXT_COLLADA => Collada::get_mesh_data_create_infos(&mesh_source_file),
                        _ => panic!("error")
                    };

                    // Save mesh
                    if false == LOAD_FROM_EXTERNAL_FOR_MESH {
                        let mut mesh_file_path: PathBuf = mesh_directory.clone();
                        mesh_file_path.push(&mesh_name);
                        mesh_file_path.set_extension(resource_ext);
                        fs::create_dir_all(mesh_file_path.parent().unwrap()).expect("Failed to create directories.");
                        let mut write_file = File::create(mesh_file_path).expect("Failed to create file");
                        if USE_JSON_FOR_MESH {
                            let write_contents: String = serde_json::to_string(&mesh_data_create_info).expect("Failed to serialize.");
                            write_file.write(write_contents.as_bytes()).expect("Failed to write");
                        } else {
                            let write_contents: Vec<u8> = bincode::serialize(&mesh_data_create_info).unwrap();
                            write_file.write(&write_contents).expect("Failed to write");
                        }
                    }

                    mesh_data_create_info
                },
            };
            self.regist_mesh_data(renderer_data, &mesh_name, mesh_data_create_info);
        }
    }

    pub fn unload_mesh_datas(&mut self, renderer_data: &RendererData) {
        for mesh_data in self._mesh_data_map.values() {
            for geometry_data in (*mesh_data).borrow().get_geomtry_datas() {
                renderer_data.destroy_geomtry_buffer(&geometry_data.borrow());
            }
        }
        self._mesh_data_map.clear();
    }

    pub fn has_mesh_data(&self, resource_name: &str) -> bool {
        self._mesh_data_map.contains_key(resource_name)
    }

    pub fn get_mesh_data(&self, resource_name: &str) -> &RcRefCell<MeshData> {
        get_resource_data(&self._mesh_data_map, resource_name, DEFAULT_MESH_NAME)
    }

    // TextureLoader
    pub fn load_image_data(texture_file: &PathBuf) -> LoadImageInfoType {
        let loaded_contents = system::load(texture_file);
        let image_format = image::ImageFormat::from_path(texture_file);
        let image_file = image::load(loaded_contents, image_format.unwrap());
        if image_file.is_err() {
            log::error!("load_image_data error: {:?}", texture_file);
            return (0, 0, 0, Vec::new(), vk::Format::UNDEFINED);
        }
        let dynamic_image: image::DynamicImage = image_file.unwrap();
        let (image_width, image_height) = dynamic_image.dimensions();
        let image_layer = 1;
        let (image_data_raw, image_format): (Vec<u8>, vk::Format) = match dynamic_image {
            image::DynamicImage::ImageRgba8(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageRgb8(_) => (dynamic_image .to_rgba8().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageLuma8(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageRgb16(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R16G16B16A16_UNORM),
            _ => {
                log::error!("Unkown format: {:?}", texture_file);
                (Vec::new(), vk::Format::UNDEFINED)
            }
        };
        (image_width, image_height, image_layer, image_data_raw, image_format)
    }

    pub fn load_image_datas(texture_files: &Vec<PathBuf>) -> LoadImageInfoType {
        let mut image_width: u32 = 0;
        let mut image_height: u32 = 0;
        let mut image_format: vk::Format = vk::Format::UNDEFINED;
        let mut image_datas: Vec<Vec<u8>> = Vec::new();
        for texture_file in texture_files.iter() {
            let (width, height, _layer, image_data, format): LoadImageInfoType = Resources::load_image_data(texture_file);
            image_width = width;
            image_height = height;
            image_format = format;
            image_datas.push(image_data.clone());
        }
        let image_datas = image_datas.concat();
        (image_width, image_height, texture_files.len() as u32, image_datas, image_format)
    }

    pub fn regist_texture_data(&mut self, texture_data_name: String, texture_data: RcRefCell<TextureData>) {
        self._texture_data_map.insert(texture_data_name, texture_data);
    }

    pub fn load_texture_datas(&mut self, renderer_data: &RendererData) {
        let texture_datas: Vec<TextureData> = texture_generator::generate_textures(renderer_data);
        for texture_data in texture_datas {
            self.regist_texture_data(texture_data._texture_data_name.clone(), newRcRefCell(texture_data));
        }

        let texture_directory = PathBuf::from(TEXTURE_FILE_PATH);
        let texture_source_directory = PathBuf::from(TEXTURE_SOURCE_FILE_PATH);
        let mut combined_textures_name_map: HashMap<PathBuf, String> = HashMap::new();
        let mut combined_texture_files_map: HashMap<String, Vec::<PathBuf>> = HashMap::new();
        let mut combined_texture_types_map: HashMap<String, vk::ImageViewType> = HashMap::new();

        // generate necessary texture datas
        #[cfg(not(target_os = "android"))]
        texture_generator::generate_images(&texture_source_directory);

        // combined texture list
        let combined_texture_files = self.collect_resources(texture_source_directory.as_path(), &[EXT_TEXTURE_2D_ARRAY, EXT_TEXTURE_3D, EXT_TEXTURE_CUBE]);
        for texture_src_file in combined_texture_files.iter() {
            let directory = texture_src_file.parent().unwrap();
            let texture_data_name = get_resource_name_from_file_path(&texture_source_directory, &texture_src_file);
            let loaded_contents = system::load(texture_src_file);
            let contents = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let ext = texture_src_file.extension().unwrap();
            let mut texture_file_names: Vec<String> = Vec::new();
            let image_view_type = if EXT_TEXTURE_CUBE == ext {
                if let Value::Object(texture_cube_faces) = contents {
                    for face in constants::CUBE_TEXTURE_FACES.iter() {
                        if let Value::String(face_texture_name) = texture_cube_faces.get(*face).unwrap() {
                            texture_file_names.push(face_texture_name.clone());
                        }
                    }
                }
                vk::ImageViewType::CUBE
            } else if EXT_TEXTURE_2D_ARRAY == ext || EXT_TEXTURE_3D == ext {
                if let Value::Array(texture_file_list) = contents {
                    for texture_file in texture_file_list {
                        if let Value::String(texture_file) = texture_file {
                            texture_file_names.push(texture_file);
                        }
                    }
                }

                if EXT_TEXTURE_2D_ARRAY == ext {
                    vk::ImageViewType::TYPE_2D_ARRAY
                } else if EXT_TEXTURE_3D == ext {
                    vk::ImageViewType::TYPE_3D
                } else {
                    panic!("Not implementes.");
                }
            } else {
                panic!("Not implementes.");
            };

            let mut texture_files: Vec<PathBuf> = Vec::new();
            for texture_file_name in texture_file_names {
                let mut texture_file: PathBuf = PathBuf::from(directory);
                texture_file.push(PathBuf::from(texture_file_name).file_name().unwrap());
                texture_files.push(texture_file.clone());
                combined_textures_name_map.insert(texture_file.clone(), texture_data_name.clone());
            }
            combined_texture_files_map.insert(texture_data_name.clone(), texture_files);
            combined_texture_types_map.insert(texture_data_name.clone(), image_view_type);
        }

        // load texture from external files
        let texture_src_files = self.collect_resources(texture_source_directory.as_path(), &IMAGE_SOURCE_EXTS);
        for texture_src_file in texture_src_files.iter() {
            let combined_texture_name = combined_textures_name_map.get(texture_src_file);
            let texture_data_name: String = match combined_texture_name {
                Some(combined_texture_name) => combined_texture_name.clone(),
                _ => get_resource_name_from_file_path(&texture_source_directory, &texture_src_file)
            };

            let existing_resource_data: Option<&RcRefCell<TextureData>> = self._texture_data_map.get(&texture_data_name);
            if existing_resource_data.is_none() {
                let (image_view_type, (image_width, image_height, image_layers, image_data, image_format)): (vk::ImageViewType, LoadImageInfoType) =
                    if combined_texture_name.is_some() {
                        let combined_texture_files = combined_texture_files_map.get(texture_data_name.as_str()).unwrap();
                        let image_view_type = combined_texture_types_map.get(texture_data_name.as_str()).unwrap();
                        (*image_view_type, Resources::load_image_datas(&combined_texture_files))
                    } else {
                        (vk::ImageViewType::TYPE_2D, Resources::load_image_data(texture_src_file))
                    };
                if vk::Format::UNDEFINED != image_format {
                    let texture_create_info = TextureCreateInfo {
                        _texture_name: texture_data_name.clone(),
                        _texture_width: image_width,
                        _texture_height: image_height,
                        _texture_layers: image_layers,
                        _texture_format: image_format,
                        _texture_view_type: image_view_type,
                        _texture_initial_datas: image_data,
                        _enable_mipmap: true,
                        _enable_anisotropy: false,
                        ..Default::default()
                    };
                    let texture_data = renderer_data.create_texture(&texture_create_info);
                    self._texture_data_map.insert(texture_data_name, newRcRefCell(texture_data));
                }
            }
        }

        // read binary texture data
        let texture_files = self.collect_resources(texture_directory.as_path(), &EXT_TEXTURE);
        for texture_file in texture_files.iter() {
            let texture_data_name = get_resource_name_from_file_path(&texture_directory, texture_file);
            let mut loaded_contents = system::load(&texture_file);
            let image_view_type: vk::ImageViewType = vk::ImageViewType::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap() as i32);
            let image_width: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_height: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_depth: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_format: vk::Format = vk::Format::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap() as i32);
            let enable_mipmap: bool = if 0 != loaded_contents.read_i32::<LittleEndian>().unwrap() { true } else { false };
            let min_filter: vk::Filter = vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap() as i32);
            let mag_filter: vk::Filter = vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap() as i32);
            let wrap: vk::SamplerAddressMode = vk::SamplerAddressMode::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap() as i32);
            let data_bytes: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let mut image_data: Vec<u8> = Vec::new();
            loaded_contents.read_to_end(&mut image_data).expect("Failed to read datas.");
            assert_eq!(data_bytes as usize, image_data.len());

            let texture_create_info = TextureCreateInfo {
                _texture_name: texture_data_name.clone(),
                _texture_width: image_width as u32,
                _texture_height: image_height as u32,
                _texture_layers: image_depth as u32,
                _texture_format: image_format,
                _texture_min_filter: min_filter,
                _texture_mag_filter: mag_filter,
                _texture_wrap_mode: wrap,
                _texture_view_type: image_view_type,
                _texture_initial_datas: image_data,
                _enable_mipmap: enable_mipmap,
                _enable_anisotropy: false,
                ..Default::default()
            };
            let texture_data = renderer_data.create_texture(&texture_create_info);
            self._texture_data_map.insert(texture_data_name, newRcRefCell(texture_data));
        }
    }

    pub fn unload_texture_datas(&mut self, renderer_data: &RendererData) {
        for texture_data in self._texture_data_map.values() {
            renderer_data.destroy_texture(&(*texture_data).borrow());
        }
        self._texture_data_map.clear();
    }

    pub fn has_texture_data(&self, resource_name: &str) -> bool {
        self._texture_data_map.contains_key(resource_name)
    }

    pub fn get_texture_data(&self, resource_name: &str) -> &RcRefCell<TextureData> {
        get_resource_data(&self._texture_data_map, resource_name, DEFAULT_TEXTURE_NAME)
    }

    // Framebuffer
    pub fn load_framebuffer_datas(&mut self, renderer_data: &RendererData) {
        let render_pass_data_create_infos = render_pass_create_info::get_render_pass_data_create_infos(renderer_data);
        for render_pass_data in self._render_pass_data_map.values() {
            let render_pass_data = render_pass_data.borrow();
            for render_pass_data_create_info in render_pass_data_create_infos.iter() {
                if render_pass_data_create_info._render_pass_create_info_name == render_pass_data._render_pass_data_name {
                    if render_pass_data_create_info._render_pass_framebuffer_create_info.is_valid() {
                        let framebuffer_data = framebuffer::create_framebuffer_data(
                            renderer_data.get_device(),
                            render_pass_data._render_pass,
                            render_pass_data._render_pass_data_name.as_str(),
                            render_pass_data_create_info._render_pass_framebuffer_create_info.clone(),
                        );
                        self._framebuffer_datas_map.insert(render_pass_data._render_pass_data_name.clone(), newRcRefCell(framebuffer_data));
                        break;
                    }
                }
            }
        }
    }

    pub fn unload_framebuffer_datas(&mut self, renderer_data: &RendererData) {
        for framebuffer_data in self._framebuffer_datas_map.values() {
            framebuffer::destroy_framebuffer_data(renderer_data.get_device(), &framebuffer_data.borrow());
        }
        self._framebuffer_datas_map.clear();
    }

    pub fn has_framebuffer_data(&self, resource_name: &str) -> bool {
        self._framebuffer_datas_map.contains_key(resource_name)
    }

    pub fn get_framebuffer_data(&self, resource_name: &str) -> &RcRefCell<FramebufferData> {
        get_resource_data_must(&self._framebuffer_datas_map, resource_name)
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

    pub fn has_render_pass_data(&self, resource_name: &str) -> bool {
        self._render_pass_data_map.contains_key(resource_name)
    }

    pub fn get_render_pass_data(&self, resource_name: &str) -> &RcRefCell<RenderPassData> {
        get_resource_data_must(&self._render_pass_data_map, resource_name)
    }

    pub fn get_default_render_pass_data(&self) -> &RcRefCell<RenderPassData> {
        self.get_render_pass_data(DEFAULT_RENDER_PASS_NAME)
    }

    pub fn get_render_pass_pipeline_data(&self, render_pass_data_name: &str, pipeline_data_name: &str) -> RenderPassPipelineData {
        let render_pass_data_refcell = self.get_render_pass_data(render_pass_data_name);
        let render_pass_data = render_pass_data_refcell.borrow();
        let pipeline_data = render_pass_data.get_pipeline_data(pipeline_data_name);
        RenderPassPipelineData {
            _render_pass_data: render_pass_data_refcell.clone(),
            _pipeline_data: pipeline_data.clone(),
        }
    }

    // Material_datas
    pub fn load_material_datas(&mut self, _renderer_data: &RendererData) {
        let material_directory = PathBuf::from(MATERIAL_FILE_PATH);
        let material_files = self.collect_resources(&material_directory.as_path(), &[EXT_MATERIAL]);
        for material_file in material_files {
            let material_name = get_unique_resource_name(&self._material_data_map, &material_directory, &material_file);
            let loaded_contents = system::load(material_file);
            let contents: Value = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_create_info = match contents {
                Value::Object(material_create_info) => material_create_info,
                _ => panic!("material parsing error"),
            };
            let pipeline_create_infos = material_create_info.get("pipelines").unwrap().as_array().unwrap();
            let empty_object = json!({});
            let material_parameters = match material_create_info.get("material_parameters") {
                Some(material_parameters) => material_parameters,
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
                self.get_render_pass_pipeline_data(render_pass_data_name.as_str(), pipeline_data_name.as_str())
            }).collect();
            let material_data = MaterialData::create_material(&material_name, &render_pass_pipeline_datas, material_parameters);
            self._material_data_map.insert(material_name.clone(), newRcRefCell(material_data));
        }
    }

    pub fn unload_material_datas(&mut self, _renderer_data: &RendererData) {
        for material_data in self._material_data_map.values() {
            material_data.borrow().destroy_material();
        }
        self._material_data_map.clear();
    }

    pub fn has_material_data(&self, resource_name: &str) -> bool {
        self._material_data_map.contains_key(resource_name)
    }

    pub fn get_material_data(&self, resource_name: &str) -> &RcRefCell<MaterialData> {
        get_resource_data_must(&self._material_data_map, resource_name)
    }

    // MaterialInstance_datas
    pub fn load_material_instance_datas(&mut self, renderer_data: &RendererData) {
        let material_instance_directory = PathBuf::from(MATERIAL_INSTANCE_FILE_PATH);
        let material_instance_files = self.collect_resources(&material_instance_directory, &[EXT_MATERIAL_INSTANCE]);
        for material_instance_file in material_instance_files.iter() {
            let material_instance_name = get_unique_resource_name(&self._material_instance_data_map, &material_instance_directory, &material_instance_file);
            let loaded_contents = system::load(material_instance_file);
            let contents: Value = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_instance_create_info = match contents {
                Value::Object(material_instance_create_info) => material_instance_create_info,
                _ => panic!("material instance parsing error"),
            };
            let material_data_name = match material_instance_create_info.get("material_name").unwrap() {
                Value::String(material_data_name) => material_data_name,
                _ => panic!("material name parsing error")
            };
            let material_parameter_map = match material_instance_create_info.get("material_parameters").unwrap() {
                Value::Object(material_parameter_map) => material_parameter_map,
                _ => panic!("material parameters parsing error")
            };
            let material_data = self.get_material_data(material_data_name.as_str()).clone();
            let default_material_parameter_map = &material_data.borrow()._material_parameter_map;
            let pipeline_bind_create_infos = material_data.borrow()._render_pass_pipeline_data_map.iter().map(|(_key, render_pass_pipeline_data)| {
                let descriptor_data_create_infos = &render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data._descriptor_data_create_infos;
                let descriptor_resource_infos_list = constants::SWAPCHAIN_IMAGE_INDICES.iter().map(|swapchain_index| {
                    let descriptor_resource_infos = descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
                        let material_parameter_name = &descriptor_data_create_info._descriptor_name;
                        let material_parameter_resource_type = &descriptor_data_create_info._descriptor_resource_type;
                        let maybe_material_parameter = match material_parameter_map.get(material_parameter_name) {
                            None => default_material_parameter_map.get(material_parameter_name),
                            value => value,
                        };
                        let descriptor_resource_info = match material_parameter_resource_type {
                            DescriptorResourceType::UniformBuffer | DescriptorResourceType::StorageBuffer => {
                                let uniform_buffer_data = renderer_data.get_shader_buffer_data(ShaderBufferDataType::from_str(&material_parameter_name.as_str()).unwrap());
                                uniform_buffer_data._descriptor_buffer_infos[*swapchain_index].clone()
                            },
                            DescriptorResourceType::Texture | DescriptorResourceType::StorageTexture => {
                                let texture_data = match maybe_material_parameter {
                                    Some(Value::String(value)) => self.get_texture_data(value),
                                    _ => self.get_texture_data(DEFAULT_TEXTURE_NAME),
                                };
                                if descriptor_data_create_info.use_sub_image() {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.borrow().get_sub_image_info(
                                        descriptor_data_create_info._descriptor_image_layer,
                                        descriptor_data_create_info._descriptor_image_mip_level,
                                    ))
                                } else {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.borrow().get_default_image_info())
                                }
                            },
                            DescriptorResourceType::RenderTarget | DescriptorResourceType::StorageRenderTarget => {
                                let texture_data = renderer_data.get_render_target(RenderTargetType::from_str(&material_parameter_name.as_str()).unwrap());
                                if descriptor_data_create_info.use_sub_image() {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_sub_image_info(
                                        descriptor_data_create_info._descriptor_image_layer,
                                        descriptor_data_create_info._descriptor_image_mip_level,
                                    ))
                                } else {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_default_image_info())
                                }
                            },
                        };
                        return descriptor_resource_info;
                    }).filter(|descriptor_resource_info| match *descriptor_resource_info {
                        DescriptorResourceInfo::InvalidDescriptorInfo => false,
                        _ => true,
                    }).collect();
                    return descriptor_resource_infos;
                }).collect();
                return (render_pass_pipeline_data.clone(), descriptor_resource_infos_list);
            }).collect();

            let material_instance_data = MaterialInstanceData::create_material_instance(
                renderer_data.get_device(),
                &material_instance_name,
                material_data.clone(),
                pipeline_bind_create_infos
            );
            self._material_instance_data_map.insert(material_instance_name.clone(), newRcRefCell(material_instance_data));
        }
    }

    pub fn unload_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        for material_instance_data in self._material_instance_data_map.values() {
            (*material_instance_data).borrow().destroy_material_instance();
        }
        self._material_instance_data_map.clear();
    }

    pub fn update_material_instance_datas(&mut self) {
        for (_key, model_data) in self._model_data_map.iter() {
            let new_material_instances = model_data.borrow().get_material_instance_datas().iter().map(|material_instance| {
                self.get_material_instance_data(&material_instance.borrow()._material_instance_data_name.as_str()).clone()
            }).collect();
            model_data.borrow_mut().set_material_instance_datas(new_material_instances);
        }
    }

    pub fn has_material_instance_data(&self, resource_name: &str) -> bool {
        self._material_instance_data_map.contains_key(resource_name)
    }

    pub fn get_material_instance_data(&self, resource_name: &str) -> &RcRefCell<MaterialInstanceData> {
        get_resource_data_must(&self._material_instance_data_map, resource_name)
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