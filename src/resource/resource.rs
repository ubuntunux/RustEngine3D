use std::collections::HashMap;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use ash::vk;
use bincode;
use byteorder::{LittleEndian, ReadBytesExt};
use image::{self, GenericImageView};
use nalgebra::{Vector2, Vector3};
use serde::{Deserialize, Serialize};
use serde_json::{self, Value};

use crate::audio::audio_manager::{AudioBankCreateInfo, AudioBankData, AudioData};
use crate::constants;
use crate::effect::effect_data::{
    EffectData, EffectDataCreateInfo, EmitterData, EmitterDataCreateInfo,
};
use crate::renderer::renderer_context::RendererContext;
use crate::resource::collada_loader::Collada;
use crate::resource::font_loader;
use crate::resource::gltf_loader::GLTF;
use crate::resource::obj_loader::WaveFrontOBJ;
use crate::resource::texture_generator;
use crate::scene::animation::AnimationLayerData;
use crate::scene::font::{self, FontData, FontDataCreateInfo};
use crate::scene::material::MaterialData;
use crate::scene::material_instance::{MaterialInstanceData, PipelineBindingDataCreateInfo};
use crate::scene::mesh::{MeshData, MeshDataCreateInfo};
use crate::scene::model::{ModelData, ModelDataInfo};
use crate::scene::scene_manager::SceneDataCreateInfo;
use crate::utilities::system::{self, newRcRefCell, ptr_as_mut, ptr_as_ref, RcRefCell};
use crate::vulkan_context::descriptor::{
    self, DescriptorData, DescriptorResourceInfo, DescriptorResourceType,
};
use crate::vulkan_context::framebuffer::{self, FramebufferData};
use crate::vulkan_context::geometry_buffer::{self, GeometryData};
use crate::vulkan_context::render_pass::{
    self, PipelineDataCreateInfo, RenderPassData, RenderPassDataCreateInfo, RenderPassPipelineData,
};
use crate::vulkan_context::texture::{TextureCreateInfo, TextureData};

const USE_JSON_FOR_MESH: bool = false;
const LOAD_FROM_EXTERNAL_FOR_MESH: bool = true;

pub const APPLICATION_RESOURCE_PATH: &str = "resources";
pub const ENGINE_RESOURCE_PATH: &str = "resources/engine_resources";
pub const ENGINE_RESOURCE_SOURCE_PATH: &str = "RustEngine3D/engine_resources";

pub const ANIMATION_LAYER_DIRECTORY: &str = "animation_layers";
pub const AUDIO_DIRECTORY: &str = "externals/sounds";
pub const AUDIO_BANK_DIRECTORY: &str = "sound_banks";
pub const EFFECT_DIRECTORY: &str = "effects";
pub const FONT_SOURCE_DIRECTORY: &str = "externals/fonts";
pub const FONT_DIRECTORY: &str = "fonts";
pub const FONT_TEXTURE_DIRECTORY: &str = "externals/textures/fonts";
pub const MATERIAL_DIRECTORY: &str = "materials";
pub const MATERIAL_INSTANCE_DIRECTORY: &str = "material_instances";
pub const MESH_SOURCE_DIRECTORY: &str = "meshes"; // "externals/meshes";
pub const MESH_DIRECTORY: &str = "meshes";
pub const MODEL_DIRECTORY: &str = "models";
pub const TEXTURE_SOURCE_DIRECTORY: &str = "externals/textures";
pub const TEXTURE_DIRECTORY: &str = "textures";
pub const SHADER_CACHE_DIRECTORY: &str = "shader_caches";
pub const SHADER_DIRECTORY: &str = "shaders";
pub const SCENE_DIRECTORY: &str = "scenes";

pub const EXT_ANIMATION_LAYER: &str = "layer";
pub const EXT_AUDIO_SOURCE: [&str; 2] = ["wav", "mp3"];
pub const EXT_AUDIO_BANK: &str = "bank";
pub const EXT_EFFECT: &str = "effect";
pub const EXT_FONT_SOURCE: [&str; 1] = ["ttf"];
pub const EXT_FONT: &str = "font";
pub const EXT_FONT_TEXTURE: &str = "png";
pub const EXT_OBJ: &str = "obj";
pub const EXT_COLLADA: &str = "dae";
pub const EXT_GLTF_BINARY: &str = "glb";
pub const EXT_GLTF: &str = "gltf";
pub const EXT_MESH_SOURCE: [&str; 3] = [EXT_OBJ, EXT_COLLADA, EXT_GLTF];
pub const EXT_META_FILE: &str = "meta";
pub const EXT_JSON: &str = "json";
pub const EXT_MATERIAL: &str = "mat";
pub const EXT_MATERIAL_INSTANCE: &str = "matinst";
pub const EXT_MESH: &str = "mesh";
pub const EXT_MODEL: &str = "model";
pub const EXT_IMAGE_SOURCE: [&str; 5] = ["jpeg", "jpg", "png", "tga", "bmp"];
pub const EXT_TEXTURE_CUBE: &str = "cube";
pub const EXT_TEXTURE_2D_ARRAY: &str = "2darray";
pub const EXT_TEXTURE_3D: &str = "3d";
pub const EXT_TEXTURE: [&str; 1] = ["texture"];
pub const EXT_SCENE: &str = "scene";

pub const DEFAULT_ANIMATION_LAYER_NAME: &str = "default";
pub const DEFAULT_AUDIO_NAME: &str = "default";
pub const DEFAULT_AUDIO_BANK_NAME: &str = "default";
pub const DEFAULT_EFFECT_NAME: &str = "default";
pub const DEFAULT_EFFECT_MATERIAL_INSTANCE_NAME: &str = "effect/render_particle";
pub const DEFAULT_FONT_NAME: &str = "UbuntuSansMono-Regular_Basic_Latin";
pub const DEFAULT_RENDER_FONT_MATERIAL_INSTANCE_NAME: &str = "ui/render_font";
pub const DEFAULT_RENDER_UI_MATERIAL_INSTANCE_NAME: &str = "ui/render_ui";
pub const TEXTURE_FONT_MATERIAL_PARAMETER_NAME: &str = "texture_font";
pub const DEFAULT_MESH_NAME: &str = "quad";
pub const DEFAULT_MODEL_NAME: &str = "quad";
pub const DEFAULT_TEXTURE_NAME: &str = "common/default";
pub const DEFAULT_MATERIAL_INSTANCE_NAME: &str = "default";
pub const DEFAULT_RENDER_PASS_NAME: &str = "render_pass_static_opaque";

pub type CallbackLoadRenderPassCreateInfo = fn(
    renderer_context: &RendererContext,
    render_pass_data_create_info_map: &mut RenderPassDataCreateInfoMap
);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct MetaData {
    pub _is_engine_resource: bool,
    pub _meta_file_path: PathBuf,
    pub _resource_version: i32,
    pub _resource_file_path: PathBuf,
    pub _resource_modify_time: SystemTime,
    pub _source_file_path: PathBuf,
    pub _source_modify_time: SystemTime,
    pub _source_changed: bool,
}

impl Default for MetaData {
    fn default() -> MetaData {
        MetaData {
            _is_engine_resource: true,
            _meta_file_path: PathBuf::new(),
            _resource_version: 0,
            _resource_file_path: PathBuf::new(),
            _resource_modify_time: SystemTime::now(),
            _source_file_path: PathBuf::new(),
            _source_modify_time: SystemTime::now(),
            _source_changed: false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResourceData {
    None,
    Audio(RcRefCell<AudioData>),
    AudioBank(RcRefCell<AudioBankData>),
}

#[derive(Debug, Clone)]
pub struct ResourceInfo {
    _resource_name: String,
    _resource_data: ResourceData,
    _meta_data: MetaData,
}

pub type ResourceDataMap<T> = HashMap<String, RcRefCell<T>>;
pub type ResourceInfoMap = HashMap<String, ResourceInfo>;

pub type AnimationLayerDataMap = ResourceDataMap<AnimationLayerData>;
pub type AudioBankDataMap = ResourceDataMap<AudioBankData>;
pub type EffectDataMap<'a> = ResourceDataMap<EffectData<'a>>;
pub type FramebufferDataListMap<'a> = ResourceDataMap<FramebufferData<'a>>;
pub type MaterialDataMap<'a> = ResourceDataMap<MaterialData<'a>>;
pub type MaterialInstanceDataMap<'a> = ResourceDataMap<MaterialInstanceData<'a>>;
pub type FontDataMap = ResourceDataMap<FontData>;
pub type MeshDataMap = ResourceDataMap<MeshData>;
pub type ModelDataMap<'a> = ResourceDataMap<ModelData<'a>>;
pub type TextureDataMap = ResourceDataMap<TextureData>;
pub type RenderPassDataMap<'a> = ResourceDataMap<RenderPassData<'a>>;
pub type RenderPassDataCreateInfoMap = HashMap<String, RenderPassDataCreateInfo>;
pub type SceneDataCreateInfoMap = ResourceDataMap<SceneDataCreateInfo>;
pub type DescriptorDataMap<'a> = ResourceDataMap<DescriptorData<'a>>;
pub type MetaDataMap = ResourceDataMap<MetaData>;
type LoadImageInfoType = (u32, u32, u32, Vec<u8>, vk::Format);


#[derive(Clone)]
pub struct EngineResources<'a> {
    pub _relative_resource_file_path_map: HashMap<PathBuf, PathBuf>,
    pub _animation_layer_data_map: AnimationLayerDataMap,
    pub _audio_data_map: ResourceInfoMap,
    pub _audio_bank_data_map: ResourceInfoMap,
    pub _effect_data_map: EffectDataMap<'a>,
    pub _font_data_map: FontDataMap,
    pub _mesh_data_map: MeshDataMap,
    pub _model_data_map: ModelDataMap<'a>,
    pub _texture_data_map: TextureDataMap,
    pub _framebuffer_data_list_map: FramebufferDataListMap<'a>,
    pub _render_pass_data_map: RenderPassDataMap<'a>,
    pub _render_pass_data_create_info_map: RenderPassDataCreateInfoMap,
    pub _scene_data_create_infos_map: SceneDataCreateInfoMap,
    pub _material_data_map: MaterialDataMap<'a>,
    pub _material_instance_data_map: MaterialInstanceDataMap<'a>,
    pub _descriptor_data_map: DescriptorDataMap<'a>,
    pub _callback_load_render_pass_create_infos: *const CallbackLoadRenderPassCreateInfo,
}

fn walk_directory_recursive(dir: &Path, include_extensions: &[&str], ignore_extensions: &[&str], out_contents: &mut Vec<PathBuf>) {
    let contents = fs::read_dir(dir).unwrap();
    for content in contents {
        let content = content.unwrap().path();
        if content.is_dir() {
            walk_directory_recursive(&content, include_extensions, ignore_extensions, out_contents);
        } else {
            let ext = content.extension();
            if !ignore_extensions.is_empty() && ext.is_some() && ignore_extensions.contains(&ext.unwrap().to_str().unwrap()) {
                continue;
            }
            else if !include_extensions.is_empty() && ext.is_some() && !include_extensions.contains(&ext.unwrap().to_str().unwrap()) {
                continue;
            }

            out_contents.push(PathBuf::from(content));
        }
    }
}

pub fn walk_directory(dir: &Path, include_extensions: &[&str], ignore_extensions: &[&str]) -> Vec<PathBuf> {
    let mut out_contents: Vec<PathBuf> = Vec::new();
    walk_directory_recursive(dir, include_extensions, ignore_extensions, &mut out_contents);
    out_contents
}

pub fn get_resource_data_must<'a, T>(
    resource_data_map: &'a ResourceDataMap<T>,
    resource_name: &str,
) -> &'a RcRefCell<T> {
    if let Some(resource) = resource_data_map.get(resource_name) {
        return resource
    };
    panic!("not found resource: {:?}", resource_name);
}

pub fn get_resource_data<'a, T>(
    resource_data_map: &'a ResourceDataMap<T>,
    resource_name: &str,
    default_resource_name: &str,
) -> &'a RcRefCell<T> {
    let maybe_data = resource_data_map.get(resource_name);
    match maybe_data {
        None => {
            log::error!("not found resource: {}", resource_name);
            resource_data_map.get(default_resource_name).unwrap()
        }
        _ => maybe_data.unwrap(),
    }
}

pub fn get_resource_name_from_file_path(
    resource_dircetory: &PathBuf,
    resource_file_path: &PathBuf,
) -> String {
    let mut resource_name = PathBuf::from(resource_file_path.parent().unwrap());
    resource_name.push(resource_file_path.file_stem().unwrap());

    let mut resource_root_path: PathBuf;
    if resource_file_path.starts_with(ENGINE_RESOURCE_PATH) {
        resource_root_path = PathBuf::from(ENGINE_RESOURCE_PATH);
    } else {
        resource_root_path = PathBuf::from(APPLICATION_RESOURCE_PATH);
    }
    resource_root_path.push(resource_dircetory);

    let resource_name = String::from(
        system::get_relative_path(&resource_root_path, &resource_name)
            .to_str()
            .unwrap(),
    );
    resource_name.replace("\\", "/")
}

pub fn get_unique_resource_name<T>(
    resource_map: &HashMap<String, T>,
    resource_dircetory: &PathBuf,
    resource_file_path: &PathBuf,
) -> String {
    let resource_name = get_resource_name_from_file_path(resource_dircetory, resource_file_path);
    system::generate_unique_name(resource_map, &resource_name)
}

pub fn get_resource_file_path(
    resource_root_path: &str,
    resource_dircetory: &str,
    resource_name: &String,
    resource_ext: &str,
) -> PathBuf {
    let mut resource_file_path: PathBuf = PathBuf::from(resource_root_path);
    resource_file_path.push(resource_dircetory);
    resource_file_path.push(resource_name);
    resource_file_path.set_extension(resource_ext);
    resource_file_path
}

pub fn make_engine_resource_file_path(file_name: &str) -> PathBuf {
    PathBuf::from(ENGINE_RESOURCE_PATH).join(file_name)
}

pub fn make_application_resource_file_path(file_name: &str) -> PathBuf {
    PathBuf::from(APPLICATION_RESOURCE_PATH).join(file_name)
}

impl MetaData {
    pub fn new(resource_file_path: &PathBuf, source_file_path: &PathBuf) -> MetaData {
        let mut meta_file_path: PathBuf = resource_file_path.clone();
        meta_file_path.set_extension(EXT_META_FILE);

        let meta_data = if meta_file_path.is_file() {
            let loaded_contents = system::load(&meta_file_path);
            serde_json::from_reader(loaded_contents).expect("Failed to deserialize.")
        } else {
            let meta_data = MetaData {
                _is_engine_resource: resource_file_path.starts_with(ENGINE_RESOURCE_PATH),
                _meta_file_path: meta_file_path.clone(),
                _resource_version: 0,
                _resource_file_path: resource_file_path.clone(),
                _resource_modify_time: fs::metadata(&resource_file_path)
                    .unwrap()
                    .modified()
                    .unwrap(),
                _source_file_path: source_file_path.clone(),
                _source_modify_time: fs::metadata(&source_file_path).unwrap().modified().unwrap(),
                _source_changed: false,
            };

            let mut write_meta_file = File::create(&meta_file_path).expect("Failed to create file");
            let mut write_meta_contents: String = serde_json::to_string(&meta_data).expect("Failed to serialize.");
            write_meta_contents = write_meta_contents.replace(",\"", ",\n\"");
            write_meta_file.write(write_meta_contents.as_bytes()).expect("Failed to write");
            meta_data
        };

        meta_data
    }
}

impl<'a> EngineResources<'a> {
    pub fn create_engine_resources(
        callback_load_render_pass_create_info: *const CallbackLoadRenderPassCreateInfo
    ) -> Box<EngineResources<'a>> {
        let mut engine_resource = EngineResources {
            _relative_resource_file_path_map: HashMap::new(),
            _animation_layer_data_map: AnimationLayerDataMap::new(),
            _audio_data_map: ResourceInfoMap::new(),
            _audio_bank_data_map: ResourceInfoMap::new(),
            _effect_data_map: EffectDataMap::new(),
            _font_data_map: FontDataMap::new(),
            _mesh_data_map: MeshDataMap::new(),
            _model_data_map: ModelDataMap::new(),
            _texture_data_map: TextureDataMap::new(),
            _framebuffer_data_list_map: FramebufferDataListMap::new(),
            _render_pass_data_map: RenderPassDataMap::new(),
            _render_pass_data_create_info_map: RenderPassDataCreateInfoMap::new(),
            _scene_data_create_infos_map: SceneDataCreateInfoMap::new(),
            _material_data_map: MaterialDataMap::new(),
            _material_instance_data_map: MaterialInstanceDataMap::new(),
            _descriptor_data_map: DescriptorDataMap::new(),
            _callback_load_render_pass_create_infos: callback_load_render_pass_create_info,
        };

        engine_resource.preinitialize_engine_resources();
        Box::new(engine_resource)
    }

    pub fn preinitialize_engine_resources(&mut self) {
        let resource_list_file_path: PathBuf =
            PathBuf::from(APPLICATION_RESOURCE_PATH).join("resources.txt");

        #[cfg(not(target_os = "android"))]
        {
            // update engine resources
            self.update_engine_resources();

            // create resources.txt file
            let include_extensions: &[&str] = &[];
            let ignore_extensions: &[&str] = &[ "log" ];
            let mut application_resource_files = walk_directory(&Path::new(APPLICATION_RESOURCE_PATH), include_extensions, ignore_extensions);
            application_resource_files.sort();
            let mut write_file = File::create(&resource_list_file_path).expect("Failed to create file");
            for file_path in application_resource_files {
                if let Some(filename) = file_path.file_name() {
                    if "empty" == filename {
                        continue;
                    }
                }

                if resource_list_file_path != file_path {
                    write_file
                        .write(format!("{}\n", file_path.to_str().unwrap()).as_bytes())
                        .expect("Failed to write");
                }
            }
        }

        let loaded_resources = system::load(&resource_list_file_path);
        let contents: String = String::from_utf8(loaded_resources.into_inner()).unwrap();
        let resources = contents.split("\n");
        for resource in resources {
            let resource_file_path = PathBuf::from(resource);
            if resource.is_empty() || resource_list_file_path == resource_file_path {
                continue;
            }

            let mut relative_resource_file_path = resource_file_path.clone();
            let is_engine_resource = relative_resource_file_path.starts_with(ENGINE_RESOURCE_PATH);
            if is_engine_resource {
                relative_resource_file_path = relative_resource_file_path
                    .strip_prefix(ENGINE_RESOURCE_PATH)
                    .unwrap()
                    .to_path_buf();
            } else {
                relative_resource_file_path = relative_resource_file_path
                    .strip_prefix(APPLICATION_RESOURCE_PATH)
                    .unwrap()
                    .to_path_buf();
            }

            if self
                ._relative_resource_file_path_map
                .get(&relative_resource_file_path)
                .is_none()
                || false == is_engine_resource
            {
                self._relative_resource_file_path_map
                    .insert(relative_resource_file_path.clone(), resource_file_path);
            }
        }
    }

    pub fn update_engine_resources(&mut self) {
        log::info!("EngineResources::update_engine_resources");
        let engine_resource_path = Path::new(ENGINE_RESOURCE_PATH);
        let engine_resource_source_path = Path::new(ENGINE_RESOURCE_SOURCE_PATH);

        // build engine_resource_file_map
        let mut engine_resource_file_map: HashMap<PathBuf, PathBuf> = HashMap::new();
        if engine_resource_path.is_dir() {
            let engine_font_path = PathBuf::from(ENGINE_RESOURCE_PATH).join(FONT_DIRECTORY);
            let engine_font_texture_path = PathBuf::from(ENGINE_RESOURCE_PATH).join(FONT_TEXTURE_DIRECTORY);
            let engine_shader_cache_path = PathBuf::from(ENGINE_RESOURCE_PATH).join(SHADER_CACHE_DIRECTORY);
            let include_extensions: &[&str] = &[];
            let ignore_extensions: &[&str] = &[ "log" ];
            let engine_resource_files = walk_directory(&engine_resource_path, include_extensions, ignore_extensions);
            for file_path in engine_resource_files.iter() {
                if false == file_path.starts_with(&engine_font_path)
                    && false == file_path.starts_with(&engine_font_texture_path)
                    && false == file_path.starts_with(&engine_shader_cache_path)
                {
                    engine_resource_file_map.insert(file_path.clone(), file_path.clone());
                }
            }
        }

        // build engine_resource_source_file_map
        let mut engine_resource_source_file_map: HashMap<PathBuf, PathBuf> = HashMap::new();
        if engine_resource_source_path.is_dir() {
            let engine_font_source_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(FONT_DIRECTORY);
            let engine_font_texture_source_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(FONT_TEXTURE_DIRECTORY);
            let engine_shader_cache_source_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(SHADER_CACHE_DIRECTORY);
            let include_extensions: &[&str] = &[];
            let ignore_extensions: &[&str] = &[ "log" ];
            let engine_resource_source_files = walk_directory(&engine_resource_source_path, include_extensions, ignore_extensions);
            for file_path in engine_resource_source_files.iter() {
                if false == file_path.starts_with(&engine_font_source_path)
                    && false == file_path.starts_with(&engine_font_texture_source_path)
                    && false == file_path.starts_with(&engine_shader_cache_source_path)
                {
                    engine_resource_source_file_map.insert(file_path.clone(), file_path.clone());
                }
            }
        }

        // remove unnecessary engine resource files
        for (_, file_path) in engine_resource_file_map.iter() {
            let relative_file_path: PathBuf = file_path
                .strip_prefix(ENGINE_RESOURCE_PATH)
                .unwrap()
                .to_path_buf();
            let engine_source_file_path: PathBuf =
                PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(relative_file_path);
            if false == engine_source_file_path.is_file() {
                fs::remove_file(&file_path).expect("failed to remove file");
            } else {
                let modified_time: SystemTime =
                    fs::metadata(file_path).unwrap().modified().unwrap();
                let source_modified_time: SystemTime = fs::metadata(engine_source_file_path)
                    .unwrap()
                    .modified()
                    .unwrap();
                if modified_time < source_modified_time {
                    fs::remove_file(&file_path).expect("failed to remove file");
                }
            }
        }

        // copy engine resource source file -> engine resource file
        for (_, source_file_path) in engine_resource_source_file_map.iter() {
            let relative_source_file_path: PathBuf = source_file_path
                .strip_prefix(ENGINE_RESOURCE_SOURCE_PATH)
                .unwrap()
                .to_path_buf();
            let engine_file_path: PathBuf =
                PathBuf::from(ENGINE_RESOURCE_PATH).join(relative_source_file_path);
            if false == engine_file_path.is_file() {
                let dir = engine_file_path.parent().unwrap();
                log::info!(
                    "dir: {:?}, source_file_path: {:?}, engine_file_path: {:?}",
                    dir,
                    source_file_path,
                    engine_file_path
                );
                if false == dir.is_dir() {
                    fs::create_dir_all(dir).expect("Failed to create directories.");
                }
                fs::copy(source_file_path, engine_file_path).expect("failed to file copy");
            }
        }
    }

    pub fn load_engine_resources(&mut self, renderer_context: &'a RendererContext<'a>) {
        log::info!("load_engine_resources");
        let is_reload: bool = false;
        self.load_texture_data_list(renderer_context);
        self.load_font_data_list(renderer_context);
        self.load_render_pass_data_create_infos(renderer_context);
        self.load_render_pass_data_list(renderer_context);
        self.load_framebuffer_data_list(renderer_context);
        self.load_material_data_list();
        self.load_material_instance_data_list(renderer_context, is_reload);
        self.load_mesh_data_list(renderer_context);
        self.load_model_data_list();
        self.load_animation_layer_data_list();
        self.load_audio_data_list();
        self.load_effect_data_list();
        self.load_scene_data_list(renderer_context);
    }

    pub fn destroy_engine_resources(&mut self, renderer_context: &RendererContext) {
        log::info!("destroy_engine_resources");
        let is_reload: bool = false;
        self.unload_scene_data_list(renderer_context);
        self.unload_effect_data_list();
        self.unload_audio_data_list();
        self.unload_animation_layer_data_list();
        self.unload_model_data_list();
        self.unload_mesh_data_list(renderer_context);
        self.unload_material_instance_data_list(is_reload);
        self.unload_material_data_list();
        self.unload_framebuffer_data_list(renderer_context);
        self.unload_render_pass_data_list(renderer_context);
        self.unload_render_pass_data_create_infos(renderer_context);
        self.unload_font_data_list();
        self.unload_texture_data_list(renderer_context);
        self.unload_descriptor_data_list(renderer_context);
    }

    // GraphicsDataList
    pub fn load_graphics_data_list(&mut self, renderer_context: &'a RendererContext<'a>) {
        log::info!("load_graphics_data_list");
        let is_reload: bool = true;
        self.load_render_pass_data_create_infos(renderer_context);
        self.load_render_pass_data_list(renderer_context);
        self.load_framebuffer_data_list(renderer_context);
        self.load_material_data_list();
        self.load_material_instance_data_list(renderer_context, is_reload);
    }

    pub fn unload_graphics_data_list(&mut self, renderer_context: &'a RendererContext<'a>) {
        log::info!("unload_graphics_data_list");
        let is_reload: bool = true;
        self.unload_material_instance_data_list(is_reload);
        self.unload_material_data_list();
        self.unload_framebuffer_data_list(renderer_context);
        self.unload_render_pass_data_list(renderer_context);
        self.unload_render_pass_data_create_infos(renderer_context);
        self.unload_descriptor_data_list(renderer_context);
    }

    pub fn create_resource(&mut self) {
        // nothing..
    }

    pub fn register_resource(&mut self) {
        // nothing..
    }

    pub fn unregister_resource(&mut self) {
        // nothing..
    }

    pub fn collect_resources(&self, directory: &Path, extensions: &[&str]) -> Vec<PathBuf> {
        let mut out_engine_resources: Vec<PathBuf> = Vec::new();
        for (relative_filepath, resource_filename) in self._relative_resource_file_path_map.iter() {
            if relative_filepath.starts_with(directory) {
                let ext = resource_filename.extension();
                if extensions.is_empty()
                    || (ext.is_some() && extensions.contains(&ext.unwrap().to_str().unwrap()))
                {
                    out_engine_resources.push(PathBuf::from(resource_filename));
                }
            }
        }
        out_engine_resources
    }

    // AnimationLayerData
    pub fn load_animation_layer_data_list(&mut self) {
        log::info!("    load_animation_layer_data_list");
        let animation_layer_directory = PathBuf::from(ANIMATION_LAYER_DIRECTORY);
        let animation_layer_files: Vec<PathBuf> = self.collect_resources(&animation_layer_directory, &[EXT_ANIMATION_LAYER]);
        for animation_layer_file in animation_layer_files {
            let animation_layer_name =
                get_unique_resource_name(&self._animation_layer_data_map, &animation_layer_directory, &animation_layer_file);
            let loaded_contents = system::load(&animation_layer_file);
            let contents: AnimationLayerData = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            self._animation_layer_data_map
                .insert(animation_layer_name.clone(), newRcRefCell(contents));
        }
    }

    pub fn unload_animation_layer_data_list(&mut self) {
        self._animation_layer_data_map.clear();
    }

    pub fn has_animation_layer_data(&self, resource_name: &str) -> bool {
        self._animation_layer_data_map.contains_key(resource_name)
    }

    pub fn get_animation_layer_data(&self, resource_name: &str) -> &RcRefCell<AnimationLayerData> {
        get_resource_data(&self._animation_layer_data_map, resource_name, DEFAULT_ANIMATION_LAYER_NAME)
    }

    // Audio Data
    pub fn load_audio_data_list(&mut self) {
        log::info!("    load_audio_data_list");
        let audio_directory = PathBuf::from(AUDIO_DIRECTORY);
        let audio_bank_directory = PathBuf::from(AUDIO_BANK_DIRECTORY);

        // load audio data_list
        let audio_data_files: Vec<PathBuf> =
            self.collect_resources(&audio_directory, &EXT_AUDIO_SOURCE);
        for audio_data_file in audio_data_files.iter() {
            let audio_data_name =
                get_unique_resource_name(&self._audio_data_map, &audio_directory, &audio_data_file);
            let meta_data = MetaData::new(&audio_data_file, &audio_data_file);
            let audio_resource_info = ResourceInfo {
                _resource_name: audio_data_name.clone(),
                _resource_data: ResourceData::None,
                _meta_data: meta_data,
            };
            self._audio_data_map
                .insert(audio_data_name, audio_resource_info);
        }

        // create default audio bank data
        {
            let mut default_audio_bank_file_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
            default_audio_bank_file_path.push(&audio_bank_directory);
            default_audio_bank_file_path.push(&DEFAULT_AUDIO_BANK_NAME);
            default_audio_bank_file_path.set_extension(EXT_AUDIO_BANK);
            #[cfg(not(target_os = "android"))]
            if false == default_audio_bank_file_path.is_file() {
                let mut default_audio_bank_data_create_info = AudioBankCreateInfo::default();
                default_audio_bank_data_create_info
                    ._audio_names
                    .push(DEFAULT_AUDIO_NAME.to_string());
                log::info!(
                    "default_audio_bank_file_path: {:?}",
                    default_audio_bank_file_path
                );
                let mut write_file =
                    File::create(&default_audio_bank_file_path).expect("Failed to create file");
                let mut write_contents: String =
                    serde_json::to_string(&default_audio_bank_data_create_info)
                        .expect("Failed to serialize.");
                write_contents = write_contents.replace(",\"", ",\n\"");
                write_file
                    .write(write_contents.as_bytes())
                    .expect("Failed to write");
            }
        }

        // load audio bank data_list
        let audio_bank_data_files: Vec<PathBuf> =
            self.collect_resources(&audio_bank_directory, &[EXT_AUDIO_BANK]);
        for audio_bank_data_file in audio_bank_data_files {
            let audio_bank_data_name = get_unique_resource_name(
                &self._audio_bank_data_map,
                &audio_bank_directory,
                &audio_bank_data_file,
            );
            let meta_data = MetaData::new(&audio_bank_data_file, &audio_bank_data_file);
            let audio_resource_info = ResourceInfo {
                _resource_name: audio_bank_data_name.clone(),
                _resource_data: ResourceData::None,
                _meta_data: meta_data,
            };
            self._audio_bank_data_map
                .insert(audio_bank_data_name.clone(), audio_resource_info);
        }
    }

    pub fn unload_audio_data_list(&mut self) {
        self._audio_bank_data_map.clear();
        self._audio_data_map.clear();
    }

    pub fn has_audio_data(&self, resource_name: &str) -> bool {
        self._audio_data_map.get(resource_name).is_some()
    }

    pub fn get_audio_data(&mut self, resource_name: &str) -> &ResourceData {
        if let Some(resource_info) = self._audio_data_map.get_mut(resource_name) {
            match resource_info._resource_data {
                ResourceData::None => {
                    if resource_info._meta_data._resource_file_path.is_file() {
                        // load audio data
                        let audio_data = AudioData {
                            _audio_name: resource_info._resource_name.clone(),
                            _sound_chunk: sdl2::mixer::Chunk::from_file(
                                &resource_info._meta_data._resource_file_path,
                            )
                            .unwrap(),
                        };
                        resource_info._resource_data =
                            ResourceData::Audio(newRcRefCell(audio_data));
                    }
                }
                _ => (),
            }
            return &resource_info._resource_data;
        }
        &ResourceData::None
    }

    pub fn has_audio_bank_data(&self, resource_name: &str) -> bool {
        self._audio_bank_data_map.get(resource_name).is_some()
    }

    pub fn get_audio_bank_data(&mut self, resource_name: &str) -> &ResourceData {
        if let Some(resource_info) = self._audio_bank_data_map.get(resource_name) {
            let resource_info = ptr_as_mut(resource_info);
            match resource_info._resource_data {
                ResourceData::None => {
                    if resource_info._meta_data._resource_file_path.is_file() {
                        // load audio bank data
                        let loaded_contents =
                            system::load(&resource_info._meta_data._resource_file_path);
                        let audio_bank_create_info: AudioBankCreateInfo =
                            serde_json::from_reader(loaded_contents)
                                .expect("Failed to deserialize.");
                        let mut audio_data_list: Vec<RcRefCell<AudioData>> = Vec::new();
                        for audio_name in audio_bank_create_info._audio_names.iter() {
                            if let ResourceData::Audio(audio_data) = self.get_audio_data(audio_name) {
                                audio_data_list.push(audio_data.clone());
                            }
                        }
                        let audio_bank_data = AudioBankData {
                            _audio_bank_name: resource_info._resource_name.clone(),
                            _audio_data_list: audio_data_list,
                        };
                        resource_info._resource_data =
                            ResourceData::AudioBank(newRcRefCell(audio_bank_data));
                    }
                }
                _ => (),
            }
            return &resource_info._resource_data;
        }
        &ResourceData::None
    }

    // EffectData
    pub fn load_effect_data_list(&mut self) {
        log::info!("    load_effect_data_list");
        // create default effect
        let mut default_effect_file_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
        default_effect_file_path.push(EFFECT_DIRECTORY);
        default_effect_file_path.push(DEFAULT_EFFECT_NAME);
        default_effect_file_path.set_extension(EXT_EFFECT);
        #[cfg(not(target_os = "android"))]
        if false == default_effect_file_path.is_file() {
            let default_effect_data_create_info = EffectDataCreateInfo {
                _emitter_data_create_infos: vec![EmitterDataCreateInfo {
                    _enable: true,
                    _emitter_data_name: String::from("emitter"),
                    _emitter_lifetime: -1.0,
                    _scale_min: Vector3::new(0.5, 0.5, 0.5),
                    _scale_max: Vector3::new(1.0, 1.0, 1.0),
                    _max_particle_count: 1000,
                    _spawn_count: 10,
                    _spawn_term: 0.0,
                    _particle_lifetime_min: 1.0,
                    _particle_lifetime_max: 3.0,
                    _velocity_min: Vector3::new(-1.0, 15.0, -1.0),
                    _velocity_max: Vector3::new(1.0, 15.0, 1.0),
                    _force_min: Vector3::new(0.0, -7.0, 0.0),
                    _force_max: Vector3::new(0.0, -8.0, 0.0),
                    _material_instance_name: String::from(DEFAULT_EFFECT_MATERIAL_INSTANCE_NAME),
                    _mesh_name: String::from(DEFAULT_MESH_NAME),
                    ..Default::default()
                }],
                ..Default::default()
            };
            let mut write_file =
                File::create(&default_effect_file_path).expect("Failed to create file");
            let mut write_contents: String =
                serde_json::to_string(&default_effect_data_create_info)
                    .expect("Failed to serialize.");
            write_contents = write_contents.replace(",\"", ",\n\"");
            write_file
                .write(write_contents.as_bytes())
                .expect("Failed to write");
        }

        let effect_files: Vec<PathBuf> =
            self.collect_resources(&Path::new(EFFECT_DIRECTORY), &[EXT_EFFECT]);
        for effect_file in effect_files {
            let effect_data_name = get_unique_resource_name(
                &self._effect_data_map,
                &PathBuf::from(EFFECT_DIRECTORY),
                &effect_file,
            );
            let loaded_contents = system::load(&effect_file);
            let effect_data_create_info: EffectDataCreateInfo =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let emitter_data_list = effect_data_create_info
                ._emitter_data_create_infos
                .iter()
                .map(|emitter_data_create_info| {
                    let material_instance_data = self.get_material_instance_data(
                        &emitter_data_create_info._material_instance_name,
                    );
                    let mesh_data = self.get_mesh_data(&emitter_data_create_info._mesh_name);
                    EmitterData::create_emitter_data(
                        emitter_data_create_info,
                        material_instance_data.clone(),
                        mesh_data.clone(),
                    )
                })
                .collect();
            let effect_data = EffectData::create_effect_data(
                &effect_data_name,
                &effect_data_create_info,
                emitter_data_list,
            );
            self._effect_data_map
                .insert(effect_data_name.clone(), newRcRefCell(effect_data));
        }
    }

    pub fn unload_effect_data_list(&mut self) {
        for effect_data in self._effect_data_map.values() {
            effect_data.borrow_mut().destroy_effect_data();
        }
        self._effect_data_map.clear();
    }

    pub fn has_effect_data(&self, resource_name: &str) -> bool {
        self._effect_data_map.contains_key(resource_name)
    }

    pub fn get_effect_data(&self, resource_name: &str) -> &RcRefCell<EffectData<'a>> {
        get_resource_data(&self._effect_data_map, resource_name, DEFAULT_EFFECT_NAME)
    }

    // FontData
    pub fn get_font_data_create_info(
        &self,
        resource_root_path: &PathBuf,
        font_directory: &PathBuf,
        font_texture_directory: &PathBuf,
        font_data_name: &String,
        font_source_file: &PathBuf,
        range_min: u32,
        range_max: u32,
    ) -> FontDataCreateInfo {
        let mut font_file_path: PathBuf = resource_root_path.clone();
        font_file_path.push(font_directory);
        font_file_path.push(font_data_name);
        font_file_path.set_extension(EXT_FONT);
        fs::create_dir_all(font_file_path.parent().unwrap())
            .expect("Failed to create directories.");

        let mut font_texture_file_path: PathBuf = resource_root_path.clone();
        font_texture_file_path.push(font_texture_directory);
        font_texture_file_path.push(&font_data_name);
        font_texture_file_path.set_extension(EXT_FONT_TEXTURE);
        fs::create_dir_all(font_texture_file_path.parent().unwrap())
            .expect("Failed to create directories.");

        let mut write_file = File::create(&font_file_path).expect("Failed to create file");
        let font_data_create_info = font_loader::get_font_data_create_info(
            &font_source_file,
            font::FONT_SIZE as f32,
            font::FONT_PADDING as f32,
            &font_data_name,
            &font_texture_file_path,
            range_min,
            range_max,
        );
        let write_contents: String =
            serde_json::to_string(&font_data_create_info).expect("Failed to serialize.");
        write_file
            .write(write_contents.as_bytes())
            .expect("Failed to write");
        font_data_create_info
    }

    pub fn load_font_data_list(&mut self, renderer_context: &RendererContext) {
        log::info!("    load_font_data_list");
        let mut unicode_blocks: HashMap<String, (u32, u32)> = HashMap::new();
        unicode_blocks.insert(String::from("Basic_Latin"), (0x20, 0x7F)); // 32 ~ 127
        //unicode_blocks.insert(String::from("Hangul_Syllables"), (0xAC00, 0xD7AF)); // 44032 ~ 55215

        // gather font files
        let font_directory = PathBuf::from(FONT_DIRECTORY);
        let font_files = self.collect_resources(font_directory.as_path(), &[EXT_FONT]);
        let mut font_file_map: HashMap<String, PathBuf> = HashMap::new();
        for font_file in font_files.iter() {
            let font_name = get_resource_name_from_file_path(&font_directory, &font_file);
            font_file_map.insert(font_name, font_file.clone());
        }

        // load font textures
        let font_texture_directory = PathBuf::from(FONT_TEXTURE_DIRECTORY);
        let font_source_directory = PathBuf::from(FONT_SOURCE_DIRECTORY);
        let font_source_files: Vec<PathBuf> = self.collect_resources(&font_source_directory, &EXT_FONT_SOURCE);
        for font_source_file in font_source_files {
            let is_engine_resource = font_source_file.starts_with(ENGINE_RESOURCE_PATH);
            let resource_root_path: PathBuf;
            if is_engine_resource {
                resource_root_path = PathBuf::from(ENGINE_RESOURCE_PATH);
            } else {
                resource_root_path = PathBuf::from(APPLICATION_RESOURCE_PATH);
            }
            for (unicode_block_key, (range_min, range_max)) in unicode_blocks.iter() {
                let font_name = get_unique_resource_name(
                    &self._font_data_map,
                    &font_source_directory,
                    &font_source_file,
                );
                let font_data_name = format!("{}_{}", font_name, unicode_block_key);
                let font_texture_name = format!("fonts/{}_{}", font_name, unicode_block_key);

                // Gets FontDataCreateInfo and also creates font texture files if needed.
                #[cfg(target_os = "android")]
                let check_font_texture_file_exists = false;
                #[cfg(not(target_os = "android"))]
                let check_font_texture_file_exists = true;
                let maybe_font_file = font_file_map.get(&font_data_name);
                let font_data_create_info: FontDataCreateInfo = if check_font_texture_file_exists
                    && (maybe_font_file.is_none()
                        || false == self.has_texture_data(&font_texture_name))
                {
                    self.get_font_data_create_info(
                        &resource_root_path,
                        &font_directory,
                        &font_texture_directory,
                        &font_data_name,
                        &font_source_file,
                        *range_min,
                        *range_max,
                    )
                } else {
                    let loaded_contents = system::load(maybe_font_file.unwrap());
                    serde_json::from_reader(loaded_contents).expect("Failed to deserialize.")
                };

                // regist font texture
                if false == self.has_texture_data(&font_texture_name) {
                    let mut font_texture_file_path: PathBuf;
                    if is_engine_resource {
                        font_texture_file_path = PathBuf::from(ENGINE_RESOURCE_PATH);
                    } else {
                        font_texture_file_path = PathBuf::from(APPLICATION_RESOURCE_PATH);
                    }
                    font_texture_file_path.push(&font_texture_directory);
                    font_texture_file_path.push(&font_data_name);
                    font_texture_file_path.set_extension(EXT_FONT_TEXTURE);

                    let (image_width, image_height, image_layers, image_data, image_format): LoadImageInfoType =
                        EngineResources::load_image_data(&font_texture_file_path);

                    assert_ne!(vk::Format::UNDEFINED, image_format);
                    let texture_create_info = TextureCreateInfo {
                        _texture_name: font_texture_name.clone(),
                        _texture_width: image_width,
                        _texture_height: image_height,
                        _texture_layers: image_layers,
                        _texture_format: image_format,
                        _texture_view_type: vk::ImageViewType::TYPE_2D,
                        _texture_initial_data_list: image_data,
                        _enable_mipmap: false,
                        _enable_anisotropy: false,
                        ..Default::default()
                    };
                    let texture_data =
                        newRcRefCell(renderer_context.create_texture(&texture_create_info));
                    self.register_texture_data(font_texture_name.clone(), texture_data);
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
                        (texture_data.borrow()._image_width / font_data_create_info._count_of_side)
                            as f32,
                        (texture_data.borrow()._image_height / font_data_create_info._count_of_side)
                            as f32,
                    ),
                    _texture: texture_data.clone(),
                });
                self._font_data_map.insert(font_data_name, font_data);
            }
        }
    }

    pub fn unload_font_data_list(&mut self) {
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
    pub fn load_model_data_list(&mut self) {
        log::info!("    load_model_data_list");
        let model_directory = PathBuf::from(MODEL_DIRECTORY);
        let model_files: Vec<PathBuf> = self.collect_resources(&model_directory, &[EXT_MODEL]);
        for model_file in model_files {
            let model_name = get_unique_resource_name(&self._model_data_map, &model_directory, &model_file);
            let loaded_contents = system::load(&model_file);
            let model_data_info: ModelDataInfo = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_instance_count = model_data_info._material_instances.len();
            let mesh_data = self.get_mesh_data(model_data_info._mesh.as_str());
            let geometry_data_count = mesh_data.borrow().get_geometry_data_count();
            let material_instance_data_list: Vec<RcRefCell<MaterialInstanceData>> =
                (0..geometry_data_count).map(|index| {
                    if 0 < material_instance_count {
                        let material_index = 0.max(index.min(material_instance_count - 1));
                        self.get_material_instance_data(&model_data_info._material_instances[material_index]).clone()
                    } else {
                        self.get_material_instance_data(DEFAULT_MATERIAL_INSTANCE_NAME).clone()
                    }
                })
                .collect();

            let model_data = ModelData::create_model_data(
                &model_name,
                &mesh_data,
                material_instance_data_list,
                &model_data_info
            );

            self._model_data_map.insert(model_name.clone(), newRcRefCell(model_data));
        }
    }

    pub fn unload_model_data_list(&mut self) {
        for model_data in self._model_data_map.values() {
            model_data.borrow().destroy_model_data();
        }
        self._model_data_map.clear();
    }

    pub fn has_model_data(&self, resource_name: &str) -> bool {
        self._model_data_map.contains_key(resource_name)
    }

    pub fn get_model_data(&self, resource_name: &str) -> &RcRefCell<ModelData<'a>> {
        get_resource_data(&self._model_data_map, resource_name, DEFAULT_MODEL_NAME)
    }

    // Mesh Loader
    pub fn register_mesh_data(
        &mut self,
        renderer_context: &RendererContext<'a>,
        mesh_name: &String,
        mesh_data_create_info: MeshDataCreateInfo,
    ) {
        let mut geometry_data_list: Vec<RcRefCell<GeometryData>> = Vec::new();
        for (i, geometry_create_info) in mesh_data_create_info
            ._geometry_create_infos
            .iter()
            .enumerate()
        {
            let geomtery_name: String = format!("{}_{}", mesh_name, i);
            let geometry_data =
                renderer_context.create_geometry_buffer(&geomtery_name, geometry_create_info);
            geometry_data_list.push(newRcRefCell(geometry_data));
        }

        let mesh_data = newRcRefCell(MeshData::create_mesh_data(
            &mesh_name,
            mesh_data_create_info,
            geometry_data_list,
        ));
        self._mesh_data_map
            .insert(mesh_name.clone(), mesh_data.clone());
    }

    pub fn load_mesh_data_list(&mut self, renderer_context: &RendererContext<'a>) {
        log::info!("    load_mesh_data_list");
        self.register_mesh_data(
            renderer_context,
            &String::from("quad"),
            geometry_buffer::quad_mesh_create_info(),
        );
        self.register_mesh_data(
            renderer_context,
            &String::from("cube"),
            geometry_buffer::cube_mesh_create_info(),
        );
        let mesh_directory = PathBuf::from(MESH_DIRECTORY);
        let mesh_source_directory = PathBuf::from(MESH_SOURCE_DIRECTORY);
        let resource_ext = if USE_JSON_FOR_MESH {
            EXT_JSON
        } else {
            EXT_MESH
        };
        let mesh_files = self.collect_resources(mesh_directory.as_path(), &[resource_ext]);
        let mut mesh_file_map: HashMap<String, PathBuf> = HashMap::new();
        for mesh_file in mesh_files.iter() {
            let mesh_name = get_resource_name_from_file_path(&mesh_directory, &mesh_file);
            mesh_file_map.insert(mesh_name, mesh_file.clone());
        }
        let mesh_source_files =
            self.collect_resources(mesh_source_directory.as_path(), &EXT_MESH_SOURCE);
        for mesh_source_file in mesh_source_files {
            let mesh_name = get_unique_resource_name(
                &self._mesh_data_map,
                &mesh_source_directory,
                &mesh_source_file,
            );
            let src_file_ext: String =
                String::from(mesh_source_file.extension().unwrap().to_str().unwrap());
            let mesh_data_create_info =
                match (LOAD_FROM_EXTERNAL_FOR_MESH, mesh_file_map.get(&mesh_name)) {
                    (false, Some(mesh_file)) => {
                        // Load mesh
                        let loaded_contents = system::load(mesh_file);
                        if USE_JSON_FOR_MESH {
                            serde_json::from_reader(loaded_contents)
                                .expect("Failed to deserialize.")
                        } else {
                            bincode::deserialize_from(loaded_contents).unwrap()
                        }
                    }
                    _ => {
                        // Convert to mesh from source
                        let mesh_data_create_info = match src_file_ext.as_str() {
                            EXT_OBJ => WaveFrontOBJ::get_mesh_data_create_infos(&mesh_source_file),
                            EXT_COLLADA => Collada::get_mesh_data_create_infos(&mesh_source_file),
                            EXT_GLTF_BINARY => GLTF::get_mesh_data_create_infos(&mesh_source_file),
                            EXT_GLTF => GLTF::get_mesh_data_create_infos(&mesh_source_file),
                            _ => panic!("error"),
                        };

                        // Save mesh
                        if false == LOAD_FROM_EXTERNAL_FOR_MESH {
                            let mut mesh_file_path: PathBuf;
                            if mesh_source_file.starts_with(ENGINE_RESOURCE_PATH) {
                                mesh_file_path = PathBuf::from(ENGINE_RESOURCE_PATH);
                            } else {
                                mesh_file_path = PathBuf::from(APPLICATION_RESOURCE_PATH);
                            }
                            mesh_file_path.push(&mesh_directory);
                            mesh_file_path.push(&mesh_name);
                            mesh_file_path.set_extension(resource_ext);
                            fs::create_dir_all(mesh_file_path.parent().unwrap())
                                .expect("Failed to create directories.");
                            let mut write_file =
                                File::create(mesh_file_path).expect("Failed to create file");
                            if USE_JSON_FOR_MESH {
                                let write_contents: String =
                                    serde_json::to_string(&mesh_data_create_info)
                                        .expect("Failed to serialize.");
                                write_file
                                    .write(write_contents.as_bytes())
                                    .expect("Failed to write");
                            } else {
                                let write_contents: Vec<u8> =
                                    bincode::serialize(&mesh_data_create_info).unwrap();
                                write_file.write(&write_contents).expect("Failed to write");
                            }
                        }

                        mesh_data_create_info
                    }
                };
            self.register_mesh_data(renderer_context, &mesh_name, mesh_data_create_info);
        }
    }

    pub fn unload_mesh_data_list(&mut self, renderer_context: &RendererContext) {
        for mesh_data in self._mesh_data_map.values() {
            for geometry_data in (*mesh_data).borrow().get_geometry_data_list() {
                renderer_context.destroy_geometry_buffer(&geometry_data.borrow());
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
            image::DynamicImage::ImageRgba8(_) => (
                dynamic_image.to_rgba8().into_raw(),
                vk::Format::R8G8B8A8_UNORM,
            ),
            image::DynamicImage::ImageRgb8(_) => (
                dynamic_image.to_rgba8().into_raw(),
                vk::Format::R8G8B8A8_UNORM,
            ),
            image::DynamicImage::ImageLuma8(_) => (
                dynamic_image.to_rgba8().into_raw(),
                vk::Format::R8G8B8A8_UNORM,
            ),
            image::DynamicImage::ImageRgb16(_) => (
                dynamic_image.to_rgba8().into_raw(),
                vk::Format::R16G16B16A16_UNORM,
            ),
            _ => {
                log::error!("Unkown format: {:?}", texture_file);
                (Vec::new(), vk::Format::UNDEFINED)
            }
        };
        (
            image_width,
            image_height,
            image_layer,
            image_data_raw,
            image_format,
        )
    }

    pub fn load_image_data_list(texture_files: &Vec<PathBuf>) -> LoadImageInfoType {
        let mut image_width: u32 = 0;
        let mut image_height: u32 = 0;
        let mut image_format: vk::Format = vk::Format::UNDEFINED;
        let mut image_data_list: Vec<Vec<u8>> = Vec::new();
        for texture_file in texture_files.iter() {
            let (width, height, _layer, image_data, format): LoadImageInfoType =
                EngineResources::load_image_data(texture_file);
            image_width = width;
            image_height = height;
            image_format = format;
            image_data_list.push(image_data.clone());
        }
        let image_data_list = image_data_list.concat();
        (
            image_width,
            image_height,
            texture_files.len() as u32,
            image_data_list,
            image_format,
        )
    }

    pub fn register_texture_data(
        &mut self,
        texture_data_name: String,
        texture_data: RcRefCell<TextureData>,
    ) {
        self._texture_data_map
            .insert(texture_data_name, texture_data);
    }

    pub fn load_texture_data_list(&mut self, renderer_context: &RendererContext) {
        log::info!("    load_texture_data_list");
        let default_texture_data_list: Vec<TextureData> =
            texture_generator::generate_textures(renderer_context);
        for texture_data in default_texture_data_list {
            self.register_texture_data(
                texture_data._texture_data_name.clone(),
                newRcRefCell(texture_data),
            );
        }

        let texture_directory = PathBuf::from(TEXTURE_DIRECTORY);
        let texture_source_directory = PathBuf::from(TEXTURE_SOURCE_DIRECTORY);
        let mut combined_textures_name_map: HashMap<PathBuf, String> = HashMap::new();
        let mut combined_texture_files_map: HashMap<String, Vec<PathBuf>> = HashMap::new();
        let mut combined_texture_types_map: HashMap<String, vk::ImageViewType> = HashMap::new();

        // generate necessary texture data_list
        #[cfg(not(target_os = "android"))]
        {
            let mut engine_texture_source_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
            engine_texture_source_path.push(TEXTURE_SOURCE_DIRECTORY);
            texture_generator::generate_images(&engine_texture_source_path);
        }

        // combined texture list
        let combined_texture_files = self.collect_resources(
            texture_source_directory.as_path(),
            &[EXT_TEXTURE_2D_ARRAY, EXT_TEXTURE_3D, EXT_TEXTURE_CUBE],
        );
        for texture_src_file in combined_texture_files.iter() {
            let directory = texture_src_file.parent().unwrap();
            let texture_data_name =
                get_resource_name_from_file_path(&texture_source_directory, &texture_src_file);
            let loaded_contents = system::load(texture_src_file);
            let contents =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
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
        let texture_src_files = self.collect_resources(texture_source_directory.as_path(), &EXT_IMAGE_SOURCE);
        for texture_src_file in texture_src_files.iter() {
            let combined_texture_name = combined_textures_name_map.get(texture_src_file);
            let texture_data_name: String = match combined_texture_name {
                Some(combined_texture_name) => combined_texture_name.clone(),
                _ => get_resource_name_from_file_path(&texture_source_directory, &texture_src_file),
            };

            let existing_resource_data: Option<&RcRefCell<TextureData>> =
                self._texture_data_map.get(&texture_data_name);
            if existing_resource_data.is_none() {
                let (
                    image_view_type,
                    (image_width, image_height, image_layers, image_data, image_format),
                ): (vk::ImageViewType, LoadImageInfoType) = if combined_texture_name.is_some() {
                    let combined_texture_files = combined_texture_files_map
                        .get(texture_data_name.as_str())
                        .unwrap();
                    let image_view_type = combined_texture_types_map
                        .get(texture_data_name.as_str())
                        .unwrap();
                    (
                        *image_view_type,
                        EngineResources::load_image_data_list(&combined_texture_files),
                    )
                } else {
                    (
                        vk::ImageViewType::TYPE_2D,
                        EngineResources::load_image_data(texture_src_file),
                    )
                };
                if vk::Format::UNDEFINED != image_format {
                    let texture_create_info = TextureCreateInfo {
                        _texture_name: texture_data_name.clone(),
                        _texture_width: image_width,
                        _texture_height: image_height,
                        _texture_layers: image_layers,
                        _texture_format: image_format,
                        _texture_view_type: image_view_type,
                        _texture_initial_data_list: image_data,
                        _enable_mipmap: true,
                        _enable_anisotropy: false,
                        ..Default::default()
                    };
                    let texture_data = renderer_context.create_texture(&texture_create_info);
                    self._texture_data_map
                        .insert(texture_data_name, newRcRefCell(texture_data));
                }
            }
        }

        // read binary texture data
        let texture_files = self.collect_resources(texture_directory.as_path(), &EXT_TEXTURE);
        for texture_file in texture_files.iter() {
            let texture_data_name =
                get_resource_name_from_file_path(&texture_directory, texture_file);
            let mut loaded_contents = system::load(&texture_file);
            let image_view_type: vk::ImageViewType =
                vk::ImageViewType::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap());
            let image_width: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_height: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_depth: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let image_format: vk::Format =
                vk::Format::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap());
            let enable_mipmap: bool = if 0 != loaded_contents.read_i32::<LittleEndian>().unwrap() {
                true
            } else {
                false
            };
            let min_filter: vk::Filter =
                vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap());
            let mag_filter: vk::Filter =
                vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().unwrap());
            let wrap: vk::SamplerAddressMode = vk::SamplerAddressMode::from_raw(
                loaded_contents.read_i32::<LittleEndian>().unwrap(),
            );
            let data_bytes: i32 = loaded_contents.read_i32::<LittleEndian>().unwrap();
            let mut image_data: Vec<u8> = Vec::new();
            loaded_contents
                .read_to_end(&mut image_data)
                .expect("Failed to read data_list.");
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
                _texture_initial_data_list: image_data,
                _enable_mipmap: enable_mipmap,
                _enable_anisotropy: false,
                ..Default::default()
            };
            let texture_data = renderer_context.create_texture(&texture_create_info);
            self._texture_data_map
                .insert(texture_data_name, newRcRefCell(texture_data));
        }
    }

    pub fn unload_texture_data_list(&mut self, renderer_context: &RendererContext) {
        for texture_data in self._texture_data_map.values() {
            renderer_context.destroy_texture(&(*texture_data).borrow());
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
    pub fn load_framebuffer_data_list(&mut self, renderer_context: &RendererContext) {
        log::info!("    load_framebuffer_data_list");
        for render_pass_data in self._render_pass_data_map.values() {
            let render_pass_data = render_pass_data.borrow();
            for (_key, render_pass_data_create_info) in
                self._render_pass_data_create_info_map.iter()
            {
                if render_pass_data_create_info._render_pass_create_info_name
                    == render_pass_data._render_pass_data_name
                {
                    if render_pass_data_create_info
                        ._render_pass_framebuffer_create_info
                        .is_valid()
                    {
                        let framebuffer_data = framebuffer::create_framebuffer_data(
                            renderer_context.get_device(),
                            renderer_context.get_debug_utils(),
                            render_pass_data._render_pass,
                            render_pass_data._render_pass_data_name.as_str(),
                            render_pass_data_create_info
                                ._render_pass_framebuffer_create_info
                                .clone(),
                        );
                        self._framebuffer_data_list_map.insert(
                            render_pass_data._render_pass_data_name.clone(),
                            newRcRefCell(framebuffer_data),
                        );
                        break;
                    }
                }
            }
        }
    }

    pub fn unload_framebuffer_data_list(&mut self, renderer_context: &RendererContext) {
        for framebuffer_data in self._framebuffer_data_list_map.values() {
            framebuffer::destroy_framebuffer_data(
                renderer_context.get_device(),
                &framebuffer_data.borrow(),
            );
        }
        self._framebuffer_data_list_map.clear();
    }

    pub fn has_framebuffer_data(&self, resource_name: &str) -> bool {
        self._framebuffer_data_list_map.contains_key(resource_name)
    }

    pub fn get_framebuffer_data(&self, resource_name: &str) -> &RcRefCell<FramebufferData<'a>> {
        get_resource_data_must(&self._framebuffer_data_list_map, resource_name)
    }

    // render pass data create info
    pub fn load_render_pass_data_create_infos(&mut self, renderer_context: &RendererContext) {
        let render_pass_data_create_infos = renderer_context.get_render_pass_data_create_infos();
        for render_pass_data_create_info in render_pass_data_create_infos.iter() {
            self._render_pass_data_create_info_map.insert(
                render_pass_data_create_info
                    ._render_pass_create_info_name
                    .clone(),
                render_pass_data_create_info.clone(),
            );
        }

        {
            unsafe {
                (*self._callback_load_render_pass_create_infos)(
                    renderer_context,
                    &mut self._render_pass_data_create_info_map
                );
            }
        }
    }

    pub fn unload_render_pass_data_create_infos(&mut self, _renderer_context: &RendererContext) {
        self._render_pass_data_create_info_map.clear();
    }

    pub fn get_render_pass_data_create_info(&self, resource_name: &str) -> &RenderPassDataCreateInfo {
        if let Some(resource) = self._render_pass_data_create_info_map.get(resource_name) {
            return resource
        };
        panic!("not found resource: {:?}", resource_name);
    }

    // render pass data
    pub fn load_render_pass_data_list(&mut self, renderer_context: &RendererContext<'a>) {
        log::info!("    load_render_pass_data_list");

        let render_pass_data_create_info_map = ptr_as_ref(&self._render_pass_data_create_info_map);
        for (_key, render_pass_data_create_info) in render_pass_data_create_info_map.iter() {
            let mut descriptor_data_list: Vec<RcRefCell<DescriptorData>> = Vec::new();
            for pipeline_data_create_info in render_pass_data_create_info
                ._pipeline_data_create_infos
                .iter()
            {
                descriptor_data_list.push(
                    self.get_descriptor_data(
                        renderer_context,
                        &render_pass_data_create_info._render_pass_create_info_name,
                        pipeline_data_create_info,
                    )
                );
            }
            let default_render_pass_data = render_pass::create_render_pass_data(
                renderer_context,
                render_pass_data_create_info,
                &descriptor_data_list,
            );
            self._render_pass_data_map.insert(
                default_render_pass_data.get_render_pass_data_name().clone(),
                newRcRefCell(default_render_pass_data),
            );
        }
    }

    pub fn unload_render_pass_data_list(&mut self, renderer_context: &RendererContext) {
        for render_pass_data in self._render_pass_data_map.values() {
            render_pass::destroy_render_pass_data(
                renderer_context.get_device(),
                &(*render_pass_data).borrow(),
            );
        }
        self._render_pass_data_map.clear()
    }

    pub fn has_render_pass_data(&self, resource_name: &str) -> bool {
        self._render_pass_data_map.contains_key(resource_name)
    }

    pub fn get_render_pass_data(&self, resource_name: &str) -> &RcRefCell<RenderPassData<'a>> {
        get_resource_data_must(&self._render_pass_data_map, resource_name)
    }

    pub fn get_default_render_pass_data(&self) -> &RcRefCell<RenderPassData<'a>> {
        self.get_render_pass_data(DEFAULT_RENDER_PASS_NAME)
    }

    pub fn get_render_pass_pipeline_data(
        &self,
        render_pass_data_name: &str,
        pipeline_data_name: &str,
    ) -> RenderPassPipelineData<'a> {
        let render_pass_data_refcell = self.get_render_pass_data(render_pass_data_name);
        let render_pass_data = render_pass_data_refcell.borrow();
        let pipeline_data = render_pass_data.get_pipeline_data(pipeline_data_name);
        RenderPassPipelineData {
            _render_pass_data: render_pass_data_refcell.clone(),
            _pipeline_data: pipeline_data.clone(),
        }
    }

    // Material_data_list
    pub fn load_material_data_list(&mut self) {
        log::info!("    load_material_data_list");
        let material_directory = PathBuf::from(MATERIAL_DIRECTORY);
        let material_files = self.collect_resources(&material_directory.as_path(), &[EXT_MATERIAL]);
        for material_file in material_files {
            let material_name = get_unique_resource_name(
                &self._material_data_map,
                &material_directory,
                &material_file,
            );
            let loaded_contents = system::load(material_file);
            let contents: Value =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_create_info = match contents {
                Value::Object(material_create_info) => material_create_info,
                _ => panic!("material parsing error"),
            };
            let pipeline_create_infos = material_create_info
                .get("pipelines")
                .unwrap()
                .as_array()
                .unwrap();
            let render_pass_pipeline_data_list: Vec<Option<RenderPassPipelineData<'a>>> =
                pipeline_create_infos
                    .iter()
                    .map(|pipeline_create_info| {
                        let render_pass_data_name =
                            match pipeline_create_info.get("render_pass").unwrap() {
                                Value::String(render_pass_data_name) => render_pass_data_name,
                                _ => panic!("failed to parsing render_pass"),
                            };
                        let pipeline_data_name = match pipeline_create_info.get("pipeline").unwrap()
                        {
                            Value::String(pipeline_data_name) => pipeline_data_name,
                            _ => panic!("failed to parsing pipeline"),
                        };

                        if self.has_render_pass_data(render_pass_data_name.as_str()) {
                            Some(self.get_render_pass_pipeline_data(
                                render_pass_data_name.as_str(),
                                pipeline_data_name.as_str(),
                            ))
                        } else {
                            None
                        }
                    })
                    .collect();

            let empty_object: serde_json::Map<String, Value> = serde_json::Map::new();
            let material_parameters = match material_create_info.get("material_parameters") {
                Some(Value::Object(material_parameters)) => material_parameters,
                _ => &empty_object,
            };

            let material_data = MaterialData::create_material(
                &material_name,
                &render_pass_pipeline_data_list,
                material_parameters,
            );
            self._material_data_map
                .insert(material_name.clone(), newRcRefCell(material_data));
        }
    }

    pub fn unload_material_data_list(&mut self) {
        for material_data in self._material_data_map.values() {
            material_data.borrow().destroy_material();
        }
        self._material_data_map.clear();
    }

    pub fn has_material_data(&self, resource_name: &str) -> bool {
        self._material_data_map.contains_key(resource_name)
    }

    pub fn get_material_data(&self, resource_name: &str) -> &RcRefCell<MaterialData<'a>> {
        get_resource_data_must(&self._material_data_map, resource_name)
    }

    // MaterialInstance_data_list
    pub fn load_material_instance_data_list(
        &mut self,
        renderer_context: &'a RendererContext<'a>,
        is_reload: bool,
    ) {
        log::info!("    load_material_instance_data_list");
        let material_instance_directory = PathBuf::from(MATERIAL_INSTANCE_DIRECTORY);
        let material_instance_files =
            self.collect_resources(&material_instance_directory, &[EXT_MATERIAL_INSTANCE]);
        for material_instance_file in material_instance_files.iter() {
            let material_instance_name = if is_reload {
                get_resource_name_from_file_path(
                    &material_instance_directory,
                    &material_instance_file,
                )
            } else {
                get_unique_resource_name(
                    &self._material_instance_data_map,
                    &material_instance_directory,
                    &material_instance_file,
                )
            };

            let loaded_contents = system::load(material_instance_file);
            let contents: Value =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_instance_create_info = match contents {
                Value::Object(material_instance_create_info) => material_instance_create_info,
                _ => panic!("material instance parsing error"),
            };
            let material_data_name =
                match material_instance_create_info.get("material_name").unwrap() {
                    Value::String(material_data_name) => material_data_name,
                    _ => panic!("material name parsing error"),
                };

            let mut material_parameters: serde_json::Map<String, Value> =
                match material_instance_create_info.get("material_parameters") {
                    Some(Value::Object(material_parameters)) => material_parameters.clone(),
                    _ => serde_json::Map::new(),
                };

            let material_data = self.get_material_data(material_data_name.as_str()).clone();
            let default_material_parameters = &material_data.borrow()._material_parameters;
            for (key, value) in default_material_parameters.iter() {
                if false == material_parameters.contains_key(key) {
                    material_parameters.insert(key.to_string(), value.clone());
                }
            }

            // replace texture_font parameter as a matched to default font.
            if material_instance_name == DEFAULT_RENDER_FONT_MATERIAL_INSTANCE_NAME || material_instance_name == DEFAULT_RENDER_UI_MATERIAL_INSTANCE_NAME {
                material_parameters.insert(
                    String::from(TEXTURE_FONT_MATERIAL_PARAMETER_NAME),
                    Value::String(String::from("fonts/") + &String::from(DEFAULT_FONT_NAME))
                );
            }

            let pipeline_bind_create_infos = material_data.borrow()._render_pass_pipeline_data_map.iter().map(|(_key, render_pass_pipeline_data)| {
                let descriptor_data_create_infos = &render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data._descriptor_data_create_infos;
                let descriptor_resource_infos_list = constants::SWAPCHAIN_IMAGE_INDICES.iter().map(|swapchain_index| {
                    let descriptor_resource_infos = descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
                        let material_parameter_name = &descriptor_data_create_info._descriptor_name;
                        let material_parameter_resource_type = &descriptor_data_create_info._descriptor_resource_type;
                        let maybe_material_parameter = material_parameters.get(material_parameter_name);
                        let descriptor_resource_info = match material_parameter_resource_type {
                            DescriptorResourceType::UniformBuffer | DescriptorResourceType::StorageBuffer => {
                                let uniform_buffer_data = renderer_context.get_shader_buffer_data_from_str(material_parameter_name.as_str());
                                uniform_buffer_data._descriptor_buffer_infos[*swapchain_index].clone()
                            },
                            DescriptorResourceType::Sampler => {
                                let sampler = renderer_context.get_sampler_from_str(material_parameter_name.as_str());
                                DescriptorResourceInfo::DescriptorSamplerInfo(*sampler)
                            },
                            DescriptorResourceType::Image | DescriptorResourceType::Texture | DescriptorResourceType::StorageTexture => {
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
                                let texture_data = renderer_context.get_render_target_from_str(material_parameter_name.as_str());
                                if descriptor_data_create_info.use_sub_image() {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_sub_image_info(
                                        descriptor_data_create_info._descriptor_image_layer,
                                        descriptor_data_create_info._descriptor_image_mip_level,
                                    ))
                                } else {
                                    DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_default_image_info())
                                }
                            },
                            DescriptorResourceType::AccelerationStructure => {
                                log::info!(">>> TEST CODE: load_material_instance_data_list: {:?}", material_instance_name);
                                log::info!("    WriteDescriptorSetAccelerationStructure");
                                let ray_tracing_data = renderer_context.get_ray_tracing_test_data();
                                ray_tracing_data.get_top_level_descriptor_resource_info()
                            },
                        };
                        return descriptor_resource_info;
                    }).collect();
                    return descriptor_resource_infos;
                }).collect();

                // update push constant
                let mut push_constant_data_list = render_pass_pipeline_data._pipeline_data.borrow()._push_constant_data_list.clone();
                for push_constant_data in push_constant_data_list.iter_mut() {
                    push_constant_data._push_constant.update_material_parameters(&material_parameters);
                }

                // create PipelineBindingDataCreateInfo
                PipelineBindingDataCreateInfo {
                    _render_pass_pipeline_data: render_pass_pipeline_data.clone(),
                    _descriptor_resource_infos_list: descriptor_resource_infos_list,
                    _push_constant_data_list: push_constant_data_list
                }
            }).collect();

            let material_instance_data = MaterialInstanceData::create_material_instance(
                renderer_context.get_device(),
                renderer_context.get_debug_utils(),
                &material_instance_name,
                &material_data,
                &material_parameters,
                pipeline_bind_create_infos,
            );

            if is_reload && self.has_material_instance_data(&material_instance_name) {
                // replace material_instance_data
                let exists_material_instance_data: &mut MaterialInstanceData = &mut self
                    .get_material_instance_data(&material_instance_name)
                    .borrow_mut();
                *exists_material_instance_data = material_instance_data;
            } else {
                self._material_instance_data_map.insert(
                    material_instance_name.clone(),
                    newRcRefCell(material_instance_data),
                );
            }
        }
    }

    pub fn unload_material_instance_data_list(&mut self, is_reload: bool) {
        for material_instance_data in self._material_instance_data_map.values() {
            (*material_instance_data)
                .borrow()
                .destroy_material_instance();
        }

        if false == is_reload {
            self._material_instance_data_map.clear();
        }
    }

    pub fn has_material_instance_data(&self, resource_name: &str) -> bool {
        self._material_instance_data_map.contains_key(resource_name)
    }

    pub fn get_material_instance_data(
        &self,
        resource_name: &str,
    ) -> &RcRefCell<MaterialInstanceData<'a>> {
        get_resource_data_must(&self._material_instance_data_map, resource_name)
    }

    // Descriptor_data_list
    pub fn get_descriptor_data(
        &mut self,
        renderer_context: &RendererContext<'a>,
        render_pass_name: &String,
        pipeline_data_create_info: &PipelineDataCreateInfo,
    ) -> RcRefCell<DescriptorData<'a>> {
        let descriptor_name: String = format!(
            "{}{}",
            render_pass_name, pipeline_data_create_info._pipeline_data_create_info_name
        );
        let descriptor_data_create_infos = &pipeline_data_create_info._descriptor_data_create_infos;
        // TODO: Let's set an appropriate max_descriptor_pool_count according to the render pass
        let max_descriptor_pool_count: u32 = unsafe {
            (constants::MAX_DESCRIPTOR_POOL_ALLOC_COUNT * constants::SWAPCHAIN_IMAGE_COUNT) as u32
        };
        let maybe_descriptor_data = self._descriptor_data_map.get(&descriptor_name);
        match maybe_descriptor_data {
            Some(descriptor_data) => descriptor_data.clone(),
            None => {
                let descriptor_data = newRcRefCell(descriptor::create_descriptor_data(
                    renderer_context.get_device(),
                    renderer_context.get_debug_utils(),
                    descriptor_name.as_str(),
                    descriptor_data_create_infos,
                    max_descriptor_pool_count,
                ));
                self._descriptor_data_map.insert(descriptor_name, descriptor_data.clone());
                descriptor_data
            }
        }
    }

    pub fn unload_descriptor_data_list(&mut self, renderer_context: &RendererContext) {
        for descriptor_data in self._descriptor_data_map.values() {
            descriptor::destroy_descriptor_data(
                renderer_context.get_device(),
                &(*descriptor_data).borrow(),
            );
        }
        self._descriptor_data_map.clear();
    }

    // SceneData
    pub fn load_scene_data_list(&mut self, _renderer_context: &RendererContext) {
        log::info!("    load_scene_data_list");
        let scene_directory = PathBuf::from(SCENE_DIRECTORY);
        let scene_data_files: Vec<PathBuf> = self.collect_resources(&scene_directory, &[EXT_SCENE]);
        for scene_data_file in scene_data_files {
            let scene_data_name = get_unique_resource_name(
                &self._scene_data_create_infos_map,
                &scene_directory,
                &scene_data_file,
            );
            let loaded_contents = system::load(&scene_data_file);
            let scene_data_create_info: SceneDataCreateInfo =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            self._scene_data_create_infos_map.insert(
                scene_data_name.clone(),
                newRcRefCell(scene_data_create_info),
            );
        }
    }

    pub fn unload_scene_data_list(&mut self, _renderer_context: &RendererContext) {
        self._scene_data_create_infos_map.clear();
    }

    pub fn save_scene_data(
        &mut self,
        scene_data_name: &str,
        scene_data_create_info: &SceneDataCreateInfo,
    ) {
        let mut scene_data_filepath = PathBuf::from(APPLICATION_RESOURCE_PATH);
        scene_data_filepath.push(SCENE_DIRECTORY);
        scene_data_filepath.push(scene_data_name);
        scene_data_filepath.set_extension(EXT_SCENE);
        let mut write_file = File::create(&scene_data_filepath).expect("Failed to create file");
        let mut write_contents: String =
            serde_json::to_string(&scene_data_create_info).expect("Failed to serialize.");
        write_contents = write_contents.replace(",\"", ",\n\"");
        write_file
            .write(write_contents.as_bytes())
            .expect("Failed to write");

        self._scene_data_create_infos_map.insert(
            String::from(scene_data_name),
            newRcRefCell(scene_data_create_info.clone()),
        );
    }

    pub fn has_scene_data(&self, resource_name: &str) -> bool {
        self._scene_data_create_infos_map
            .get(resource_name)
            .is_some()
    }

    pub fn get_scene_data(&self, resource_name: &str) -> &RcRefCell<SceneDataCreateInfo> {
        self._scene_data_create_infos_map
            .get(resource_name)
            .unwrap()
    }
}
