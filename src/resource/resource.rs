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
use crate::constants::DEVELOPMENT;
use crate::core::engine_service_locator::{get_engine_resources, get_renderer_context};
use crate::effect::effect_data::{EffectData, EffectDataCreateInfo, EmitterData, EmitterDataCreateInfo};
use crate::renderer::renderer_context::RendererContext;
use crate::resource::async_loader::{AsyncLoadRequest, AsyncLoadResult, AsyncResourceLoader};
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
use crate::utilities::system::{self, RcRefCell, newRcRefCell, ptr_as_ref};
use crate::vulkan_context::descriptor::{self, DescriptorData, DescriptorResourceInfo, DescriptorResourceType};
use crate::vulkan_context::framebuffer::{self, FramebufferData};
use crate::vulkan_context::geometry_buffer::{self, GeometryData};
use crate::vulkan_context::render_pass::{
    self, PipelineDataCreateInfo, RenderPassData, RenderPassDataCreateInfo, RenderPassPipelineData,
};
use crate::vulkan_context::texture::{TextureCreateInfo, TextureData};

pub const USE_JSON_FOR_MESH: bool = false;
pub const LOAD_FROM_EXTERNAL_FOR_MESH: bool = true;

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

pub type CallbackLoadRenderPassCreateInfo =
    fn(renderer_context: &RendererContext, render_pass_data_create_info_map: &mut RenderPassDataCreateInfoMap);

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

pub const DEFAULT_RESOURCE_GC_TIMEOUT_SEC: f32 = 30.0;
pub const RESOURCE_GC_CHECK_INTERVAL_SEC: f32 = 1.0;
pub const LOG_RESOURCE_LIFECYCLE: bool = false;
pub const DEFAULT_VRAM_BUDGET_BYTES: usize = 1536 * 1024 * 1024; // 1.5 GB
pub const DEFAULT_ASYNC_LOAD_TIME_SLICE_MS: f32 = 2.0; // 2.0 ms per frame
pub const DEFAULT_ASYNC_LOAD_MAX_ITEMS: usize = 8;
pub const LOADING_SCENE_ASYNC_TIME_SLICE_MS: f32 = 16.0;
pub const LOADING_SCENE_ASYNC_MAX_ITEMS: usize = 64;

#[derive(Clone, Debug, Default)]
pub struct ResourceStreamingMetrics {
    pub _loaded_textures: usize,
    pub _total_textures: usize,
    pub _texture_memory_bytes: usize,
    pub _loaded_meshes: usize,
    pub _total_meshes: usize,
    pub _mesh_memory_bytes: usize,
    pub _loaded_models: usize,
    pub _total_models: usize,
    pub _total_vram_bytes: usize,
    pub _vram_budget_bytes: usize,
    pub _pending_async_requests: usize,
    pub _last_async_processed_count: u32,
    pub _last_async_process_duration_ms: f32,
    pub _total_gc_evicted_count: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResourceState {
    Unloaded,
    Loading,
    Loaded,
    Failed,
}

pub struct ResourceEntry<T> {
    pub _resource_name: String,
    pub _meta_data: MetaData,
    pub _state: RcRefCell<ResourceState>,
    pub _is_pinned: bool,
    pub _unused_start_time: Option<std::time::Instant>,
    pub _cached_data: std::cell::UnsafeCell<Option<RcRefCell<T>>>,
}

impl<T> Clone for ResourceEntry<T> {
    fn clone(&self) -> Self {
        let cached = unsafe { (*self._cached_data.get()).clone() };
        ResourceEntry {
            _resource_name: self._resource_name.clone(),
            _meta_data: self._meta_data.clone(),
            _state: self._state.clone(),
            _is_pinned: self._is_pinned,
            _unused_start_time: self._unused_start_time,
            _cached_data: std::cell::UnsafeCell::new(cached),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ResourceEntry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResourceEntry")
            .field("_resource_name", &self._resource_name)
            .field("_meta_data", &self._meta_data)
            .field("_state", &self._state)
            .field("_is_pinned", &self._is_pinned)
            .field("_unused_start_time", &self._unused_start_time)
            .finish()
    }
}

impl<T> ResourceEntry<T> {
    pub fn new(resource_name: String, meta_data: MetaData) -> Self {
        ResourceEntry {
            _resource_name: resource_name,
            _meta_data: meta_data,
            _state: newRcRefCell(ResourceState::Unloaded),
            _is_pinned: false,
            _unused_start_time: None,
            _cached_data: std::cell::UnsafeCell::new(None),
        }
    }

    pub fn new_pinned(resource_name: String, meta_data: MetaData) -> Self {
        ResourceEntry {
            _resource_name: resource_name,
            _meta_data: meta_data,
            _state: newRcRefCell(ResourceState::Unloaded),
            _is_pinned: true,
            _unused_start_time: None,
            _cached_data: std::cell::UnsafeCell::new(None),
        }
    }

    pub fn is_loaded(&self) -> bool {
        *self._state.borrow() == ResourceState::Loaded
    }

    pub fn is_loading(&self) -> bool {
        *self._state.borrow() == ResourceState::Loading
    }

    pub fn get_state(&self) -> ResourceState {
        *self._state.borrow()
    }

    pub fn set_state(&self, state: ResourceState) {
        *self._state.borrow_mut() = state;
    }

    pub fn get_loaded_data(&self) -> Option<RcRefCell<T>> {
        unsafe { (*self._cached_data.get()).clone() }
    }

    pub fn set_loaded_data(&self, data: RcRefCell<T>) {
        if LOG_RESOURCE_LIFECYCLE {
            log::info!("[RESOURCE LOAD (Explicit)] {}", self._resource_name);
        }
        *self._state.borrow_mut() = ResourceState::Loaded;
        unsafe {
            *self._cached_data.get() = Some(data);
        }
    }

    pub fn pin(&mut self) {
        self._is_pinned = true;
        self._unused_start_time = None;
    }

    pub fn is_pinned(&self) -> bool {
        self._is_pinned
    }

    pub fn unload(&self) {
        self.unload_with(|_| {});
    }

    pub fn unload_with<F>(&self, destructor: F)
    where
        F: FnOnce(&RcRefCell<T>),
    {
        if LOG_RESOURCE_LIFECYCLE && self.is_loaded() {
            log::info!("[RESOURCE UNLOAD] {}", self._resource_name);
        }
        *self._state.borrow_mut() = ResourceState::Unloaded;
        unsafe {
            if let Some(data) = (*self._cached_data.get()).take() {
                destructor(&data);
            }
        }
    }

    pub fn get_or_load<F>(&self, loader: F) -> Option<RcRefCell<T>>
    where
        F: FnOnce(&str, &MetaData) -> Option<T>,
    {
        unsafe {
            if let Some(ref data) = *self._cached_data.get() {
                return Some(data.clone());
            }

            let maybe_loaded = loader(&self._resource_name, &self._meta_data);
            if let Some(data) = maybe_loaded {
                let rc_data = newRcRefCell(data);
                if LOG_RESOURCE_LIFECYCLE {
                    log::info!("[RESOURCE LOAD (On-Demand)] {}", self._resource_name);
                }
                *self._state.borrow_mut() = ResourceState::Loaded;
                *self._cached_data.get() = Some(rc_data.clone());
                Some(rc_data)
            } else {
                *self._state.borrow_mut() = ResourceState::Failed;
                None
            }
        }
    }

    pub fn get_or_load_ref<F>(&self, loader: F) -> Option<&RcRefCell<T>>
    where
        F: FnOnce(&str, &MetaData) -> Option<T>,
    {
        unsafe {
            let cached = &mut *self._cached_data.get();
            if let Some(data) = cached {
                return Some(data);
            }

            let maybe_loaded = loader(&self._resource_name, &self._meta_data);
            if let Some(data) = maybe_loaded {
                let rc_data = newRcRefCell(data);
                if LOG_RESOURCE_LIFECYCLE {
                    log::info!("[RESOURCE LOAD (On-Demand)] {}", self._resource_name);
                }
                *self._state.borrow_mut() = ResourceState::Loaded;
                *cached = Some(rc_data);
                return cached.as_ref();
            } else {
                *self._state.borrow_mut() = ResourceState::Failed;
                None
            }
        }
    }

    pub fn check_and_update_gc(&mut self, now: std::time::Instant, timeout: std::time::Duration) -> bool {
        self.check_and_update_gc_with(now, timeout, |_| {})
    }

    pub fn check_and_update_gc_with<F>(
        &mut self,
        now: std::time::Instant,
        timeout: std::time::Duration,
        destructor: F,
    ) -> bool
    where
        F: FnOnce(&RcRefCell<T>),
    {
        if self._is_pinned || self.is_loading() {
            self._unused_start_time = None;
            return false;
        }

        let strong_count = unsafe {
            if let Some(ref data) = *self._cached_data.get() {
                std::rc::Rc::strong_count(data)
            } else {
                0
            }
        };

        if strong_count == 0 {
            self._unused_start_time = None;
            return false;
        }

        // strong_count <= 1: 캐시 엔트리 자신만 보유 중인 상태 (외부 미사용)
        if strong_count <= 1 {
            match self._unused_start_time {
                None => {
                    self._unused_start_time = Some(now);
                    false
                }
                Some(start_time) => {
                    let elapsed = now.duration_since(start_time);
                    if elapsed >= timeout {
                        if LOG_RESOURCE_LIFECYCLE {
                            log::info!(
                                "[RESOURCE GC EVICT] {} (unused for {:.1}s)",
                                self._resource_name,
                                elapsed.as_secs_f32()
                            );
                        }
                        self.unload_with(destructor);
                        self._unused_start_time = None;
                        true
                    } else {
                        false
                    }
                }
            }
        } else {
            self._unused_start_time = None;
            false
        }
    }
}

pub fn update_resource_entry_map_gc<T>(
    entry_map: &mut ResourceEntryMap<T>,
    now: std::time::Instant,
    timeout: std::time::Duration,
) -> u32 {
    update_resource_entry_map_gc_with(entry_map, now, timeout, |_| {})
}

pub fn update_resource_entry_map_gc_with<T, F>(
    entry_map: &mut ResourceEntryMap<T>,
    now: std::time::Instant,
    timeout: std::time::Duration,
    destructor: F,
) -> u32
where
    F: Fn(&RcRefCell<T>),
{
    let mut evicted_count = 0;
    for entry in entry_map.values_mut() {
        if entry.check_and_update_gc_with(now, timeout, &destructor) {
            evicted_count += 1;
        }
    }
    if LOG_RESOURCE_LIFECYCLE && 0 < evicted_count {
        log::info!("[RESOURCE GC SUMMARY] Evicted {} unused resources", evicted_count);
    }
    evicted_count
}

pub type ResourceEntryMap<T> = HashMap<String, ResourceEntry<T>>;

#[derive(Clone)]
pub enum ResourceData {
    None,
    Audio(RcRefCell<AudioData>),
    AudioBank(RcRefCell<AudioBankData>),
}

#[derive(Clone)]
pub struct ResourceInfo {
    pub _resource_name: String,
    pub _resource_data: ResourceData,
    pub _meta_data: MetaData,
}

pub type ResourceDataContainer<T> = HashMap<String, RcRefCell<T>>;
pub type ResourceInfoMap = HashMap<String, ResourceInfo>;

pub type AnimationLayerDataMap = ResourceDataContainer<AnimationLayerData>;
pub type AudioBankDataMap = ResourceDataContainer<AudioBankData>;
pub type EffectDataMap<'a> = ResourceDataContainer<EffectData<'a>>;
pub type FramebufferDataListMap<'a> = ResourceDataContainer<FramebufferData<'a>>;
pub type MaterialDataMap<'a> = ResourceDataContainer<MaterialData<'a>>;
pub type MaterialInstanceDataMap<'a> = ResourceDataContainer<MaterialInstanceData<'a>>;
pub type FontDataMap = ResourceDataContainer<FontData>;
pub type MeshDataMap = ResourceEntryMap<MeshData>;
pub type ModelDataMap<'a> = ResourceEntryMap<ModelData<'a>>;
pub type TextureDataMap = ResourceEntryMap<TextureData>;
pub type RenderPassDataMap<'a> = ResourceDataContainer<RenderPassData<'a>>;
pub type RenderPassDataCreateInfoMap = HashMap<String, RenderPassDataCreateInfo>;
pub type SceneDataCreateInfoMap = ResourceDataContainer<SceneDataCreateInfo>;
pub type DescriptorDataMap<'a> = ResourceDataContainer<DescriptorData<'a>>;
pub type MetaDataMap = ResourceDataContainer<MetaData>;
type LoadImageInfoType = (u32, u32, u32, Vec<u8>, vk::Format);

pub struct EngineResources<'a> {
    pub _relative_resource_file_path_map: HashMap<PathBuf, PathBuf>,
    pub _animation_layer_data_map: AnimationLayerDataMap,
    pub _audio_data_map: ResourceEntryMap<AudioData>,
    pub _audio_bank_data_map: ResourceEntryMap<AudioBankData>,
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
    pub _gc_check_timer: f32,
    pub _gc_timeout_sec: f32,
    pub _callback_load_render_pass_create_infos: *const CallbackLoadRenderPassCreateInfo,
    pub _async_loader: Option<AsyncResourceLoader>,
    pub _vram_budget_bytes: usize,
    pub _enable_budget_gc: bool,
    pub _async_load_time_slice_ms: f32,
    pub _async_load_max_items_per_frame: usize,
    pub _total_gc_evicted_count: u64,
    pub _last_async_processed_count: u32,
    pub _last_async_process_duration_ms: f32,
}

fn walk_directory_recursive(
    dir: &Path,
    include_extensions: &[&str],
    ignore_extensions: &[&str],
    out_contents: &mut Vec<PathBuf>,
) {
    let contents = fs::read_dir(dir).unwrap();
    for content in contents {
        let content = content.unwrap().path();
        if content.is_dir() {
            walk_directory_recursive(&content, include_extensions, ignore_extensions, out_contents);
        } else {
            let ext = content.extension();
            if !ignore_extensions.is_empty()
                && ext.is_some()
                && ignore_extensions.contains(&ext.unwrap().to_str().unwrap())
            {
                continue;
            } else if !include_extensions.is_empty()
                && ext.is_some()
                && !include_extensions.contains(&ext.unwrap().to_str().unwrap())
            {
                continue;
            }

            let path_string = content.to_string_lossy().replace("\\", "/");
            out_contents.push(PathBuf::from(path_string));
        }
    }
}

pub fn walk_directory(dir: &Path, include_extensions: &[&str], ignore_extensions: &[&str]) -> Vec<PathBuf> {
    let mut out_contents: Vec<PathBuf> = Vec::new();
    walk_directory_recursive(dir, include_extensions, ignore_extensions, &mut out_contents);
    out_contents
}

pub fn get_resource_data_must<'a, T>(
    resource_type: &str,
    resource_data_map: &'a ResourceDataContainer<T>,
    resource_name: &str,
) -> &'a RcRefCell<T> {
    if let Some(resource) = resource_data_map.get(resource_name) {
        return resource;
    };
    panic!("not found {}: {:?}", resource_type, resource_name);
}

pub fn get_resource_data<'a, T>(
    resource_type: &str,
    resource_data_map: &'a ResourceDataContainer<T>,
    resource_name: &str,
    default_resource_name: &str,
) -> &'a RcRefCell<T> {
    let maybe_data = resource_data_map.get(resource_name);
    match maybe_data {
        None => {
            log::error!("not found {}: {}", resource_type, resource_name);
            resource_data_map.get(default_resource_name).unwrap()
        }
        _ => maybe_data.unwrap(),
    }
}

pub fn get_resource_entry_data_must<'a, T, F>(
    resource_type: &str,
    entry_map: &'a ResourceEntryMap<T>,
    resource_name: &str,
    loader: F,
) -> &'a RcRefCell<T>
where
    F: FnOnce(&str, &MetaData) -> Option<T>,
{
    if let Some(entry) = entry_map.get(resource_name) {
        if let Some(data) = entry.get_or_load_ref(loader) {
            return data;
        }
        panic!("Failed to load {}: {:?}", resource_type, resource_name);
    }
    panic!("not found {}: {:?}", resource_type, resource_name);
}

pub fn get_resource_entry_data<'a, T, F>(
    resource_type: &str,
    entry_map: &'a ResourceEntryMap<T>,
    resource_name: &str,
    default_resource_name: &str,
    loader: F,
) -> &'a RcRefCell<T>
where
    F: Fn(&str, &MetaData) -> Option<T>,
{
    if let Some(entry) = entry_map.get(resource_name) {
        if let Some(data) = entry.get_or_load_ref(&loader) {
            return data;
        }
        log::error!(
            "Failed to load {}: {}, fallback to {}",
            resource_type,
            resource_name,
            default_resource_name
        );
    } else {
        log::error!(
            "not found {}: {}, fallback to {}",
            resource_type,
            resource_name,
            default_resource_name
        );
    }

    if let Some(default_entry) = entry_map.get(default_resource_name) {
        if let Some(data) = default_entry.get_or_load_ref(&loader) {
            return data;
        }
    }
    panic!("not found default {}: {:?}", resource_type, default_resource_name);
}

pub fn get_resource_name_from_file_path(resource_directory: &PathBuf, resource_file_path: &PathBuf) -> String {
    let mut resource_name = PathBuf::from(resource_file_path.parent().unwrap());
    resource_name.push(resource_file_path.file_stem().unwrap());

    let mut resource_root_path: PathBuf;
    if resource_file_path.starts_with(ENGINE_RESOURCE_PATH) {
        resource_root_path = PathBuf::from(ENGINE_RESOURCE_PATH);
    } else {
        resource_root_path = PathBuf::from(APPLICATION_RESOURCE_PATH);
    }
    resource_root_path.push(resource_directory);

    let resource_name = String::from(system::get_relative_path(&resource_root_path, &resource_name).to_str().unwrap());
    resource_name.replace("\\", "/")
}

pub fn get_unique_resource_name<T>(
    resource_map: &HashMap<String, T>,
    resource_directory: &PathBuf,
    resource_file_path: &PathBuf,
) -> String {
    let resource_name = get_resource_name_from_file_path(resource_directory, resource_file_path);
    system::generate_unique_name(resource_map, &resource_name)
}

pub fn get_resource_file_path(
    resource_root_path: &str,
    resource_directory: &str,
    resource_name: &String,
    resource_ext: &str,
) -> PathBuf {
    let mut resource_file_path: PathBuf = PathBuf::from(resource_root_path);
    resource_file_path.push(resource_directory);
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
            serde_json::from_reader(loaded_contents).unwrap_or_else(|_| MetaData {
                _is_engine_resource: resource_file_path.starts_with(ENGINE_RESOURCE_PATH),
                _meta_file_path: meta_file_path.clone(),
                _resource_version: 0,
                _resource_file_path: resource_file_path.clone(),
                _resource_modify_time: fs::metadata(&resource_file_path)
                    .and_then(|m| m.modified())
                    .unwrap_or(SystemTime::UNIX_EPOCH),
                _source_file_path: source_file_path.clone(),
                _source_modify_time: fs::metadata(&source_file_path)
                    .and_then(|m| m.modified())
                    .unwrap_or(SystemTime::UNIX_EPOCH),
                _source_changed: false,
            })
        } else {
            let resource_modify_time =
                fs::metadata(&resource_file_path).and_then(|m| m.modified()).unwrap_or(SystemTime::UNIX_EPOCH);
            let source_modify_time =
                fs::metadata(&source_file_path).and_then(|m| m.modified()).unwrap_or(SystemTime::UNIX_EPOCH);

            let meta_data = MetaData {
                _is_engine_resource: resource_file_path.starts_with(ENGINE_RESOURCE_PATH),
                _meta_file_path: meta_file_path.clone(),
                _resource_version: 0,
                _resource_file_path: resource_file_path.clone(),
                _resource_modify_time: resource_modify_time,
                _source_file_path: source_file_path.clone(),
                _source_modify_time: source_modify_time,
                _source_changed: false,
            };

            if let Some(parent) = meta_file_path.parent() {
                let _ = fs::create_dir_all(parent);
            }
            if let Ok(mut write_meta_file) = File::create(&meta_file_path) {
                if let Ok(mut write_meta_contents) = serde_json::to_string(&meta_data) {
                    write_meta_contents = write_meta_contents.replace(",\"", ",\n\"");
                    let _ = write_meta_file.write_all(write_meta_contents.as_bytes());
                }
            }
            meta_data
        };

        meta_data
    }
}

impl<'a> EngineResources<'a> {
    pub fn create_engine_resources(
        callback_load_render_pass_create_info: *const CallbackLoadRenderPassCreateInfo,
    ) -> Box<EngineResources<'a>> {
        let mut engine_resource = Box::new(EngineResources {
            _relative_resource_file_path_map: HashMap::new(),
            _animation_layer_data_map: AnimationLayerDataMap::default(),
            _audio_data_map: ResourceEntryMap::new(),
            _audio_bank_data_map: ResourceEntryMap::new(),
            _effect_data_map: EffectDataMap::default(),
            _font_data_map: FontDataMap::default(),
            _mesh_data_map: MeshDataMap::default(),
            _model_data_map: ModelDataMap::default(),
            _texture_data_map: TextureDataMap::default(),
            _framebuffer_data_list_map: FramebufferDataListMap::default(),
            _render_pass_data_map: RenderPassDataMap::default(),
            _render_pass_data_create_info_map: RenderPassDataCreateInfoMap::new(),
            _scene_data_create_infos_map: SceneDataCreateInfoMap::default(),
            _material_data_map: MaterialDataMap::default(),
            _material_instance_data_map: MaterialInstanceDataMap::default(),
            _descriptor_data_map: DescriptorDataMap::default(),
            _gc_check_timer: 0.0,
            _gc_timeout_sec: DEFAULT_RESOURCE_GC_TIMEOUT_SEC,
            _callback_load_render_pass_create_infos: callback_load_render_pass_create_info,
            _async_loader: Some(AsyncResourceLoader::new(2)),
            _vram_budget_bytes: DEFAULT_VRAM_BUDGET_BYTES,
            _enable_budget_gc: true,
            _async_load_time_slice_ms: DEFAULT_ASYNC_LOAD_TIME_SLICE_MS,
            _async_load_max_items_per_frame: DEFAULT_ASYNC_LOAD_MAX_ITEMS,
            _total_gc_evicted_count: 0,
            _last_async_processed_count: 0,
            _last_async_process_duration_ms: 0.0,
        });

        engine_resource.preinitialize_engine_resources();
        engine_resource
    }

    pub fn request_texture_data(&self, texture_name: &str) -> bool {
        if let Some(entry) = self._texture_data_map.get(texture_name) {
            if entry.is_loaded() || entry.is_loading() {
                return false;
            }
            entry.set_state(ResourceState::Loading);
            if let Some(ref loader) = self._async_loader {
                return loader.send_request(AsyncLoadRequest::Texture {
                    _texture_name: texture_name.to_string(),
                    _meta_data: entry._meta_data.clone(),
                });
            }
        }
        false
    }

    pub fn request_mesh_data(&self, mesh_name: &str) -> bool {
        if let Some(entry) = self._mesh_data_map.get(mesh_name) {
            if entry.is_loaded() || entry.is_loading() {
                return false;
            }
            entry.set_state(ResourceState::Loading);
            if let Some(ref loader) = self._async_loader {
                return loader.send_request(AsyncLoadRequest::Mesh {
                    _mesh_name: mesh_name.to_string(),
                    _meta_data: entry._meta_data.clone(),
                });
            }
        }
        false
    }

    pub fn request_model_resources(&self, model_data: &ModelData) {
        if !model_data._mesh_name.is_empty() {
            self.request_mesh_data(&model_data._mesh_name);
        } else {
            let mesh_name = &model_data._mesh_data.borrow()._name;
            self.request_mesh_data(mesh_name);
        }

        for mat_inst in model_data._material_instance_data_list.iter() {
            for (_key, value) in mat_inst.borrow()._material_parameters.iter() {
                if let serde_json::Value::String(texture_name) = value {
                    self.request_texture_data(texture_name);
                }
            }
        }
    }

    pub fn process_async_loading(&mut self, renderer_context: &RendererContext, max_items: usize) -> u32 {
        let time_slice_ms = self._async_load_time_slice_ms;
        self.process_async_loading_with_budget(renderer_context, max_items, time_slice_ms)
    }

    pub fn process_async_loading_with_budget(
        &mut self,
        renderer_context: &RendererContext,
        max_items: usize,
        time_slice_ms: f32,
    ) -> u32 {
        let loader = match self._async_loader {
            Some(ref loader) => loader,
            None => return 0,
        };

        let start_time = std::time::Instant::now();
        let time_budget = std::time::Duration::from_secs_f32(time_slice_ms * 0.001);
        let mut processed_count = 0;

        while processed_count < max_items {
            if processed_count > 0 && start_time.elapsed() >= time_budget {
                break;
            }

            let result = match loader.try_recv_result() {
                Some(res) => res,
                None => break,
            };

            match result {
                AsyncLoadResult::Texture { _texture_name, _result } => match _result {
                    Ok(texture_data) => {
                        let already_loaded =
                            self._texture_data_map.get(&_texture_name).map(|entry| entry.is_loaded()).unwrap_or(false);
                        if !already_loaded {
                            let texture_create_info = TextureCreateInfo {
                                _texture_name: texture_data._texture_name.clone(),
                                _texture_width: texture_data._image_width,
                                _texture_height: texture_data._image_height,
                                _texture_layers: texture_data._image_layers,
                                _texture_format: texture_data._image_format,
                                _texture_min_filter: texture_data._min_filter,
                                _texture_mag_filter: texture_data._mag_filter,
                                _texture_wrap_mode: texture_data._wrap_mode,
                                _texture_view_type: texture_data._image_view_type,
                                _texture_initial_data_list: texture_data._image_data,
                                _enable_mipmap: texture_data._enable_mipmap,
                                _enable_anisotropy: false,
                                ..Default::default()
                            };
                            let created = renderer_context.create_texture(&texture_create_info);
                            let created_rc = newRcRefCell(created);
                            if let Some(entry) = self._texture_data_map.get(&_texture_name) {
                                entry.set_loaded_data(created_rc.clone());
                                if LOG_RESOURCE_LIFECYCLE {
                                    log::info!("[ASYNC LOAD COMPLETE] Texture: {}", _texture_name);
                                }
                            }
                            self.notify_texture_loaded(
                                renderer_context.get_device(),
                                &_texture_name,
                                &created_rc.borrow(),
                            );
                        }
                        processed_count += 1;
                    }
                    Err(err) => {
                        log::error!("[ASYNC LOAD ERROR] Texture '{}': {}", _texture_name, err);
                        if let Some(entry) = self._texture_data_map.get(&_texture_name) {
                            if !entry.is_loaded() {
                                entry.set_state(ResourceState::Failed);
                            }
                        }
                    }
                },
                AsyncLoadResult::Mesh { _mesh_name, _result } => match _result {
                    Ok(mesh_data) => {
                        let already_loaded =
                            self._mesh_data_map.get(&_mesh_name).map(|entry| entry.is_loaded()).unwrap_or(false);
                        if !already_loaded {
                            let mut geometry_data_list: Vec<RcRefCell<GeometryData>> = Vec::new();
                            for (i, geometry_create_info) in
                                mesh_data._create_info._geometry_create_infos.iter().enumerate()
                            {
                                let geometry_name = format!("{}_{}", _mesh_name, i);
                                let geometry_data =
                                    renderer_context.create_geometry_buffer(&geometry_name, geometry_create_info);
                                geometry_data_list.push(newRcRefCell(geometry_data));
                            }
                            let created =
                                MeshData::create_mesh_data(&_mesh_name, mesh_data._create_info, geometry_data_list);
                            if let Some(entry) = self._mesh_data_map.get(&_mesh_name) {
                                entry.set_loaded_data(newRcRefCell(created));
                                if LOG_RESOURCE_LIFECYCLE {
                                    log::info!("[ASYNC LOAD COMPLETE] Mesh: {}", _mesh_name);
                                }
                            }
                        }
                        processed_count += 1;
                    }
                    Err(err) => {
                        log::error!("[ASYNC LOAD ERROR] Mesh '{}': {}", _mesh_name, err);
                        if let Some(entry) = self._mesh_data_map.get(&_mesh_name) {
                            if !entry.is_loaded() {
                                entry.set_state(ResourceState::Failed);
                            }
                        }
                    }
                },
            }
        }

        let elapsed_ms = start_time.elapsed().as_secs_f32() * 1000.0;
        self._last_async_processed_count = processed_count as u32;
        self._last_async_process_duration_ms = elapsed_ms;

        processed_count as u32
    }

    pub fn update_async_loading(&mut self) -> u32 {
        self.update_async_loading_with_mode(false)
    }

    pub fn update_async_loading_with_mode(&mut self, boost_loading: bool) -> u32 {
        let renderer_context = get_renderer_context();
        let (max_items, time_slice_ms) = if boost_loading {
            (LOADING_SCENE_ASYNC_MAX_ITEMS, LOADING_SCENE_ASYNC_TIME_SLICE_MS)
        } else {
            (self._async_load_max_items_per_frame, self._async_load_time_slice_ms)
        };
        self.process_async_loading_with_budget(renderer_context, max_items, time_slice_ms)
    }

    pub fn pin_texture_data(&mut self, texture_name: &str) {
        if let Some(entry) = self._texture_data_map.get_mut(texture_name) {
            entry.pin();
        }
    }

    pub fn pin_mesh_data(&mut self, mesh_name: &str) {
        if let Some(entry) = self._mesh_data_map.get_mut(mesh_name) {
            entry.pin();
        }
    }

    pub fn pin_model_data(&mut self, model_name: &str) {
        if let Some(entry) = self._model_data_map.get_mut(model_name) {
            entry.pin();
        }
    }

    pub fn register_model_data(&mut self, model_name: String, model_data: RcRefCell<ModelData<'a>>) {
        let entry = ResourceEntry::new_pinned(model_name.clone(), MetaData::default());
        entry.set_loaded_data(model_data);
        self._model_data_map.insert(model_name, entry);
    }

    pub fn preinitialize_engine_resources(&mut self) {
        let resource_list_file_path: PathBuf = PathBuf::from(APPLICATION_RESOURCE_PATH).join("resources.txt");

        if unsafe { DEVELOPMENT } {
            // update engine resources
            self.update_engine_resources();

            if !resource_list_file_path.exists() {
                log::info!("Creating initial resources.txt file");
                let include_extensions: &[&str] = &[];
                let ignore_extensions: &[&str] = &["log"];
                let mut application_resource_files = walk_directory(
                    &Path::new(APPLICATION_RESOURCE_PATH),
                    include_extensions,
                    ignore_extensions,
                );
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
                            .write_all(format!("{}\n", file_path.to_str().unwrap()).as_bytes())
                            .expect("Failed to write");
                    }
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
                relative_resource_file_path =
                    relative_resource_file_path.strip_prefix(ENGINE_RESOURCE_PATH).unwrap().to_path_buf();
            } else {
                relative_resource_file_path =
                    relative_resource_file_path.strip_prefix(APPLICATION_RESOURCE_PATH).unwrap().to_path_buf();
            }

            if self._relative_resource_file_path_map.get(&relative_resource_file_path).is_none()
                || false == is_engine_resource
            {
                self._relative_resource_file_path_map.insert(relative_resource_file_path.clone(), resource_file_path);
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
            let ignore_extensions: &[&str] = &["log"];
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
            let engine_font_texture_source_path =
                PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(FONT_TEXTURE_DIRECTORY);
            let engine_shader_cache_source_path =
                PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(SHADER_CACHE_DIRECTORY);
            let include_extensions: &[&str] = &[];
            let ignore_extensions: &[&str] = &["log"];
            let engine_resource_source_files =
                walk_directory(&engine_resource_source_path, include_extensions, ignore_extensions);
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
            let relative_file_path: PathBuf = file_path.strip_prefix(ENGINE_RESOURCE_PATH).unwrap().to_path_buf();
            let engine_source_file_path: PathBuf = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH).join(relative_file_path);
            if false == engine_source_file_path.is_file() {
                fs::remove_file(&file_path).expect("failed to remove file");
            } else {
                let modified_time: SystemTime = fs::metadata(file_path).unwrap().modified().unwrap();
                let source_modified_time: SystemTime =
                    fs::metadata(engine_source_file_path).unwrap().modified().unwrap();
                if modified_time < source_modified_time {
                    fs::remove_file(&file_path).expect("failed to remove file");
                }
            }
        }

        // copy engine resource source file -> engine resource file
        for (_, source_file_path) in engine_resource_source_file_map.iter() {
            let relative_source_file_path: PathBuf =
                source_file_path.strip_prefix(ENGINE_RESOURCE_SOURCE_PATH).unwrap().to_path_buf();
            let engine_file_path: PathBuf = PathBuf::from(ENGINE_RESOURCE_PATH).join(relative_source_file_path);
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

    pub fn load_engine_resources(&mut self) {
        log::info!("load_engine_resources");
        let renderer_context = get_renderer_context();
        let is_reload: bool = false;

        let step_timer = std::time::Instant::now();
        self.load_texture_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_texture_data_list (catalog & meta): {:.2}ms (loaded: {}/{})",
            step_timer.elapsed().as_secs_f64() * 1000.0,
            self._texture_data_map.values().filter(|e| e.is_loaded()).count(),
            self._texture_data_map.len()
        );

        let step_timer = std::time::Instant::now();
        self.load_font_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_font_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        self.load_render_pass_data_create_infos(renderer_context);
        log::info!(
            "[StartupTimer]   load_render_pass_data_create_infos: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        self.load_render_pass_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_render_pass_data_list (shaders & pipelines): {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        self.load_framebuffer_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_framebuffer_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        self.load_material_data_list();
        log::info!(
            "[StartupTimer]   load_material_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        let tex_before = self._texture_data_map.values().filter(|e| e.is_loaded()).count();
        self.load_material_instance_data_list(renderer_context, is_reload);
        let tex_after = self._texture_data_map.values().filter(|e| e.is_loaded()).count();
        log::info!(
            "[StartupTimer]   load_material_instance_data_list: {:.2}ms (synchronously loaded {} textures! loaded: {} -> {}/{})",
            step_timer.elapsed().as_secs_f64() * 1000.0,
            tex_after.saturating_sub(tex_before),
            tex_before,
            tex_after,
            self._texture_data_map.len()
        );

        let step_timer = std::time::Instant::now();
        self.load_mesh_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_mesh_data_list (catalog & meta): {:.2}ms (loaded: {}/{})",
            step_timer.elapsed().as_secs_f64() * 1000.0,
            self._mesh_data_map.values().filter(|e| e.is_loaded()).count(),
            self._mesh_data_map.len()
        );

        let step_timer = std::time::Instant::now();
        self.load_model_data_list();
        log::info!(
            "[StartupTimer]   load_model_data_list (catalog & meta): {:.2}ms (loaded: {}/{})",
            step_timer.elapsed().as_secs_f64() * 1000.0,
            self._model_data_map.values().filter(|e| e.is_loaded()).count(),
            self._model_data_map.len()
        );

        let step_timer = std::time::Instant::now();
        self.load_animation_layer_data_list();
        log::info!(
            "[StartupTimer]   load_animation_layer_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        self.load_audio_data_list();
        log::info!(
            "[StartupTimer]   load_audio_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );

        let step_timer = std::time::Instant::now();
        let mesh_before = self._mesh_data_map.values().filter(|e| e.is_loaded()).count();
        self.load_effect_data_list();
        let mesh_after = self._mesh_data_map.values().filter(|e| e.is_loaded()).count();
        log::info!(
            "[StartupTimer]   load_effect_data_list: {:.2}ms (synchronously loaded {} meshes! loaded: {} -> {}/{})",
            step_timer.elapsed().as_secs_f64() * 1000.0,
            mesh_after.saturating_sub(mesh_before),
            mesh_before,
            mesh_after,
            self._mesh_data_map.len()
        );

        let step_timer = std::time::Instant::now();
        self.load_scene_data_list(renderer_context);
        log::info!(
            "[StartupTimer]   load_scene_data_list: {:.2}ms",
            step_timer.elapsed().as_secs_f64() * 1000.0
        );
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

    pub fn update_garbage_collection(&mut self, delta_time: f64) -> u32 {
        self._gc_check_timer += delta_time as f32;
        if self._gc_check_timer < RESOURCE_GC_CHECK_INTERVAL_SEC {
            return 0;
        }
        self._gc_check_timer = 0.0;

        let now = std::time::Instant::now();
        let timeout = std::time::Duration::from_secs_f32(self._gc_timeout_sec);

        let mut evicted = 0;
        evicted += update_resource_entry_map_gc(&mut self._audio_data_map, now, timeout);
        evicted += update_resource_entry_map_gc(&mut self._audio_bank_data_map, now, timeout);
        evicted += update_resource_entry_map_gc(&mut self._model_data_map, now, timeout);

        let renderer_context = get_renderer_context();

        // GPU 리소스(메시, 텍스처) 중 실제로 GC 만료 대상이 있는지 사전 검사
        let has_expired_mesh = self._mesh_data_map.values().any(|entry| {
            if entry.is_pinned() {
                return false;
            }
            let strong_count = unsafe {
                if let Some(ref data) = *entry._cached_data.get() {
                    std::rc::Rc::strong_count(data)
                } else {
                    0
                }
            };
            if strong_count == 1 {
                if let Some(start_time) = entry._unused_start_time {
                    return now.duration_since(start_time) >= timeout;
                }
            }
            false
        });

        let has_expired_texture = self._texture_data_map.values().any(|entry| {
            if entry.is_pinned() {
                return false;
            }
            let strong_count = unsafe {
                if let Some(ref data) = *entry._cached_data.get() {
                    std::rc::Rc::strong_count(data)
                } else {
                    0
                }
            };
            if strong_count == 1 {
                if let Some(start_time) = entry._unused_start_time {
                    return now.duration_since(start_time) >= timeout;
                }
            }
            false
        });

        // 만료 대상이 있을 때만 device_wait_idle()을 호출하여 In-Flight GPU 커맨드 버퍼 완료 대기
        if has_expired_mesh || has_expired_texture {
            renderer_context.device_wait_idle();
        }

        let mut gpu_evicted = 0;
        gpu_evicted += update_resource_entry_map_gc_with(&mut self._mesh_data_map, now, timeout, |mesh_data| {
            for geometry_data in (*mesh_data).borrow().get_geometry_data_list() {
                renderer_context.destroy_geometry_buffer(&geometry_data.borrow());
            }
        });
        gpu_evicted += update_resource_entry_map_gc_with(&mut self._texture_data_map, now, timeout, |texture_data| {
            renderer_context.destroy_texture(&(*texture_data).borrow());
        });

        evicted += gpu_evicted;

        // VRAM Budget 초과 시 LRU 조기 회수 실행
        let budget_evicted = self.evict_resources_to_fit_budget(renderer_context);
        evicted += budget_evicted;

        self._total_gc_evicted_count += (evicted - budget_evicted) as u64;

        if LOG_RESOURCE_LIFECYCLE && 0 < evicted {
            log::info!("[EngineResources GC] Evicted total {} unused resources", evicted);
        }
        evicted
    }

    pub fn evict_resources_to_fit_budget(&mut self, renderer_context: &RendererContext) -> u32 {
        if !self._enable_budget_gc {
            return 0;
        }

        let current_vram = self.get_total_vram_usage_bytes();
        if current_vram <= self._vram_budget_bytes {
            return 0;
        }

        let target_bytes = (self._vram_budget_bytes * 9) / 10;
        let mut bytes_to_free = current_vram.saturating_sub(target_bytes);
        let now = std::time::Instant::now();

        struct EvictCandidate {
            _name: String,
            _is_texture: bool,
            _unused_time: std::time::Instant,
            _mem_bytes: usize,
        }

        let mut candidates = Vec::new();

        for (name, entry) in self._texture_data_map.iter() {
            if entry.is_pinned() || entry.is_loading() {
                continue;
            }
            let strong_count = unsafe {
                match *entry._cached_data.get() {
                    Some(ref data) => std::rc::Rc::strong_count(data),
                    None => 0,
                }
            };
            if strong_count == 1 {
                if let Some(tex) = entry.get_loaded_data() {
                    let mem = tex.borrow().get_memory_usage_bytes();
                    let unused_time = entry._unused_start_time.unwrap_or(now);
                    candidates.push(EvictCandidate {
                        _name: name.clone(),
                        _is_texture: true,
                        _unused_time: unused_time,
                        _mem_bytes: mem,
                    });
                }
            }
        }

        for (name, entry) in self._mesh_data_map.iter() {
            if entry.is_pinned() || entry.is_loading() {
                continue;
            }
            let strong_count = unsafe {
                match *entry._cached_data.get() {
                    Some(ref data) => std::rc::Rc::strong_count(data),
                    None => 0,
                }
            };
            if strong_count == 1 {
                if let Some(mesh) = entry.get_loaded_data() {
                    let mem = mesh.borrow().get_memory_usage_bytes();
                    let unused_time = entry._unused_start_time.unwrap_or(now);
                    candidates.push(EvictCandidate {
                        _name: name.clone(),
                        _is_texture: false,
                        _unused_time: unused_time,
                        _mem_bytes: mem,
                    });
                }
            }
        }

        if candidates.is_empty() {
            return 0;
        }

        // LRU 정렬 (가장 먼저 미사용된 리소스부터 오름차순)
        candidates.sort_by_key(|c| c._unused_time);

        // GPU 리소스 안전 파괴를 위한 펜스 대기
        renderer_context.device_wait_idle();

        let mut evicted_count = 0;
        let mut freed_bytes = 0;

        for cand in candidates {
            if bytes_to_free == 0 {
                break;
            }

            if cand._is_texture {
                if let Some(entry) = self._texture_data_map.get_mut(&cand._name) {
                    entry.unload_with(|texture_data| {
                        renderer_context.destroy_texture(&(*texture_data).borrow());
                    });
                    entry._unused_start_time = None;
                    evicted_count += 1;
                    freed_bytes += cand._mem_bytes;
                    bytes_to_free = bytes_to_free.saturating_sub(cand._mem_bytes);
                }
            } else {
                if let Some(entry) = self._mesh_data_map.get_mut(&cand._name) {
                    entry.unload_with(|mesh_data| {
                        for geometry_data in (*mesh_data).borrow().get_geometry_data_list() {
                            renderer_context.destroy_geometry_buffer(&geometry_data.borrow());
                        }
                    });
                    entry._unused_start_time = None;
                    evicted_count += 1;
                    freed_bytes += cand._mem_bytes;
                    bytes_to_free = bytes_to_free.saturating_sub(cand._mem_bytes);
                }
            }
        }

        if evicted_count > 0 {
            self._total_gc_evicted_count += evicted_count as u64;
            log::warn!(
                "[RESOURCE BUDGET GC] VRAM exceeded budget ({:.1}MB / {:.1}MB). Evicted {} resources, freed {:.1}MB",
                current_vram as f32 / (1024.0 * 1024.0),
                self._vram_budget_bytes as f32 / (1024.0 * 1024.0),
                evicted_count,
                freed_bytes as f32 / (1024.0 * 1024.0)
            );
        }

        evicted_count
    }

    pub fn get_texture_memory_usage(&self) -> (usize, usize, usize) {
        let total_count = self._texture_data_map.len();
        let mut loaded_count = 0;
        let mut total_bytes = 0;
        for entry in self._texture_data_map.values() {
            if let Some(tex) = entry.get_loaded_data() {
                loaded_count += 1;
                total_bytes += tex.borrow().get_memory_usage_bytes();
            }
        }
        (loaded_count, total_count, total_bytes)
    }

    pub fn get_mesh_memory_usage(&self) -> (usize, usize, usize) {
        let total_count = self._mesh_data_map.len();
        let mut loaded_count = 0;
        let mut total_bytes = 0;
        for entry in self._mesh_data_map.values() {
            if let Some(mesh) = entry.get_loaded_data() {
                loaded_count += 1;
                total_bytes += mesh.borrow().get_memory_usage_bytes();
            }
        }
        (loaded_count, total_count, total_bytes)
    }

    pub fn get_model_status(&self) -> (usize, usize) {
        let total_count = self._model_data_map.len();
        let loaded_count = self._model_data_map.values().filter(|e| e.is_loaded()).count();
        (loaded_count, total_count)
    }

    pub fn get_total_vram_usage_bytes(&self) -> usize {
        let (_, _, tex_bytes) = self.get_texture_memory_usage();
        let (_, _, mesh_bytes) = self.get_mesh_memory_usage();
        tex_bytes + mesh_bytes
    }

    pub fn get_pending_async_count(&self) -> usize {
        match self._async_loader {
            Some(ref loader) => loader.get_pending_count(),
            None => 0,
        }
    }

    pub fn get_resource_metrics(&self) -> ResourceStreamingMetrics {
        let (loaded_tex, total_tex, tex_bytes) = self.get_texture_memory_usage();
        let (loaded_mesh, total_mesh, mesh_bytes) = self.get_mesh_memory_usage();
        let (loaded_models, total_models) = self.get_model_status();
        let total_vram = tex_bytes + mesh_bytes;
        let pending = self.get_pending_async_count();

        ResourceStreamingMetrics {
            _loaded_textures: loaded_tex,
            _total_textures: total_tex,
            _texture_memory_bytes: tex_bytes,
            _loaded_meshes: loaded_mesh,
            _total_meshes: total_mesh,
            _mesh_memory_bytes: mesh_bytes,
            _loaded_models: loaded_models,
            _total_models: total_models,
            _total_vram_bytes: total_vram,
            _vram_budget_bytes: self._vram_budget_bytes,
            _pending_async_requests: pending,
            _last_async_processed_count: self._last_async_processed_count,
            _last_async_process_duration_ms: self._last_async_process_duration_ms,
            _total_gc_evicted_count: self._total_gc_evicted_count,
        }
    }

    pub fn get_vram_budget_bytes(&self) -> usize {
        self._vram_budget_bytes
    }

    pub fn set_vram_budget_bytes(&mut self, budget_bytes: usize) {
        self._vram_budget_bytes = budget_bytes;
    }

    pub fn is_budget_gc_enabled(&self) -> bool {
        self._enable_budget_gc
    }

    pub fn set_budget_gc_enabled(&mut self, enable: bool) {
        self._enable_budget_gc = enable;
    }

    pub fn get_async_load_time_slice_ms(&self) -> f32 {
        self._async_load_time_slice_ms
    }

    pub fn set_async_load_time_slice_ms(&mut self, time_slice_ms: f32) {
        self._async_load_time_slice_ms = time_slice_ms;
    }

    pub fn get_async_load_max_items_per_frame(&self) -> usize {
        self._async_load_max_items_per_frame
    }

    pub fn set_async_load_max_items_per_frame(&mut self, max_items: usize) {
        self._async_load_max_items_per_frame = max_items;
    }

    pub fn get_total_gc_evicted_count(&self) -> u64 {
        self._total_gc_evicted_count
    }

    pub fn get_gc_timeout_sec(&self) -> f32 {
        self._gc_timeout_sec
    }

    pub fn set_gc_timeout_sec(&mut self, timeout_sec: f32) {
        self._gc_timeout_sec = timeout_sec;
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
                    || (ext.is_some() && extensions.contains(&ext.unwrap().to_str().unwrap().to_lowercase().as_str()))
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
        let animation_layer_files: Vec<PathBuf> =
            self.collect_resources(&animation_layer_directory, &[EXT_ANIMATION_LAYER]);
        for animation_layer_file in animation_layer_files {
            let animation_layer_name = get_unique_resource_name(
                &self._animation_layer_data_map,
                &animation_layer_directory,
                &animation_layer_file,
            );
            let loaded_contents = system::load(&animation_layer_file);
            let contents: AnimationLayerData =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            self._animation_layer_data_map.insert(animation_layer_name.clone(), newRcRefCell(contents));
        }
    }

    pub fn unload_animation_layer_data_list(&mut self) {
        self._animation_layer_data_map.clear();
    }

    pub fn has_animation_layer_data(&self, resource_name: &str) -> bool {
        self._animation_layer_data_map.contains_key(resource_name)
    }

    pub fn get_animation_layer_data(&self, resource_name: &str) -> &RcRefCell<AnimationLayerData> {
        get_resource_data(
            "animation_layer_data",
            &self._animation_layer_data_map,
            resource_name,
            DEFAULT_ANIMATION_LAYER_NAME,
        )
    }

    // Audio Data
    pub fn load_audio_data_list(&mut self) {
        log::info!("    load_audio_data_list");
        let audio_directory = PathBuf::from(AUDIO_DIRECTORY);
        let audio_bank_directory = PathBuf::from(AUDIO_BANK_DIRECTORY);

        // load audio data_list
        let audio_data_files: Vec<PathBuf> = self.collect_resources(&audio_directory, &EXT_AUDIO_SOURCE);
        for audio_data_file in audio_data_files.iter() {
            let audio_data_name = get_unique_resource_name(&self._audio_data_map, &audio_directory, &audio_data_file);
            let meta_data = MetaData::new(&audio_data_file, &audio_data_file);
            let audio_resource_entry = ResourceEntry::new(audio_data_name.clone(), meta_data);
            self._audio_data_map.insert(audio_data_name, audio_resource_entry);
        }

        // create default audio bank data
        {
            let mut default_audio_bank_file_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
            default_audio_bank_file_path.push(&audio_bank_directory);
            default_audio_bank_file_path.push(&DEFAULT_AUDIO_BANK_NAME);
            default_audio_bank_file_path.set_extension(EXT_AUDIO_BANK);
            if unsafe { DEVELOPMENT } {
                if false == default_audio_bank_file_path.is_file() {
                    let mut default_audio_bank_data_create_info = AudioBankCreateInfo::default();
                    default_audio_bank_data_create_info._audio_names.push(DEFAULT_AUDIO_NAME.to_string());
                    log::info!("default_audio_bank_file_path: {:?}", default_audio_bank_file_path);
                    let mut write_file = File::create(&default_audio_bank_file_path).expect("Failed to create file");
                    let mut write_contents: String =
                        serde_json::to_string(&default_audio_bank_data_create_info).expect("Failed to serialize.");
                    write_contents = write_contents.replace(",\"", ",\n\"");
                    write_file.write_all(write_contents.as_bytes()).expect("Failed to write");
                }
            }
        }

        // load audio bank data_list
        let audio_bank_data_files: Vec<PathBuf> = self.collect_resources(&audio_bank_directory, &[EXT_AUDIO_BANK]);
        for audio_bank_data_file in audio_bank_data_files {
            let audio_bank_data_name =
                get_unique_resource_name(&self._audio_bank_data_map, &audio_bank_directory, &audio_bank_data_file);
            let meta_data = MetaData::new(&audio_bank_data_file, &audio_bank_data_file);
            let audio_bank_resource_entry = ResourceEntry::new(audio_bank_data_name.clone(), meta_data);
            self._audio_bank_data_map.insert(audio_bank_data_name.clone(), audio_bank_resource_entry);
        }
    }

    pub fn unload_audio_data_list(&mut self) {
        self._audio_bank_data_map.clear();
        self._audio_data_map.clear();
    }

    pub fn has_audio_data(&self, resource_name: &str) -> bool {
        self._audio_data_map.contains_key(resource_name)
    }

    pub fn get_audio_data(&self, resource_name: &str) -> ResourceData {
        if let Some(entry) = self._audio_data_map.get(resource_name) {
            if let Some(audio_data) = entry.get_or_load(|audio_name, meta| {
                if meta._resource_file_path.is_file() {
                    match sdl2::mixer::Chunk::from_file(&meta._resource_file_path) {
                        Ok(chunk) => Some(AudioData {
                            _audio_name: audio_name.to_string(),
                            _sound_chunk: chunk,
                        }),
                        Err(err) => {
                            log::error!("Failed to load audio {:?}: {:?}", meta._resource_file_path, err);
                            None
                        }
                    }
                } else {
                    log::error!("Audio file does not exist: {:?}", meta._resource_file_path);
                    None
                }
            }) {
                return ResourceData::Audio(audio_data);
            }
        }
        ResourceData::None
    }

    pub fn has_audio_bank_data(&self, resource_name: &str) -> bool {
        self._audio_bank_data_map.contains_key(resource_name)
    }

    pub fn get_audio_bank_data(&self, resource_name: &str) -> ResourceData {
        if let Some(entry) = self._audio_bank_data_map.get(resource_name) {
            if let Some(audio_bank_data) = entry.get_or_load(|audio_bank_name, meta| {
                if meta._resource_file_path.is_file() {
                    let loaded_contents = system::load(&meta._resource_file_path);
                    match serde_json::from_reader(loaded_contents) {
                        Ok(audio_bank_create_info) => {
                            let audio_bank_create_info: AudioBankCreateInfo = audio_bank_create_info;
                            let mut audio_data_list: Vec<RcRefCell<AudioData>> = Vec::new();
                            for audio_name in audio_bank_create_info._audio_names.iter() {
                                if let ResourceData::Audio(audio_data) = self.get_audio_data(audio_name) {
                                    audio_data_list.push(audio_data);
                                }
                            }
                            Some(AudioBankData {
                                _audio_bank_name: audio_bank_name.to_string(),
                                _audio_data_list: audio_data_list,
                            })
                        }
                        Err(err) => {
                            log::error!(
                                "Failed to deserialize audio bank {:?}: {:?}",
                                meta._resource_file_path,
                                err
                            );
                            None
                        }
                    }
                } else {
                    log::error!("Audio bank file does not exist: {:?}", meta._resource_file_path);
                    None
                }
            }) {
                return ResourceData::AudioBank(audio_bank_data);
            }
        }
        ResourceData::None
    }

    // EffectData
    pub fn load_effect_data_list(&mut self) {
        log::info!("    load_effect_data_list");
        // create default effect
        let mut default_effect_file_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
        default_effect_file_path.push(EFFECT_DIRECTORY);
        default_effect_file_path.push(DEFAULT_EFFECT_NAME);
        default_effect_file_path.set_extension(EXT_EFFECT);
        if unsafe { DEVELOPMENT } {
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
                let mut write_file = File::create(&default_effect_file_path).expect("Failed to create file");
                let mut write_contents: String =
                    serde_json::to_string(&default_effect_data_create_info).expect("Failed to serialize.");
                write_contents = write_contents.replace(",\"", ",\n\"");
                write_file.write_all(write_contents.as_bytes()).expect("Failed to write");
            }
        }

        let effect_files: Vec<PathBuf> = self.collect_resources(&Path::new(EFFECT_DIRECTORY), &[EXT_EFFECT]);
        for effect_file in effect_files {
            let effect_data_name =
                get_unique_resource_name(&self._effect_data_map, &PathBuf::from(EFFECT_DIRECTORY), &effect_file);
            let loaded_contents = system::load(&effect_file);
            let effect_data_create_info: EffectDataCreateInfo =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let emitter_data_list = effect_data_create_info
                ._emitter_data_create_infos
                .iter()
                .map(|emitter_data_create_info| {
                    let material_instance_data =
                        self.get_material_instance_data(&emitter_data_create_info._material_instance_name);
                    let mesh_data = self.get_mesh_data(&emitter_data_create_info._mesh_name);
                    EmitterData::create_emitter_data(
                        emitter_data_create_info,
                        material_instance_data.clone(),
                        mesh_data.clone(),
                    )
                })
                .collect();
            let effect_data =
                EffectData::create_effect_data(&effect_data_name, &effect_data_create_info, emitter_data_list);
            self._effect_data_map.insert(effect_data_name.clone(), newRcRefCell(effect_data));
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
        get_resource_data(
            "effect_data",
            &self._effect_data_map,
            resource_name,
            DEFAULT_EFFECT_NAME,
        )
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
        fs::create_dir_all(font_file_path.parent().unwrap()).expect("Failed to create directories.");

        let mut font_texture_file_path: PathBuf = resource_root_path.clone();
        font_texture_file_path.push(font_texture_directory);
        font_texture_file_path.push(&font_data_name);
        font_texture_file_path.set_extension(EXT_FONT_TEXTURE);
        fs::create_dir_all(font_texture_file_path.parent().unwrap()).expect("Failed to create directories.");

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
        let write_contents: String = serde_json::to_string(&font_data_create_info).expect("Failed to serialize.");
        write_file.write_all(write_contents.as_bytes()).expect("Failed to write");
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
                let font_name =
                    get_unique_resource_name(&self._font_data_map, &font_source_directory, &font_source_file);
                let font_data_name = format!("{}_{}", font_name, unicode_block_key);
                let font_texture_name = format!("fonts/{}_{}", font_name, unicode_block_key);

                // Gets FontDataCreateInfo and also creates font texture files if needed.
                let maybe_font_file = font_file_map.get(&font_data_name);
                let font_data_create_info: FontDataCreateInfo = if unsafe { DEVELOPMENT }
                    && (maybe_font_file.is_none() || false == self.has_texture_data(&font_texture_name))
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
                    let texture_data = newRcRefCell(renderer_context.create_texture(&texture_create_info));
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
                        (texture_data.borrow()._image_width / font_data_create_info._count_of_side) as f32,
                        (texture_data.borrow()._image_height / font_data_create_info._count_of_side) as f32,
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
        get_resource_data_must("font_data", &self._font_data_map, DEFAULT_FONT_NAME)
    }

    pub fn get_font_data(&self, resource_name: &str) -> &RcRefCell<FontData> {
        get_resource_data_must("font_data", &self._font_data_map, resource_name)
    }

    // ModelData
    pub fn load_model_data(model_name: &str, meta_data: &MetaData) -> Option<ModelData<'a>> {
        let file_path = if meta_data._resource_file_path.is_file() {
            &meta_data._resource_file_path
        } else if meta_data._source_file_path.is_file() {
            &meta_data._source_file_path
        } else {
            log::error!("Model file not found: {:?}", meta_data);
            return None;
        };

        let loaded_contents = system::load(file_path);
        let model_data_info: ModelDataInfo = match serde_json::from_reader(loaded_contents) {
            Ok(info) => info,
            Err(e) => {
                log::error!("Failed to deserialize model {}: {:?}", model_name, e);
                return None;
            }
        };

        let engine_resources = get_engine_resources();
        let mesh_name = &model_data_info._mesh;
        let mesh_data = if !mesh_name.is_empty() {
            if let Some(entry) = engine_resources._mesh_data_map.get(mesh_name) {
                if let Some(loaded) = entry.get_loaded_data() {
                    loaded
                } else {
                    engine_resources.request_mesh_data(mesh_name);
                    engine_resources.get_mesh_data(DEFAULT_MESH_NAME).clone()
                }
            } else {
                engine_resources.get_mesh_data(DEFAULT_MESH_NAME).clone()
            }
        } else {
            engine_resources.get_mesh_data(DEFAULT_MESH_NAME).clone()
        };
        let material_instance_count = model_data_info._material_instances.len();
        let geometry_data_count = mesh_data.borrow().get_geometry_data_count();
        let num_materials = geometry_data_count.max(material_instance_count).max(1);

        let material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>> = (0..num_materials)
            .map(|index| {
                if 0 < material_instance_count {
                    let material_index = 0.max(index.min(material_instance_count - 1));
                    engine_resources
                        .get_material_instance_data(&model_data_info._material_instances[material_index])
                        .clone()
                } else {
                    engine_resources.get_material_instance_data(DEFAULT_MATERIAL_INSTANCE_NAME).clone()
                }
            })
            .collect();

        Some(ModelData::create_model_data(
            &model_name.to_string(),
            &mesh_data,
            material_instance_data_list,
            &model_data_info,
        ))
    }

    pub fn load_model_data_list(&mut self) {
        log::info!("    load_model_data_list");
        let model_directory = PathBuf::from(MODEL_DIRECTORY);
        let model_files: Vec<PathBuf> = self.collect_resources(&model_directory, &[EXT_MODEL]);
        for model_file in model_files {
            let model_name = get_unique_resource_name(&self._model_data_map, &model_directory, &model_file);
            let meta_data = MetaData::new(&model_file, &model_file);
            let entry = if model_name == DEFAULT_MODEL_NAME {
                ResourceEntry::new_pinned(model_name.clone(), meta_data)
            } else {
                ResourceEntry::new(model_name.clone(), meta_data)
            };
            self._model_data_map.insert(model_name, entry);
        }

        // Preload & Pin default model
        let _ = self.get_model_data(DEFAULT_MODEL_NAME);
        self.pin_model_data(DEFAULT_MODEL_NAME);
    }

    pub fn unload_model_data_list(&mut self) {
        for entry in self._model_data_map.values() {
            entry.unload_with(|model_data| {
                model_data.borrow().destroy_model_data();
            });
        }
        self._model_data_map.clear();
    }

    pub fn has_model_data(&self, resource_name: &str) -> bool {
        self._model_data_map.contains_key(resource_name)
    }

    pub fn get_model_data(&self, resource_name: &str) -> &RcRefCell<ModelData<'a>> {
        get_resource_entry_data(
            "model_data",
            &self._model_data_map,
            resource_name,
            DEFAULT_MODEL_NAME,
            |model_name, meta_data| Self::load_model_data(model_name, meta_data),
        )
    }

    // Mesh Loader
    pub fn register_mesh_data(
        &mut self,
        renderer_context: &RendererContext<'a>,
        mesh_name: &String,
        mesh_data_create_info: MeshDataCreateInfo,
    ) {
        let mut geometry_data_list: Vec<RcRefCell<GeometryData>> = Vec::new();
        for (i, geometry_create_info) in mesh_data_create_info._geometry_create_infos.iter().enumerate() {
            let geomtery_name: String = format!("{}_{}", mesh_name, i);
            let geometry_data = renderer_context.create_geometry_buffer(&geomtery_name, geometry_create_info);
            geometry_data_list.push(newRcRefCell(geometry_data));
        }

        let mesh_data = newRcRefCell(MeshData::create_mesh_data(
            mesh_name,
            mesh_data_create_info,
            geometry_data_list,
        ));
        let entry = ResourceEntry::new_pinned(mesh_name.clone(), MetaData::default());
        entry.set_loaded_data(mesh_data);
        self._mesh_data_map.insert(mesh_name.clone(), entry);
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
        let resource_ext = if USE_JSON_FOR_MESH { EXT_JSON } else { EXT_MESH };
        let mesh_files = self.collect_resources(mesh_directory.as_path(), &[resource_ext]);
        let mut mesh_file_map: HashMap<String, PathBuf> = HashMap::new();
        for mesh_file in mesh_files.iter() {
            let mesh_name = get_resource_name_from_file_path(&mesh_directory, &mesh_file);
            mesh_file_map.insert(mesh_name, mesh_file.clone());
        }
        let mesh_source_files = self.collect_resources(mesh_source_directory.as_path(), &EXT_MESH_SOURCE);
        for mesh_source_file in mesh_source_files {
            let mesh_name = get_unique_resource_name(&self._mesh_data_map, &mesh_source_directory, &mesh_source_file);
            let mesh_file_path = match mesh_file_map.get(&mesh_name) {
                Some(p) => p.clone(),
                None => {
                    let mut path = if mesh_source_file.starts_with(ENGINE_RESOURCE_PATH) {
                        PathBuf::from(ENGINE_RESOURCE_PATH)
                    } else {
                        PathBuf::from(APPLICATION_RESOURCE_PATH)
                    };
                    path.push(&mesh_directory);
                    path.push(&mesh_name);
                    path.set_extension(resource_ext);
                    path
                }
            };
            let meta_data = MetaData::new(&mesh_file_path, &mesh_source_file);
            let entry = ResourceEntry::new(mesh_name.clone(), meta_data);
            self._mesh_data_map.insert(mesh_name, entry);
        }

        for (mesh_name, mesh_file_path) in mesh_file_map {
            if !self._mesh_data_map.contains_key(&mesh_name) {
                let meta_data = MetaData::new(&mesh_file_path, &mesh_file_path);
                let entry = ResourceEntry::new(mesh_name.clone(), meta_data);
                self._mesh_data_map.insert(mesh_name, entry);
            }
        }
    }

    pub fn load_mesh_data(mesh_name: &str, meta_data: &MetaData) -> Option<MeshData> {
        let renderer_context = get_renderer_context();
        let mesh_data_create_info = if !LOAD_FROM_EXTERNAL_FOR_MESH && meta_data._resource_file_path.is_file() {
            let loaded_contents = system::load(&meta_data._resource_file_path);
            if USE_JSON_FOR_MESH {
                serde_json::from_reader(loaded_contents).ok()?
            } else {
                bincode::deserialize_from(loaded_contents).ok()?
            }
        } else if meta_data._source_file_path.is_file() {
            let src_file_ext = meta_data._source_file_path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
            let create_info = match src_file_ext {
                EXT_OBJ => WaveFrontOBJ::get_mesh_data_create_infos(&meta_data._source_file_path),
                EXT_COLLADA => Collada::get_mesh_data_create_infos(&meta_data._source_file_path),
                EXT_GLTF_BINARY | EXT_GLTF => GLTF::get_mesh_data_create_infos(&meta_data._source_file_path),
                _ => {
                    if meta_data._resource_file_path.is_file() {
                        let loaded = system::load(&meta_data._resource_file_path);
                        if USE_JSON_FOR_MESH {
                            serde_json::from_reader(loaded).ok()?
                        } else {
                            bincode::deserialize_from(loaded).ok()?
                        }
                    } else {
                        log::error!("Unknown mesh source format: {:?}", meta_data);
                        return None;
                    }
                }
            };

            if !LOAD_FROM_EXTERNAL_FOR_MESH && !meta_data._resource_file_path.as_os_str().is_empty() {
                if let Some(parent) = meta_data._resource_file_path.parent() {
                    let _ = fs::create_dir_all(parent);
                }
                if let Ok(mut write_file) = File::create(&meta_data._resource_file_path) {
                    if USE_JSON_FOR_MESH {
                        if let Ok(contents) = serde_json::to_string(&create_info) {
                            let _ = write_file.write_all(contents.as_bytes());
                        }
                    } else {
                        if let Ok(contents) = bincode::serialize(&create_info) {
                            let _ = write_file.write_all(&contents);
                        }
                    }
                }
            }
            create_info
        } else if meta_data._resource_file_path.is_file() {
            let loaded_contents = system::load(&meta_data._resource_file_path);
            if USE_JSON_FOR_MESH {
                serde_json::from_reader(loaded_contents).ok()?
            } else {
                bincode::deserialize_from(loaded_contents).ok()?
            }
        } else {
            log::error!("Mesh file not found: {:?}", meta_data);
            return None;
        };

        let mut geometry_data_list: Vec<RcRefCell<GeometryData>> = Vec::new();
        for (i, geometry_create_info) in mesh_data_create_info._geometry_create_infos.iter().enumerate() {
            let geometry_name = format!("{}_{}", mesh_name, i);
            let geometry_data = renderer_context.create_geometry_buffer(&geometry_name, geometry_create_info);
            geometry_data_list.push(newRcRefCell(geometry_data));
        }

        Some(MeshData::create_mesh_data(
            &mesh_name.to_string(),
            mesh_data_create_info,
            geometry_data_list,
        ))
    }

    pub fn unload_mesh_data_list(&mut self, renderer_context: &RendererContext) {
        for entry in self._mesh_data_map.values() {
            entry.unload_with(|mesh_data| {
                for geometry_data in (*mesh_data).borrow().get_geometry_data_list() {
                    renderer_context.destroy_geometry_buffer(&geometry_data.borrow());
                }
            });
        }
        self._mesh_data_map.clear();
    }

    pub fn has_mesh_data(&self, resource_name: &str) -> bool {
        self._mesh_data_map.contains_key(resource_name)
    }

    pub fn get_mesh_data(&self, resource_name: &str) -> &RcRefCell<MeshData> {
        get_resource_entry_data(
            "mesh_data",
            &self._mesh_data_map,
            resource_name,
            DEFAULT_MESH_NAME,
            |mesh_name, meta_data| Self::load_mesh_data(mesh_name, meta_data),
        )
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
            image::DynamicImage::ImageRgb8(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageLuma8(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R8G8B8A8_UNORM),
            image::DynamicImage::ImageRgb16(_) => (dynamic_image.to_rgba8().into_raw(), vk::Format::R16G16B16A16_UNORM),
            _ => {
                log::error!("Unkown format: {:?}", texture_file);
                (Vec::new(), vk::Format::UNDEFINED)
            }
        };
        (image_width, image_height, image_layer, image_data_raw, image_format)
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

    pub fn register_texture_data(&mut self, texture_data_name: String, texture_data: RcRefCell<TextureData>) {
        let entry = ResourceEntry::new_pinned(texture_data_name.clone(), MetaData::default());
        entry.set_loaded_data(texture_data);
        self._texture_data_map.insert(texture_data_name, entry);
    }

    fn load_binary_texture(
        renderer_context: &RendererContext,
        texture_name: &str,
        file_path: &Path,
    ) -> Option<TextureData> {
        let mut loaded_contents = system::load(file_path);
        let image_view_type = vk::ImageViewType::from_raw(loaded_contents.read_i32::<LittleEndian>().ok()?);
        let image_width = loaded_contents.read_i32::<LittleEndian>().ok()?;
        let image_height = loaded_contents.read_i32::<LittleEndian>().ok()?;
        let image_depth = loaded_contents.read_i32::<LittleEndian>().ok()?;
        let image_format = vk::Format::from_raw(loaded_contents.read_i32::<LittleEndian>().ok()?);
        let enable_mipmap = 0 != loaded_contents.read_i32::<LittleEndian>().ok()?;
        let min_filter = vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().ok()?);
        let mag_filter = vk::Filter::from_raw(loaded_contents.read_i32::<LittleEndian>().ok()?);
        let wrap = vk::SamplerAddressMode::from_raw(loaded_contents.read_i32::<LittleEndian>().ok()?);
        let data_bytes = loaded_contents.read_i32::<LittleEndian>().ok()?;
        let mut image_data = Vec::new();
        loaded_contents.read_to_end(&mut image_data).ok()?;
        if data_bytes as usize != image_data.len() {
            log::error!("Binary texture data length mismatch in {:?}", file_path);
            return None;
        }

        let texture_create_info = TextureCreateInfo {
            _texture_name: texture_name.to_string(),
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
        Some(renderer_context.create_texture(&texture_create_info))
    }

    fn load_combined_texture(
        renderer_context: &RendererContext,
        texture_name: &str,
        file_path: &Path,
    ) -> Option<TextureData> {
        let loaded_contents = system::load(file_path);
        let contents: Value = serde_json::from_reader(loaded_contents).ok()?;
        let ext = file_path.extension().and_then(|e| e.to_str()).unwrap_or("");
        let mut texture_file_names: Vec<String> = Vec::new();
        let image_view_type = if EXT_TEXTURE_CUBE == ext {
            if let Value::Object(texture_cube_faces) = contents {
                for face in constants::CUBE_TEXTURE_FACES.iter() {
                    if let Some(Value::String(face_texture_name)) = texture_cube_faces.get(*face) {
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
                return None;
            }
        } else {
            return None;
        };

        let directory = file_path.parent()?;
        let mut texture_files: Vec<PathBuf> = Vec::new();
        for texture_file_name in texture_file_names {
            let mut texture_file = PathBuf::from(directory);
            texture_file.push(PathBuf::from(texture_file_name).file_name()?);
            texture_files.push(texture_file);
        }

        let (image_width, image_height, image_layers, image_data, image_format) =
            EngineResources::load_image_data_list(&texture_files);
        if vk::Format::UNDEFINED == image_format {
            return None;
        }

        let texture_create_info = TextureCreateInfo {
            _texture_name: texture_name.to_string(),
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
        Some(renderer_context.create_texture(&texture_create_info))
    }

    fn load_simple_image_texture(
        renderer_context: &RendererContext,
        texture_name: &str,
        file_path: &Path,
    ) -> Option<TextureData> {
        let (image_width, image_height, image_layers, image_data, image_format) =
            EngineResources::load_image_data(&PathBuf::from(file_path));
        if vk::Format::UNDEFINED == image_format {
            return None;
        }

        let texture_create_info = TextureCreateInfo {
            _texture_name: texture_name.to_string(),
            _texture_width: image_width,
            _texture_height: image_height,
            _texture_layers: image_layers,
            _texture_format: image_format,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_initial_data_list: image_data,
            _enable_mipmap: true,
            _enable_anisotropy: false,
            ..Default::default()
        };
        Some(renderer_context.create_texture(&texture_create_info))
    }

    pub fn load_texture_data(texture_name: &str, meta_data: &MetaData) -> Option<TextureData> {
        let renderer_context = get_renderer_context();

        let file_path = if meta_data._source_file_path.is_file() {
            &meta_data._source_file_path
        } else if meta_data._resource_file_path.is_file() {
            &meta_data._resource_file_path
        } else {
            log::error!("Texture file not found: {:?}", meta_data);
            return None;
        };

        let ext = file_path.extension().and_then(|e| e.to_str()).unwrap_or("");
        if ext == EXT_TEXTURE_CUBE || ext == EXT_TEXTURE_2D_ARRAY || ext == EXT_TEXTURE_3D {
            Self::load_combined_texture(renderer_context, texture_name, file_path)
        } else if EXT_TEXTURE.contains(&ext) {
            Self::load_binary_texture(renderer_context, texture_name, file_path)
        } else {
            Self::load_simple_image_texture(renderer_context, texture_name, file_path)
        }
    }

    pub fn load_texture_data_list(&mut self, renderer_context: &RendererContext) {
        log::info!("    load_texture_data_list");
        let default_texture_data_list: Vec<TextureData> = texture_generator::generate_textures(renderer_context);
        for texture_data in default_texture_data_list {
            self.register_texture_data(texture_data._texture_data_name.clone(), newRcRefCell(texture_data));
        }

        let texture_directory = PathBuf::from(TEXTURE_DIRECTORY);
        let texture_source_directory = PathBuf::from(TEXTURE_SOURCE_DIRECTORY);
        let mut combined_textures_name_map: HashMap<PathBuf, String> = HashMap::new();

        // generate necessary texture data_list
        if unsafe { DEVELOPMENT } {
            let mut engine_texture_source_path = PathBuf::from(ENGINE_RESOURCE_SOURCE_PATH);
            engine_texture_source_path.push(TEXTURE_SOURCE_DIRECTORY);
            texture_generator::generate_images(&engine_texture_source_path);
        }

        // combined texture list (.cube, .2darray, .3d)
        let combined_texture_files = self.collect_resources(
            texture_source_directory.as_path(),
            &[EXT_TEXTURE_2D_ARRAY, EXT_TEXTURE_3D, EXT_TEXTURE_CUBE],
        );
        for texture_src_file in combined_texture_files.iter() {
            let directory = match texture_src_file.parent() {
                Some(parent) => parent,
                None => continue,
            };
            let texture_data_name = get_resource_name_from_file_path(&texture_source_directory, texture_src_file);
            let loaded_contents = system::load(texture_src_file);
            if let Ok(contents) = serde_json::from_reader::<_, Value>(loaded_contents) {
                let ext = texture_src_file.extension().and_then(|e| e.to_str()).unwrap_or("");
                let mut texture_file_names: Vec<String> = Vec::new();
                if EXT_TEXTURE_CUBE == ext {
                    if let Value::Object(texture_cube_faces) = &contents {
                        for face in constants::CUBE_TEXTURE_FACES.iter() {
                            if let Some(Value::String(face_texture_name)) = texture_cube_faces.get(*face) {
                                texture_file_names.push(face_texture_name.clone());
                            }
                        }
                    }
                } else if EXT_TEXTURE_2D_ARRAY == ext || EXT_TEXTURE_3D == ext {
                    if let Value::Array(texture_file_list) = &contents {
                        for texture_file in texture_file_list {
                            if let Value::String(texture_file) = texture_file {
                                texture_file_names.push(texture_file.clone());
                            }
                        }
                    }
                }

                for texture_file_name in texture_file_names {
                    let mut texture_file: PathBuf = PathBuf::from(directory);
                    if let Some(file_name) = PathBuf::from(texture_file_name).file_name() {
                        texture_file.push(file_name);
                        combined_textures_name_map.insert(texture_file, texture_data_name.clone());
                    }
                }
            }

            let meta_data = MetaData::new(texture_src_file, texture_src_file);
            let entry = ResourceEntry::new_pinned(texture_data_name.clone(), meta_data);
            self._texture_data_map.insert(texture_data_name, entry);
        }

        // read binary texture data (.texture)
        let texture_files = self.collect_resources(texture_directory.as_path(), &EXT_TEXTURE);
        for texture_file in texture_files.iter() {
            let texture_data_name = get_resource_name_from_file_path(&texture_directory, texture_file);
            let meta_data = MetaData::new(texture_file, texture_file);
            let entry = ResourceEntry::new_pinned(texture_data_name.clone(), meta_data);
            self._texture_data_map.insert(texture_data_name, entry);
        }

        // load texture from external files (.png, .jpg, etc.)
        let texture_src_files = self.collect_resources(texture_source_directory.as_path(), &EXT_IMAGE_SOURCE);
        for texture_src_file in texture_src_files.iter() {
            if combined_textures_name_map.contains_key(texture_src_file) {
                continue;
            }
            let texture_data_name = get_resource_name_from_file_path(&texture_source_directory, texture_src_file);
            if self._texture_data_map.contains_key(&texture_data_name) {
                continue;
            }
            let meta_data = MetaData::new(texture_src_file, texture_src_file);
            let entry = ResourceEntry::new(texture_data_name.clone(), meta_data);
            self._texture_data_map.insert(texture_data_name, entry);
        }

        // Preload & Pin default texture
        let _ = self.get_texture_data(DEFAULT_TEXTURE_NAME);
        self.pin_texture_data(DEFAULT_TEXTURE_NAME);
    }

    pub fn unload_texture_data_list(&mut self, renderer_context: &RendererContext) {
        for entry in self._texture_data_map.values() {
            entry.unload_with(|texture_data| {
                renderer_context.destroy_texture(&(*texture_data).borrow());
            });
        }
        self._texture_data_map.clear();
    }

    pub fn has_texture_data(&self, resource_name: &str) -> bool {
        self._texture_data_map.contains_key(resource_name)
    }

    pub fn get_loaded_texture_data(&self, resource_name: &str) -> Option<RcRefCell<TextureData>> {
        self._texture_data_map.get(resource_name).and_then(|entry| entry.get_loaded_data())
    }

    pub fn get_texture_data(&self, resource_name: &str) -> &RcRefCell<TextureData> {
        let was_loaded = self._texture_data_map.get(resource_name).map(|entry| entry.is_loaded()).unwrap_or(true);
        let result = get_resource_entry_data(
            "texture_data",
            &self._texture_data_map,
            resource_name,
            DEFAULT_TEXTURE_NAME,
            |texture_name, meta_data| Self::load_texture_data(texture_name, meta_data),
        );
        if !was_loaded && crate::core::engine_service_locator::is_engine_core_valid() {
            self.notify_texture_loaded(get_renderer_context().get_device(), resource_name, &result.borrow());
        }
        result
    }

    pub fn notify_texture_loaded(&self, device: &ash::Device, texture_name: &str, texture_data: &TextureData) {
        for (name, mat_inst_cell) in self._material_instance_data_map.iter() {
            if let Ok(mut mat_inst) = mat_inst_cell.try_borrow_mut() {
                mat_inst.update_texture_descriptor_sets(device, texture_name, texture_data);
            } else {
                log::warn!(
                    "notify_texture_loaded: Failed to borrow_mut MaterialInstance '{}' for texture '{}'",
                    name,
                    texture_name
                );
            }
        }
    }

    fn get_fallback_texture_name_for(&self, texture_name: &str) -> &'static str {
        if let Some(entry) = self._texture_data_map.get(texture_name) {
            let ext = entry._meta_data._source_file_path.extension().and_then(|e| e.to_str()).unwrap_or("");
            if ext == EXT_TEXTURE_2D_ARRAY {
                return texture_generator::DEFAULT_2D_ARRAY_TEXTURE;
            } else if ext == EXT_TEXTURE_3D {
                return texture_generator::DEFAULT_3D_TEXTURE;
            } else if ext == EXT_TEXTURE_CUBE {
                return texture_generator::DEFAULT_CUBE_TEXTURE;
            }
        }
        DEFAULT_TEXTURE_NAME
    }

    fn get_loaded_or_fallback_texture_data(
        &self,
        texture_name: &str,
        force_sync_load: bool,
    ) -> &RcRefCell<TextureData> {
        if force_sync_load {
            return self.get_texture_data(texture_name);
        }
        if let Some(entry) = self._texture_data_map.get(texture_name) {
            if entry.is_loaded() {
                return self.get_texture_data(texture_name);
            }
        }
        let fallback_name = self.get_fallback_texture_name_for(texture_name);
        self.get_texture_data(fallback_name)
    }

    // Framebuffer
    pub fn load_framebuffer_data_list(&mut self, renderer_context: &RendererContext) {
        log::info!("    load_framebuffer_data_list");
        for render_pass_data in self._render_pass_data_map.values() {
            let render_pass_data = render_pass_data.borrow();
            for (_key, render_pass_data_create_info) in self._render_pass_data_create_info_map.iter() {
                if render_pass_data_create_info._render_pass_create_info_name == render_pass_data._render_pass_data_name
                {
                    if render_pass_data_create_info._render_pass_framebuffer_create_info.is_valid() {
                        let framebuffer_data = framebuffer::create_framebuffer_data(
                            renderer_context.get_device(),
                            renderer_context.get_debug_utils(),
                            render_pass_data._render_pass,
                            render_pass_data._render_pass_data_name.as_str(),
                            render_pass_data_create_info._render_pass_framebuffer_create_info.clone(),
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
            framebuffer::destroy_framebuffer_data(renderer_context.get_device(), &framebuffer_data.borrow());
        }
        self._framebuffer_data_list_map.clear();
    }

    pub fn has_framebuffer_data(&self, resource_name: &str) -> bool {
        self._framebuffer_data_list_map.contains_key(resource_name)
    }

    pub fn get_framebuffer_data(&self, resource_name: &str) -> &RcRefCell<FramebufferData<'a>> {
        get_resource_data_must("framebuffer_data", &self._framebuffer_data_list_map, resource_name)
    }

    // render pass data create info
    pub fn load_render_pass_data_create_infos(&mut self, renderer_context: &RendererContext) {
        let render_pass_data_create_infos = renderer_context.get_render_pass_data_create_infos();
        for render_pass_data_create_info in render_pass_data_create_infos.iter() {
            self._render_pass_data_create_info_map.insert(
                render_pass_data_create_info._render_pass_create_info_name.clone(),
                render_pass_data_create_info.clone(),
            );
        }

        {
            unsafe {
                (*self._callback_load_render_pass_create_infos)(
                    renderer_context,
                    &mut self._render_pass_data_create_info_map,
                );
            }
        }
    }

    pub fn unload_render_pass_data_create_infos(&mut self, _renderer_context: &RendererContext) {
        self._render_pass_data_create_info_map.clear();
    }

    pub fn get_render_pass_data_create_info(&self, resource_name: &str) -> &RenderPassDataCreateInfo {
        if let Some(resource) = self._render_pass_data_create_info_map.get(resource_name) {
            return resource;
        };
        panic!("not found resource: {:?}", resource_name);
    }

    // render pass data
    pub fn load_render_pass_data_list(&mut self, renderer_context: &RendererContext<'a>) {
        log::info!("    load_render_pass_data_list");

        let render_pass_data_create_info_map = ptr_as_ref(&self._render_pass_data_create_info_map);
        for (_key, render_pass_data_create_info) in render_pass_data_create_info_map.iter() {
            let mut descriptor_data_list: Vec<RcRefCell<DescriptorData>> = Vec::new();
            for pipeline_data_create_info in render_pass_data_create_info._pipeline_data_create_infos.iter() {
                descriptor_data_list.push(self.get_descriptor_data(
                    renderer_context,
                    &render_pass_data_create_info._render_pass_create_info_name,
                    pipeline_data_create_info,
                ));
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
            render_pass::destroy_render_pass_data(renderer_context.get_device(), &(*render_pass_data).borrow());
        }
        self._render_pass_data_map.clear()
    }

    pub fn has_render_pass_data(&self, resource_name: &str) -> bool {
        self._render_pass_data_map.contains_key(resource_name)
    }

    pub fn get_render_pass_data(&self, resource_name: &str) -> &RcRefCell<RenderPassData<'a>> {
        get_resource_data_must("render_pass_data", &self._render_pass_data_map, resource_name)
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
            let material_name = get_unique_resource_name(&self._material_data_map, &material_directory, &material_file);
            let loaded_contents = system::load(material_file);
            let contents: Value = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            let material_create_info = match contents {
                Value::Object(material_create_info) => material_create_info,
                _ => panic!("material parsing error"),
            };
            let pipeline_create_infos = material_create_info.get("pipelines").unwrap().as_array().unwrap();
            let render_pass_pipeline_data_list: Vec<Option<RenderPassPipelineData<'a>>> =
                pipeline_create_infos
                    .iter()
                    .map(|pipeline_create_info| {
                        let render_pass_data_name = match pipeline_create_info.get("render_pass").unwrap() {
                            Value::String(render_pass_data_name) => render_pass_data_name,
                            _ => panic!("failed to parsing render_pass"),
                        };
                        let pipeline_data_name = match pipeline_create_info.get("pipeline").unwrap() {
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

            let material_data =
                MaterialData::create_material(&material_name, &render_pass_pipeline_data_list, material_parameters);
            self._material_data_map.insert(material_name.clone(), newRcRefCell(material_data));
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
        get_resource_data_must("material_data", &self._material_data_map, resource_name)
    }

    // MaterialInstance_data_list
    pub fn load_material_instance_data_list(&mut self, renderer_context: &'a RendererContext<'a>, is_reload: bool) {
        log::info!("    load_material_instance_data_list");
        let material_instance_entries: Vec<(String, String, serde_json::Map<String, Value>, bool)> = if is_reload
            && !self._material_instance_data_map.is_empty()
        {
            self._material_instance_data_map
                .iter()
                .map(|(name, mat_inst_cell)| {
                    let mat_inst = mat_inst_cell.borrow();
                    let mat_data_name = mat_inst._material_data.borrow()._material_data_name.clone();
                    let is_engine_resource = self
                        ._relative_resource_file_path_map
                        .get(&PathBuf::from(format!("material_instances/{}.matinst", name)))
                        .map(|path| path.starts_with(ENGINE_RESOURCE_PATH))
                        .unwrap_or(false);
                    (
                        name.clone(),
                        mat_data_name,
                        mat_inst._material_parameters.clone(),
                        is_engine_resource,
                    )
                })
                .collect()
        } else {
            let material_instance_directory = PathBuf::from(MATERIAL_INSTANCE_DIRECTORY);
            let material_instance_files =
                self.collect_resources(&material_instance_directory, &[EXT_MATERIAL_INSTANCE]);
            let mut entries = Vec::with_capacity(material_instance_files.len());
            let mut seen_names = std::collections::HashSet::new();
            for material_instance_file in material_instance_files.iter() {
                let base_name = get_resource_name_from_file_path(&material_instance_directory, &material_instance_file);
                let mut material_instance_name = base_name.clone();
                if !is_reload {
                    let mut index = 0;
                    while self._material_instance_data_map.contains_key(&material_instance_name)
                        || seen_names.contains(&material_instance_name)
                    {
                        material_instance_name = format!("{}_{}", base_name, index);
                        index += 1;
                    }
                    seen_names.insert(material_instance_name.clone());
                }

                let is_engine_resource = material_instance_file.starts_with(ENGINE_RESOURCE_PATH);

                let loaded_contents = system::load(material_instance_file);
                let contents: Value = serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
                let material_instance_create_info = match contents {
                    Value::Object(material_instance_create_info) => material_instance_create_info,
                    _ => panic!("material instance parsing error"),
                };
                let material_data_name = match material_instance_create_info.get("material_name").unwrap() {
                    Value::String(material_data_name) => material_data_name.clone(),
                    _ => panic!("material name parsing error"),
                };

                let material_parameters: serde_json::Map<String, Value> =
                    match material_instance_create_info.get("material_parameters") {
                        Some(Value::Object(material_parameters)) => material_parameters.clone(),
                        _ => serde_json::Map::new(),
                    };
                entries.push((
                    material_instance_name,
                    material_data_name,
                    material_parameters,
                    is_engine_resource,
                ));
            }
            entries
        };

        for (material_instance_name, material_data_name, mut material_parameters, is_engine_resource) in
            material_instance_entries
        {
            let material_data = self.get_material_data(material_data_name.as_str()).clone();
            let default_material_parameters = &material_data.borrow()._material_parameters;
            for (key, value) in default_material_parameters.iter() {
                if false == material_parameters.contains_key(key) {
                    material_parameters.insert(key.to_string(), value.clone());
                }
            }

            // replace texture_font parameter as a matched to default font.
            if material_instance_name == DEFAULT_RENDER_FONT_MATERIAL_INSTANCE_NAME
                || material_instance_name == DEFAULT_RENDER_UI_MATERIAL_INSTANCE_NAME
            {
                material_parameters.insert(
                    String::from(TEXTURE_FONT_MATERIAL_PARAMETER_NAME),
                    Value::String(String::from("fonts/") + &String::from(DEFAULT_FONT_NAME)),
                );
            }

            let force_sync_load = is_engine_resource;

            // Pin all textures referenced by this material instance so they are never evicted by GC
            for (_key, value) in material_parameters.iter() {
                if let Value::String(texture_name) = value {
                    self.pin_texture_data(texture_name);
                }
            }

            let pipeline_bind_create_infos = material_data
                .borrow()
                ._render_pass_pipeline_data_map
                .iter()
                .map(|(_key, render_pass_pipeline_data)| {
                    let descriptor_data_create_infos = &render_pass_pipeline_data
                        ._pipeline_data
                        .borrow()
                        ._descriptor_data
                        ._descriptor_data_create_infos;
                    let descriptor_resource_infos_list = constants::SWAPCHAIN_IMAGE_INDICES
                        .iter()
                        .map(|swapchain_index| {
                            let descriptor_resource_infos = descriptor_data_create_infos
                                .iter()
                                .map(|descriptor_data_create_info| {
                                    let material_parameter_name = &descriptor_data_create_info._descriptor_name;
                                    let material_parameter_resource_type =
                                        &descriptor_data_create_info._descriptor_resource_type;
                                    let maybe_material_parameter = material_parameters.get(material_parameter_name);
                                    let descriptor_resource_info = match material_parameter_resource_type {
                                        DescriptorResourceType::UniformBuffer
                                        | DescriptorResourceType::StorageBuffer => {
                                            let uniform_buffer_data = renderer_context
                                                .get_shader_buffer_data_from_str(material_parameter_name.as_str());
                                            uniform_buffer_data._descriptor_buffer_infos[*swapchain_index].clone()
                                        }
                                        DescriptorResourceType::Sampler => {
                                            let sampler =
                                                renderer_context.get_sampler_from_str(material_parameter_name.as_str());
                                            DescriptorResourceInfo::DescriptorSamplerInfo(*sampler)
                                        }
                                        DescriptorResourceType::Image
                                        | DescriptorResourceType::Texture
                                        | DescriptorResourceType::StorageTexture => {
                                            let texture_data = match maybe_material_parameter {
                                                Some(Value::String(value)) => {
                                                    self.get_loaded_or_fallback_texture_data(value, force_sync_load)
                                                }
                                                _ => self.get_texture_data(DEFAULT_TEXTURE_NAME),
                                            };
                                            if descriptor_data_create_info.use_sub_image() {
                                                DescriptorResourceInfo::DescriptorImageInfo(
                                                    texture_data.borrow().get_sub_image_info(
                                                        descriptor_data_create_info._descriptor_image_layer,
                                                        descriptor_data_create_info._descriptor_image_mip_level,
                                                    ),
                                                )
                                            } else {
                                                DescriptorResourceInfo::DescriptorImageInfo(
                                                    texture_data.borrow().get_default_image_info(),
                                                )
                                            }
                                        }
                                        DescriptorResourceType::RenderTarget
                                        | DescriptorResourceType::StorageRenderTarget => {
                                            let texture_data = renderer_context
                                                .get_render_target_from_str(material_parameter_name.as_str());
                                            if descriptor_data_create_info.use_sub_image() {
                                                DescriptorResourceInfo::DescriptorImageInfo(
                                                    texture_data.get_sub_image_info(
                                                        descriptor_data_create_info._descriptor_image_layer,
                                                        descriptor_data_create_info._descriptor_image_mip_level,
                                                    ),
                                                )
                                            } else {
                                                DescriptorResourceInfo::DescriptorImageInfo(
                                                    texture_data.get_default_image_info(),
                                                )
                                            }
                                        }
                                        DescriptorResourceType::AccelerationStructure => {
                                            log::info!(
                                                ">>> TEST CODE: load_material_instance_data_list: {:?}",
                                                material_instance_name
                                            );
                                            log::info!("    WriteDescriptorSetAccelerationStructure");
                                            let ray_tracing_data = renderer_context.get_ray_tracing_test_data();
                                            ray_tracing_data.get_top_level_descriptor_resource_info()
                                        }
                                    };
                                    return descriptor_resource_info;
                                })
                                .collect();
                            return descriptor_resource_infos;
                        })
                        .collect();

                    // update push constant
                    let mut push_constant_data_list =
                        render_pass_pipeline_data._pipeline_data.borrow()._push_constant_data_list.clone();
                    for push_constant_data in push_constant_data_list.iter_mut() {
                        push_constant_data._push_constant.update_material_parameters(&material_parameters);
                    }

                    // create PipelineBindingDataCreateInfo
                    PipelineBindingDataCreateInfo {
                        _render_pass_pipeline_data: render_pass_pipeline_data.clone(),
                        _descriptor_resource_infos_list: descriptor_resource_infos_list,
                        _push_constant_data_list: push_constant_data_list,
                    }
                })
                .collect();

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
                let exists_material_instance_data: &mut MaterialInstanceData =
                    &mut self.get_material_instance_data(&material_instance_name).borrow_mut();
                *exists_material_instance_data = material_instance_data;
            } else {
                self._material_instance_data_map
                    .insert(material_instance_name.clone(), newRcRefCell(material_instance_data));
            }
        }
    }

    pub fn unload_material_instance_data_list(&mut self, is_reload: bool) {
        for material_instance_data in self._material_instance_data_map.values() {
            (*material_instance_data).borrow().destroy_material_instance();
        }

        if false == is_reload {
            self._material_instance_data_map.clear();
        }
    }

    pub fn has_material_instance_data(&self, resource_name: &str) -> bool {
        self._material_instance_data_map.contains_key(resource_name)
    }

    pub fn get_material_instance_data(&self, resource_name: &str) -> &RcRefCell<MaterialInstanceData<'a>> {
        get_resource_data_must(
            "material_instance_data",
            &self._material_instance_data_map,
            resource_name,
        )
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
        let max_descriptor_pool_count: u32 =
            unsafe { (constants::MAX_DESCRIPTOR_POOL_ALLOC_COUNT * constants::SWAPCHAIN_IMAGE_COUNT) as u32 };
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
            descriptor::destroy_descriptor_data(renderer_context.get_device(), &(*descriptor_data).borrow());
        }
        self._descriptor_data_map.clear();
    }

    // SceneData
    pub fn load_scene_data_list(&mut self, _renderer_context: &RendererContext) {
        log::info!("    load_scene_data_list");
        let scene_directory = PathBuf::from(SCENE_DIRECTORY);
        let scene_data_files: Vec<PathBuf> = self.collect_resources(&scene_directory, &[EXT_SCENE]);
        for scene_data_file in scene_data_files {
            let scene_data_name =
                get_unique_resource_name(&self._scene_data_create_infos_map, &scene_directory, &scene_data_file);
            let loaded_contents = system::load(&scene_data_file);
            let scene_data_create_info: SceneDataCreateInfo =
                serde_json::from_reader(loaded_contents).expect("Failed to deserialize.");
            self._scene_data_create_infos_map.insert(scene_data_name.clone(), newRcRefCell(scene_data_create_info));
        }
    }

    pub fn unload_scene_data_list(&mut self, _renderer_context: &RendererContext) {
        self._scene_data_create_infos_map.clear();
    }

    pub fn save_scene_data(&mut self, scene_data_name: &str, scene_data_create_info: &SceneDataCreateInfo) {
        let mut scene_data_filepath = PathBuf::from(APPLICATION_RESOURCE_PATH);
        scene_data_filepath.push(SCENE_DIRECTORY);
        scene_data_filepath.push(scene_data_name);
        scene_data_filepath.set_extension(EXT_SCENE);
        let mut write_file = File::create(&scene_data_filepath).expect("Failed to create file");
        let mut write_contents: String = serde_json::to_string(&scene_data_create_info).expect("Failed to serialize.");
        write_contents = write_contents.replace(",\"", ",\n\"");
        write_file.write_all(write_contents.as_bytes()).expect("Failed to write");

        self._scene_data_create_infos_map.insert(
            String::from(scene_data_name),
            newRcRefCell(scene_data_create_info.clone()),
        );
    }

    pub fn has_scene_data(&self, resource_name: &str) -> bool {
        self._scene_data_create_infos_map.get(resource_name).is_some()
    }

    pub fn get_scene_data(&self, resource_name: &str) -> &RcRefCell<SceneDataCreateInfo> {
        get_resource_data_must("scene_data", &self._scene_data_create_infos_map, resource_name)
    }
}
