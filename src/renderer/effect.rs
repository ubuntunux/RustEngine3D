use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4, Matrix4 };

use crate::renderer::renderer::RendererData;
use crate::renderer::material_instance::MaterialInstanceData;
use crate::renderer::mesh::MeshData;
use crate::resource::resource::Resources;
use crate::utilities::system::RcRefCell;
use crate::utilities::math;

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleSpawnVolumeType {
    Box = 0,
    Sphere = 1,
    Cone = 2,
    Cylinder = 3,
}

impl Default for ParticleSpawnVolumeType {
    fn default() -> Self {
        ParticleSpawnVolumeType::Box
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleGeometryType {
    Quad = 0,
    Decal = 1,
    Mesh = 2,
    Ribbon = 3,
    Beam = 4,
    Capsule = 5,
}

impl Default for ParticleGeometryType {
    fn default() -> Self {
        ParticleGeometryType::Quad
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleBlendMode {
    AlphaBlend = 0,
    Additive = 1,
    Opaque = 2,
}

impl Default for ParticleBlendMode {
    fn default() -> Self {
        ParticleBlendMode::AlphaBlend
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleAlignMode {
    None = 0,
    Billboard = 1,
    VelocityAlign = 2,
}

impl Default for ParticleAlignMode {
    fn default() -> Self {
        ParticleAlignMode::Billboard
    }
}

#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleVelocityType {
    Local = 0,
    WorldY_LocalXZ = 1,
    NormalDirection = 2,
}

impl Default for ParticleVelocityType {
    fn default() -> Self {
        ParticleVelocityType::Local
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct EffectDataCreateInfo {
    pub _effect_position: Vector3<f32>,
    pub _effect_rotation: Vector3<f32>,
    pub _effect_scale: Vector3<f32>,
    pub _emitter_data_create_infos: Vec<EmitterDataCreateInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct EmitterDataCreateInfo {
    pub _enable: bool,
    pub _emitter_data_name: String,
    pub _emitter_position: Vector3<f32>,
    pub _emitter_rotation: Vector3<f32>,
    pub _emitter_scale: Vector3<f32>,
    pub _emitter_lifetime: f32,
    pub _spawn_volume_type: ParticleSpawnVolumeType,
    pub _spawn_volume_info: Vector4<f32>,
    pub _spawn_volume_position: Vector3<f32>,
    pub _spawn_volume_rotation: Vector3<f32>,
    pub _spawn_volume_scale: Vector3<f32>,
    pub _spawn_count: i32,
    pub _spawn_term: f32,
    pub _delay: f32,
    pub _life_time_min: f32,
    pub _life_time_max: f32,
    pub _align_mode: ParticleAlignMode,
    pub _blend_mode: ParticleBlendMode,
    pub _geometry_type: ParticleGeometryType,
    pub _material_instance_name: String,
    pub _mesh_name: String,
    pub _rotation_min: Vector3<f32>,
    pub _rotation_max: Vector3<f32>,
    pub _scale_min: Vector3<f32>,
    pub _scale_max: Vector3<f32>,
}

pub struct EffectData {
    pub _effect_data_name: String,
    pub _emitter_datas: Vec<EmitterData>,
    pub _effect_transform: Matrix4<f32>,
}

pub struct EmitterData {
    pub _enable: bool,
    pub _emitter_data_name: String,
    pub _emitter_transform: Matrix4<f32>,
    pub _emitter_lifetime: f32,
    pub _spawn_volume_type: ParticleSpawnVolumeType,
    pub _spawn_volume_info: Vector4<f32>,
    pub _spawn_volume_transform: Matrix4<f32>,
    pub _spawn_count: i32,
    pub _spawn_term: f32,
    pub _delay: f32,
    pub _life_time_min: f32,
    pub _life_time_max: f32,
    pub _align_mode: ParticleAlignMode,
    pub _blend_mode: ParticleBlendMode,
    pub _geometry_type: ParticleGeometryType,
    pub _material_instance_data: RcRefCell<MaterialInstanceData>,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _rotation_min: Vector3<f32>,
    pub _rotation_max: Vector3<f32>,
    pub _scale_min: Vector3<f32>,
    pub _scale_max: Vector3<f32>,
}

pub struct EffectInstance {
    pub _effect_data: RcRefCell<EffectData>,
    pub _elapsed_time: f32,
    pub _lifetime: f32,
    pub _emitters: Vec<EmitterInstance>,
}

pub struct EmitterInstance {
    pub _emitter_data: *const EmitterData,
    pub _elapsed_time: f32,
    pub _lifetime: f32,
}

pub struct EffectManagerData {
    _renderer_data: RcRefCell<RendererData>,
    _resources: RcRefCell<Resources>,
    _effects: Vec<EffectInstance>,
}

// interface
impl EffectData {
    pub fn create_effect_data(
        effect_data_name: &String,
        effect_data_create_info: &EffectDataCreateInfo,
        emitter_datas: Vec<EmitterData>
    ) -> EffectData {
        EffectData {
            _effect_data_name: effect_data_name.clone(),
            _effect_transform: math::make_srt_transform(
                &effect_data_create_info._effect_position,
                &effect_data_create_info._effect_rotation,
                &effect_data_create_info._effect_scale,
            ),
            _emitter_datas: emitter_datas,
        }
    }

    pub fn destroy_effect_data(&mut self) {
    }
}

impl EmitterData {
    pub fn create_emitter_data(
        emitter_data_create_info: &EmitterDataCreateInfo,
        material_instance: RcRefCell<MaterialInstanceData>,
        mesh_data: RcRefCell<MeshData>,
    ) -> EmitterData {
        EmitterData {
            _enable: emitter_data_create_info._enable,
            _emitter_data_name: emitter_data_create_info._emitter_data_name.clone(),
            _emitter_transform: math::make_srt_transform(
                &emitter_data_create_info._emitter_position,
                &emitter_data_create_info._emitter_rotation,
                &emitter_data_create_info._emitter_scale,
            ),
            _emitter_lifetime: emitter_data_create_info._emitter_lifetime,
            _spawn_volume_type: emitter_data_create_info._spawn_volume_type,
            _spawn_volume_info: emitter_data_create_info._spawn_volume_info.clone() as Vector4<f32>,
            _spawn_volume_transform: math::make_srt_transform(
                &emitter_data_create_info._spawn_volume_position,
                &emitter_data_create_info._spawn_volume_rotation,
                &emitter_data_create_info._spawn_volume_scale,
            ),
            _spawn_count: emitter_data_create_info._spawn_count,
            _spawn_term: emitter_data_create_info._spawn_term,
            _delay: emitter_data_create_info._delay,
            _life_time_min: emitter_data_create_info._life_time_min,
            _life_time_max: emitter_data_create_info._life_time_max,
            _align_mode: emitter_data_create_info._align_mode,
            _blend_mode: emitter_data_create_info._blend_mode,
            _geometry_type: emitter_data_create_info._geometry_type,
            _material_instance_data: material_instance,
            _mesh_data: mesh_data,
            _rotation_min: emitter_data_create_info._rotation_min.clone() as Vector3<f32>,
            _rotation_max: emitter_data_create_info._rotation_max.clone() as Vector3<f32>,
            _scale_min: emitter_data_create_info._scale_min.clone() as Vector3<f32>,
            _scale_max: emitter_data_create_info._scale_max.clone() as Vector3<f32>,
        }
    }
}

impl EffectInstance {
    pub fn create_effect_instance(effect_data: &RcRefCell<EffectData>) -> EffectInstance {
        let emitters = effect_data.borrow()._emitter_datas.iter().map(|emitter_data| {
            EmitterInstance::create_emitter_instance(emitter_data)
        }).collect();

        EffectInstance {
            _effect_data: effect_data.clone(),
            _elapsed_time: 0.0,
            _lifetime: 0.0,
            _emitters: emitters,
        }
    }
}

impl EmitterInstance {
    pub fn create_emitter_instance(emitter_data: &EmitterData) -> EmitterInstance {
        EmitterInstance {
            _emitter_data: emitter_data,
            _elapsed_time: 0.0,
            _lifetime: 0.0,
        }
    }
}

impl EffectManagerData {
    pub fn create_effect_manager_data(renderer_data: &RcRefCell<RendererData>, resources: &RcRefCell<Resources>) -> EffectManagerData {
        EffectManagerData {
            _renderer_data: renderer_data.clone(),
            _resources: resources.clone(),
            _effects: Vec::new(),
        }
    }

    pub fn destroy_effect_manager_data(&mut self) {

    }

    pub fn create_effect(&mut self, effect_data_name: &str) {
        let resources = &self._resources.borrow();
        let effect_data = resources.get_effect_data(effect_data_name);
        let effect_instance = EffectInstance::create_effect_instance(&effect_data);
        self._effects.push(effect_instance);
   }
}