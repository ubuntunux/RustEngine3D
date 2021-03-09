use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector2, Vector3, Vector4 };

use crate::utilities::system::{ RcRefCell };

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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
    pub _effect_name: String,
    pub _emitter_data_create_infos: Vec<EmitterDataCreateInfo>,
    pub _effect_position: Vector3<f32>,
    pub _effect_rotation: Vector3<f32>,
    pub _effect_scale: Vector3<f32>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct EmitterDataCreateInfo {
    pub _enable: bool,
    pub _emitter_name: String,
    pub _emitter_position: Vector3<f32>,
    pub _emitter_rotation: Vector3<f32>,
    pub _emitter_scale: Vector3<f32>,
    pub _emitter_lifetime: f32,
    pub _spawn_volume_type: ParticleSpawnVolumeType,
    pub _spawn_volume_info: Vector4<f32>,
    pub _spawn_volume_position: Vector3<f32>,
    pub _spawn_volume_rotation: Vector3<f32>,
    pub _spawn_volume_scale: Vector3<f32>,
    pub _spawn_count: Vector2<i32>,
    pub _spawn_term: Vector2<f32>,
    pub _delay: Vector2<f32>,
    pub _life_time: Vector2<f32>,
    pub _align_mode: ParticleAlignMode,
    pub _blend_mode: ParticleBlendMode,
    pub _geometry_type: ParticleGeometryType,
    pub _material_instance_name: String,
    pub _mesh_name: String,
    pub _rotation_pitch: Vector2<f32>,
    pub _rotation_yaw: Vector2<f32>,
    pub _rotation_roll: Vector2<f32>,
    pub _scale_x: Vector2<f32>,
    pub _scale_y: Vector2<f32>,
    pub _scale_z: Vector2<f32>,
}

pub struct EffectData {
}

pub struct EmitterData {
}

pub struct EffectInstance {
    pub _emitters: Vec<EmitterInstance>,
    pub _effect_data: RcRefCell<EffectData>,
    pub _elapsed_time: f32,
    pub _lifetime: f32,
}

pub struct EmitterInstance {
    pub _emitter_data: RcRefCell<EmitterData>,
    pub _elapsed_time: f32,
    pub _lifetime: f32,
}

pub struct EffectManagerData {
    _effects: Vec<EffectInstance>,
}

impl EffectData {
    pub fn destroy_effect_data(&mut self) {
    }
}