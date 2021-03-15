use std::collections::HashMap;

use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4, Matrix4 };

use crate::renderer::renderer::RendererData;
use crate::renderer::material_instance::MaterialInstanceData;
use crate::renderer::mesh::MeshData;
use crate::resource::resource::Resources;
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{ newRcRefCell, RcRefCell };
use crate::utilities::math;
use crate::renderer::transform_object::TransformObjectData;

const INVALID_EFFECT_ID: i64 = -1;

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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct EffectDataCreateInfo {
    pub _effect_position: Vector3<f32>,
    pub _effect_rotation: Vector3<f32>,
    pub _effect_scale: Vector3<f32>,
    pub _emitter_data_create_infos: Vec<EmitterDataCreateInfo>,
}

impl Default for EffectDataCreateInfo {
    fn default() -> EffectDataCreateInfo {
        EffectDataCreateInfo {
            _effect_position: Vector3::zeros(),
            _effect_rotation: Vector3::zeros(),
            _effect_scale: Vector3::new(1.0, 1.0, 1.0),
            _emitter_data_create_infos: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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
    pub _particle_lifetime_min: f32,
    pub _particle_lifetime_max: f32,
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

impl Default for EmitterDataCreateInfo {
    fn default() -> EmitterDataCreateInfo {
        EmitterDataCreateInfo {
            _enable: true,
            _emitter_data_name: String::new(),
            _emitter_position: Vector3::zeros(),
            _emitter_rotation: Vector3::zeros(),
            _emitter_scale: Vector3::new(1.0, 1.0, 1.0),
            _emitter_lifetime: 5.0,
            _spawn_volume_type: ParticleSpawnVolumeType::Sphere,
            _spawn_volume_info: Vector4::new(0.0, 1.0, 1.0, 1.0),
            _spawn_volume_position: Vector3::zeros(),
            _spawn_volume_rotation: Vector3::zeros(),
            _spawn_volume_scale: Vector3::new(1.0, 1.0, 1.0),
            _spawn_count: 10,
            _spawn_term: 0.1,
            _delay: 0.0,
            _particle_lifetime_min: 1.0,
            _particle_lifetime_max: 1.0,
            _align_mode: ParticleAlignMode::Billboard,
            _blend_mode: ParticleBlendMode::AlphaBlend,
            _geometry_type: ParticleGeometryType::Quad,
            _material_instance_name: String::new(),
            _mesh_name: String::new(),
            _rotation_min: Vector3::zeros(),
            _rotation_max: Vector3::zeros(),
            _scale_min: Vector3::new(1.0, 1.0, 1.0),
            _scale_max: Vector3::new(1.0, 1.0, 1.0),
        }
    }
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
    pub _particle_lifetime_min: f32,
    pub _particle_lifetime_max: f32,
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

pub struct EffectCreateInfo {
    pub _effect_position: Vector3<f32>,
    pub _effect_rotation: Vector3<f32>,
    pub _effect_scale: Vector3<f32>,
    pub _effect_data_name: String,
}

impl Default for EffectCreateInfo {
    fn default() -> EffectCreateInfo {
        EffectCreateInfo {
            _effect_position: Vector3::zeros(),
            _effect_rotation: Vector3::zeros(),
            _effect_scale: Vector3::new(1.0, 1.0, 1.0),
            _effect_data_name: String::new(),
        }
    }
}

pub struct EffectInstance {
    pub _effect_id: i64,
    pub _is_alive: bool,
    pub _is_culled_from_view: bool,
    pub _elapsed_time: f32,
    pub _bound_box: BoundingBox,
    pub _effect_transform: TransformObjectData,
    pub _effect_data: RcRefCell<EffectData>,
    pub _emitters: Vec<EmitterInstance>,
}

pub struct EmitterInstance {
    pub _is_alive: bool,
    pub _elapsed_time: f32,
    pub _remained_spawn_term: f32,
    pub _particle_spawn_count: i32,
    pub _emitter_transform: TransformObjectData,
    pub _emitter_data: *const EmitterData,
}

pub struct EffectManagerData {
    _renderer_data: RcRefCell<RendererData>,
    _resources: RcRefCell<Resources>,
    _effect_id_generator: i64,
    _effects: HashMap<i64, RcRefCell<EffectInstance>>,
    _dead_effect_ids: Vec<i64>,
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
            _particle_lifetime_min: emitter_data_create_info._particle_lifetime_min,
            _particle_lifetime_max: emitter_data_create_info._particle_lifetime_max,
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
    pub fn create_effect_instance(effect_id: i64, effec_create_info: &EffectCreateInfo, effect_data: &RcRefCell<EffectData>) -> RcRefCell<EffectInstance> {
        let emitters = effect_data.borrow()._emitter_datas.iter().map(|emitter_data| {
            EmitterInstance::create_emitter_instance(emitter_data)
        }).collect();

        let mut effect_instance = EffectInstance {
            _effect_id: effect_id,
            _is_alive: false,
            _is_culled_from_view: false,
            _elapsed_time: 0.0,
            _bound_box: BoundingBox::default(),
            _effect_transform: TransformObjectData::new_transform_object_data(),
            _effect_data: effect_data.clone(),
            _emitters: emitters,
        };
        effect_instance._effect_transform.set_position(&effec_create_info._effect_position);
        effect_instance._effect_transform.set_rotation(&effec_create_info._effect_rotation);
        effect_instance._effect_transform.set_scale(&effec_create_info._effect_scale);
        newRcRefCell(effect_instance)
    }

    pub fn is_valid_effect(&self) -> bool {
        INVALID_EFFECT_ID != self._effect_id
    }

    pub fn play_effect(&mut self) {
        self._is_alive = true;
        self._elapsed_time = 0.0;
    }

    pub fn update_effect(&mut self, delta_time: f32) {
        self._effect_transform.update_transform_object();

        let mut is_alive = false;
        for emitter in self._emitters.iter_mut() {
            if emitter._is_alive {
                emitter.update_emitter(delta_time);
            }
            is_alive |= emitter._is_alive;
        }
        self._is_alive = is_alive;
        self._elapsed_time += delta_time;

        if false == is_alive {
            log::info!("update_effect({:?}): dead!", self._elapsed_time);
        }
    }
}

impl EmitterInstance {
    pub fn create_emitter_instance(emitter_data: &EmitterData) -> EmitterInstance {
        EmitterInstance {
            _is_alive: false,
            _elapsed_time: 0.0,
            _remained_spawn_term: 0.0,
            _particle_spawn_count: 0,
            _emitter_transform: TransformObjectData::new_transform_object_data(),
            _emitter_data: emitter_data,
        }
    }

    pub fn is_infinite_emitter(&self) -> bool {
        self.get_emitter_data()._emitter_lifetime < 0.0
    }

    pub fn get_emitter_data(&self) -> &EmitterData {
        unsafe { &*self._emitter_data }
    }

    pub fn play_emitter(&mut self) {
        self._is_alive = true;
        self._elapsed_time = 0.0;
        self._remained_spawn_term = 0.0;
        self._particle_spawn_count = 0;
    }

    pub fn update_emitter(&mut self, delta_time: f32) {
        self._is_alive = if self.is_infinite_emitter() { true } else { self._elapsed_time <= self.get_emitter_data()._emitter_lifetime };
        self._particle_spawn_count = 0;
        // update
        if self._is_alive {
            self._emitter_transform.update_transform_object();

            // particle spawn
            if self._remained_spawn_term <= 0.0 {
                self._particle_spawn_count = self.get_emitter_data()._spawn_count;
                self._remained_spawn_term = self.get_emitter_data()._spawn_term;
                println!("    spawn: {:?}", self._particle_spawn_count);
            }
            self._remained_spawn_term -= delta_time;
        }
        self._elapsed_time += delta_time;

        if false == self._is_alive {
            log::info!("update_emitter({:?}): dead!", self._elapsed_time);
        }
    }
}

// EffectManagerData
impl EffectManagerData {
    pub fn create_effect_manager_data(renderer_data: &RcRefCell<RendererData>, resources: &RcRefCell<Resources>) -> EffectManagerData {
        EffectManagerData {
            _renderer_data: renderer_data.clone(),
            _resources: resources.clone(),
            _effect_id_generator: 0,
            _effects: HashMap::new(),
            _dead_effect_ids: Vec::new(),
        }
    }

    pub fn generate_effect_id(&mut self) -> i64 {
        let effect_id_generator = self._effect_id_generator;
        self._effect_id_generator += 1;
        effect_id_generator
    }

    pub fn destroy_effect_manager_data(&mut self) {
        self._effects.clear();
    }

    pub fn create_effect(&mut self, effect_create_info: &EffectCreateInfo) -> i64 {
        let effect_id = self.generate_effect_id();
        let resources = self._resources.borrow();
        let effect_data = resources.get_effect_data(&effect_create_info._effect_data_name);
        let effect_instance = EffectInstance::create_effect_instance(effect_id, effect_create_info, effect_data);
        self._effects.insert(effect_id, effect_instance);
        effect_id
   }

    pub fn get_effect(&self, effect_id: i64) -> Option<&RcRefCell<EffectInstance>> {
        self._effects.get(&effect_id)
    }

    pub fn update_effects(&mut self, delta_time: f32) {
        for (effect_id, effect) in self._effects.iter() {
            let mut effect = effect.borrow_mut();
            if effect._is_alive {
                effect.update_effect(delta_time);
            }

            if false == effect._is_alive {
                self._dead_effect_ids.push(*effect_id);
            }
        }

        if false == self._dead_effect_ids.is_empty() {
            for dead_effect_id in self._dead_effect_ids.iter() {
                self._effects.remove(dead_effect_id);
            }
            self._dead_effect_ids.clear();
        }
    }
}