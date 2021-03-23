use std::collections::HashMap;

use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4, Matrix4 };

use crate::constants;
use crate::renderer::renderer::RendererData;
use crate::renderer::material_instance::MaterialInstanceData;
use crate::renderer::mesh::MeshData;
use crate::renderer::transform_object::TransformObjectData;
use crate::resource::resource::{ DEFAULT_EFFECT_MATERIAL_INSTANCE_NAME, Resources };
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{ newRcRefCell, RcRefCell };
use crate::utilities::math;

const INVALID_EFFECT_ID: i64 = -1;
const INVALID_ALLOCATED_EMITTER_INDEX: i32 = -1;
const INVALID_ALLOCATED_PARTICLE_OFFSET: i32 = -1;

// must match with effect/common.glsl
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleSpawnVolumeType {
    Box = 0,
    Sphere = 1,
    Cone = 2,
    Cylinder = 3,
    Count = 4,
}

impl Default for ParticleSpawnVolumeType {
    fn default() -> Self {
        ParticleSpawnVolumeType::Box
    }
}

// must match with effect/common.glsl
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleGeometryType {
    Quad = 0,
    Decal = 1,
    Mesh = 2,
    Ribbon = 3,
    Beam = 4,
    Capsule = 5,
    Count = 6,
}

impl Default for ParticleGeometryType {
    fn default() -> Self {
        ParticleGeometryType::Quad
    }
}

// must match with effect/common.glsl
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleBlendMode {
    AlphaBlend = 0,
    Additive = 1,
    Opaque = 2,
    Count = 3,
}

impl Default for ParticleBlendMode {
    fn default() -> Self {
        ParticleBlendMode::AlphaBlend
    }
}

// must match with effect/common.glsl
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleAlignMode {
    None = 0,
    Billboard = 1,
    VelocityAlign = 2,
    Count = 3,
}

impl Default for ParticleAlignMode {
    fn default() -> Self {
        ParticleAlignMode::Billboard
    }
}

// must match with effect/common.glsl
#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum ParticleVelocityType {
    Local = 0,
    WorldY_LocalXZ = 1,
    NormalDirection = 2,
    Count = 3,
}

impl Default for ParticleVelocityType {
    fn default() -> Self {
        ParticleVelocityType::Local
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct EffectDataCreateInfo {
    pub _emitter_data_create_infos: Vec<EmitterDataCreateInfo>,
}

impl Default for EffectDataCreateInfo {
    fn default() -> EffectDataCreateInfo {
        EffectDataCreateInfo {
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
    pub _max_particle_count: i32,
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
            _max_particle_count: 100,
            _spawn_count: 1,
            _spawn_term: 0.1,
            _delay: 0.0,
            _particle_lifetime_min: 1.0,
            _particle_lifetime_max: 1.0,
            _align_mode: ParticleAlignMode::Billboard,
            _blend_mode: ParticleBlendMode::AlphaBlend,
            _geometry_type: ParticleGeometryType::Quad,
            _material_instance_name: String::from(DEFAULT_EFFECT_MATERIAL_INSTANCE_NAME),
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
}

pub struct EmitterData {
    pub _enable: bool,
    pub _emitter_data_name: String,
    pub _emitter_transform: Matrix4<f32>,
    pub _emitter_lifetime: f32,
    pub _spawn_volume_type: ParticleSpawnVolumeType,
    pub _spawn_volume_info: Vector4<f32>,
    pub _spawn_volume_transform: Matrix4<f32>,
    pub _max_particle_count: i32,
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
    pub _effect_manager_data: *const EffectManagerData,
    pub _effect_id: i64,
    pub _update_first_time: bool,
    pub _is_alive: bool,
    pub _is_culled_from_view: bool,
    pub _elapsed_time: f32,
    pub _bound_box: BoundingBox,
    pub _effect_transform: TransformObjectData,
    pub _effect_data: RcRefCell<EffectData>,
    pub _emitters: Vec<EmitterInstance>,
}

pub struct EmitterInstance {
    pub _parent_effect: *const EffectInstance,
    pub _is_alive: bool,
    pub _elapsed_time: f32,
    pub _remained_spawn_term: f32,
    pub _particle_spawn_count: i32,
    pub _allocated_emitter_index: i32,
    pub _allocated_particle_offset: i32,
    pub _allocated_particle_count: i32,
    pub _need_to_upload_static_constant_buffer: bool,
    pub _emitter_world_transform: Matrix4<f32>,
    pub _emitter_transform: TransformObjectData,
    pub _emitter_data: *const EmitterData,
}

pub trait EffectManagerBase {
    fn initialize_effect_manager(&mut self, effect_manager_data: *const EffectManagerData);
    fn gather_render_effects(&mut self);
}

pub struct EffectManagerData {
    pub _effect_manager: *const dyn EffectManagerBase,
    pub _renderer_data: RcRefCell<RendererData>,
    pub _resources: RcRefCell<Resources>,
    pub _effect_id_generator: i64,
    pub _effects: HashMap<i64, RcRefCell<EffectInstance>>,
    pub _dead_effect_ids: Vec<i64>,
    pub _allocated_emitters: Vec<*const EmitterInstance>,
    pub _allocated_emitter_count: i32,
    pub _allocated_particle_count: i32,
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
            _max_particle_count: emitter_data_create_info._max_particle_count,
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
    pub fn create_effect_instance(effect_manager_data: *const EffectManagerData, effect_id: i64, effec_create_info: &EffectCreateInfo, effect_data: &RcRefCell<EffectData>) -> RcRefCell<EffectInstance> {
        let emitters = effect_data.borrow()._emitter_datas.iter().map(|emitter_data| {
            EmitterInstance::create_emitter_instance(emitter_data)
        }).collect();

        let effect_instance = newRcRefCell(EffectInstance {
            _effect_manager_data: effect_manager_data,
            _effect_id: effect_id,
            _update_first_time: true,
            _is_alive: false,
            _is_culled_from_view: false,
            _elapsed_time: 0.0,
            _bound_box: BoundingBox::default(),
            _effect_transform: TransformObjectData::new_transform_object_data(),
            _effect_data: effect_data.clone(),
            _emitters: emitters,
        });
        effect_instance.borrow_mut().initialize_effect(effec_create_info);
        effect_instance
    }

    pub fn initialize_effect(&mut self, effec_create_info: &EffectCreateInfo) {
        self._effect_transform.set_position(&effec_create_info._effect_position);
        self._effect_transform.set_rotation(&effec_create_info._effect_rotation);
        self._effect_transform.set_scale(&effec_create_info._effect_scale);
        let parent_effect = self as *const EffectInstance;
        for emitter in self._emitters.iter_mut() {
            emitter.initialize_emitter(parent_effect);
        }
    }

    pub fn get_effect_manager_data(&self) -> &EffectManagerData {
        unsafe { &*self._effect_manager_data }
    }

    pub fn get_effect_manager_data_mut(&self) -> &mut EffectManagerData {
        unsafe { &mut *(self._effect_manager_data as *mut EffectManagerData) }
    }

    pub fn get_effect_world_transform(&self) -> &Matrix4<f32> {
        &self._effect_transform._matrix
    }

    pub fn is_valid_effect(&self) -> bool {
        INVALID_EFFECT_ID != self._effect_id
    }

    pub fn play_effect(&mut self) {
        self._is_alive = true;
        self._elapsed_time = 0.0;

        let effect_manager_data = self._effect_manager_data as *mut EffectManagerData;
        for emitter in self._emitters.iter_mut() {
            unsafe {
                (*effect_manager_data).allocate_emitter(emitter);
            }
            emitter.play_emitter();
        }
    }

    pub fn update_effect(&mut self, delta_time: f32) {
        if self._update_first_time {
            self.play_effect();
            self._update_first_time = false;
        }

        let updated_effect_transform = self._effect_transform.update_transform_object();

        let effect_manager_data = self._effect_manager_data as *mut EffectManagerData;
        let mut is_alive = false;
        for emitter in self._emitters.iter_mut() {
            if emitter._is_alive {
                emitter.update_emitter(delta_time, updated_effect_transform);
            }

            if false == emitter._is_alive {
                unsafe {
                    (*effect_manager_data).deallocate_emitter(emitter);
                }
            }
            is_alive |= emitter._is_alive;
        }
        self._is_alive = is_alive;
        self._elapsed_time += delta_time;
    }
}

impl EmitterInstance {
    pub fn create_emitter_instance(emitter_data: &EmitterData) -> EmitterInstance {
        EmitterInstance {
            _parent_effect: std::ptr::null(),
            _is_alive: false,
            _elapsed_time: 0.0,
            _remained_spawn_term: 0.0,
            _particle_spawn_count: 0,
            _allocated_emitter_index: INVALID_ALLOCATED_EMITTER_INDEX,
            _allocated_particle_offset: INVALID_ALLOCATED_PARTICLE_OFFSET,
            _allocated_particle_count: 0,
            _need_to_upload_static_constant_buffer: false,
            _emitter_world_transform: Matrix4::identity(),
            _emitter_transform: TransformObjectData::new_transform_object_data(),
            _emitter_data: emitter_data,
        }
    }

    pub fn initialize_emitter(&mut self, parent_effect: *const EffectInstance) {
        self._parent_effect = parent_effect;
    }

    pub fn get_parent_effect(&self) -> &EffectInstance {
        unsafe { &*self._parent_effect }
    }

    pub fn get_parent_effect_mut(&self) -> &mut EffectInstance {
        unsafe { &mut *(self._parent_effect as *mut EffectInstance) }
    }

    pub fn is_valid_allocated(&self) -> bool {
        INVALID_ALLOCATED_EMITTER_INDEX != self._allocated_emitter_index && INVALID_ALLOCATED_PARTICLE_OFFSET != self._allocated_particle_offset
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

    pub fn update_emitter(&mut self, delta_time: f32, updated_effect_transform: bool) {
        self._is_alive = if self.is_infinite_emitter() { true } else { self._elapsed_time <= self.get_emitter_data()._emitter_lifetime };
        self._particle_spawn_count = 0;
        // update
        if self._is_alive {
            let updated_emitter_transform = self._emitter_transform.update_transform_object();
            if updated_effect_transform || updated_emitter_transform {
                self._emitter_world_transform = self.get_parent_effect().get_effect_world_transform() * &self._emitter_transform._matrix;
            }

            // particle spawn
            if self._remained_spawn_term <= 0.0 {
                self._particle_spawn_count = self.get_emitter_data()._spawn_count;
                self._remained_spawn_term = self.get_emitter_data()._spawn_term;
            }
            self._remained_spawn_term -= delta_time;
        }
        self._elapsed_time += delta_time;
    }
}

// EffectManagerData
impl EffectManagerData {
    pub fn create_effect_manager_data(
        renderer_data: &RcRefCell<RendererData>,
        resources: &RcRefCell<Resources>,
        effect_manager: *const dyn EffectManagerBase
    ) -> EffectManagerData {
        EffectManagerData {
            _effect_manager: effect_manager,
            _renderer_data: renderer_data.clone(),
            _resources: resources.clone(),
            _effect_id_generator: 0,
            _effects: HashMap::new(),
            _dead_effect_ids: Vec::new(),
            _allocated_emitters: unsafe { vec![std::ptr::null(); constants::MAX_EMITTER_COUNT as usize] },
            _allocated_emitter_count: 0,
            _allocated_particle_count: 0,
        }
    }

    pub fn initialize_effect_manager(&mut self) {
        self.get_effect_manager_mut().initialize_effect_manager(self);
    }

    pub fn generate_effect_id(&mut self) -> i64 {
        let effect_id_generator = self._effect_id_generator;
        self._effect_id_generator += 1;
        effect_id_generator
    }

    pub fn destroy_effect_manager_data(&mut self) {
        self._effects.clear();
    }

    pub fn get_effect_manager(&self) -> &dyn EffectManagerBase {
        unsafe { &*self._effect_manager }
    }

    pub fn get_effect_manager_mut(&self) -> &mut dyn EffectManagerBase {
        unsafe { &mut *(self._effect_manager as *mut dyn EffectManagerBase) }
    }

    pub fn create_effect(&mut self, effect_create_info: &EffectCreateInfo) -> i64 {
        let effect_id = self.generate_effect_id();
        let resources = self._resources.borrow();
        let effect_data = resources.get_effect_data(&effect_create_info._effect_data_name);
        let effect_instance = EffectInstance::create_effect_instance(self, effect_id, effect_create_info, effect_data);
        self._effects.insert(effect_id, effect_instance);
        effect_id
   }

    pub fn get_effect(&self, effect_id: i64) -> Option<&RcRefCell<EffectInstance>> {
        self._effects.get(&effect_id)
    }

    pub fn get_effects(&self) -> &HashMap<i64, RcRefCell<EffectInstance>> {
        &self._effects
    }

    pub fn allocate_emitter(&mut self, emitter: &mut EmitterInstance) {
        if false == emitter.is_valid_allocated() {
            let available_emitter_count = unsafe { constants::MAX_EMITTER_COUNT - self._allocated_emitter_count };
            let available_particle_count = unsafe { constants::MAX_PARTICLE_COUNT - self._allocated_particle_count };
            if 0 < available_emitter_count && 0 < available_particle_count {
                emitter._allocated_particle_count = available_particle_count.min(emitter.get_emitter_data()._max_particle_count);
                emitter._allocated_particle_offset = self._allocated_particle_count;
                emitter._allocated_emitter_index = self._allocated_emitter_count;
                emitter._need_to_upload_static_constant_buffer = true;
                self._allocated_particle_count += emitter._allocated_particle_count;
                self._allocated_emitters[emitter._allocated_emitter_index as usize] = emitter;
                self._allocated_emitter_count += 1;
            }
        }
    }

    pub fn deallocate_emitter(&mut self, emitter: &mut EmitterInstance) {
        if emitter.is_valid_allocated() {
            self._allocated_emitters[emitter._allocated_emitter_index as usize] = std::ptr::null();
            emitter._allocated_particle_count = 0;
            emitter._allocated_particle_offset = INVALID_ALLOCATED_PARTICLE_OFFSET;
            emitter._allocated_emitter_index = INVALID_ALLOCATED_EMITTER_INDEX;
        }
    }

    pub fn update_effects(&mut self, delta_time: f32) {
        // update effects
        for (effect_id, effect) in self._effects.iter() {
            let mut effect = effect.borrow_mut();
            if effect._is_alive || effect._update_first_time {
                effect.update_effect(delta_time);
            }

            if false == effect._is_alive {
                self._dead_effect_ids.push(*effect_id);
            }
        }

        // unregist effect
        if false == self._dead_effect_ids.is_empty() {
            for dead_effect_id in self._dead_effect_ids.iter() {
                self._effects.remove(dead_effect_id);
            }
            self._dead_effect_ids.clear();
        }

        self.get_effect_manager_mut().gather_render_effects();
    }
}