use std::collections::HashMap;

use ash::Device;
use nalgebra::{
    Vector3
};

use crate::renderer::{self, RendererData};
use crate::renderer::camera::{ CameraCreateInfo, CameraObjectData};
use crate::renderer::light::{ DirectionalLightCreateInfo, DirectionalLightData };
use crate::renderer::render_element::{ RenderElementData };
use crate::renderer::render_object::{ RenderObjectCreateInfo, RenderObjectData, AnimationPlayArgs };
use crate::renderer::fft_ocean::FFTOcean;
use crate::renderer::shader_buffer_datas::{ LightConstants };
use crate::resource::{ self, Resources };
use crate::utilities::system::{self, RcRefCell, newRcRefCell};

type CameraObjectMap = HashMap<String, RcRefCell<CameraObjectData>>;
type DirectionalLightObjectMap = HashMap<String, RcRefCell<DirectionalLightData>>;
type RenderObjectMap = HashMap<String, RcRefCell<RenderObjectData>>;

#[derive(Clone)]
pub struct SceneManagerData {
    pub _renderer_data: RcRefCell<RendererData>,
    pub _resources: RcRefCell<Resources>,
    pub _main_camera: RcRefCell<CameraObjectData>,
    pub _main_light: RcRefCell<DirectionalLightData>,
    pub _camera_object_map: CameraObjectMap,
    pub _directional_light_object_map: DirectionalLightObjectMap,
    pub _static_render_object_map: RenderObjectMap,
    pub _static_render_elements: Vec<RenderElementData>,
    pub _skeletal_render_object_map: RenderObjectMap,
    pub _skeletal_render_elements: Vec<RenderElementData>,
    pub _fft_ocean: RcRefCell<FFTOcean>,
}

pub fn create_scene_manager_data(
    renderer_data: RcRefCell<renderer::RendererData>,
    resources: RcRefCell<resource::Resources>
) -> RcRefCell<SceneManagerData> {
    let default_camera = CameraObjectData::create_camera_object_data(&String::from("default_camera"), &CameraCreateInfo::default());
    let default_light = DirectionalLightData::create_light_data(&String::from("default_light"), &DirectionalLightCreateInfo::default());
    let fft_ocean = system::newRcRefCell(FFTOcean::default());
    system::newRcRefCell(SceneManagerData {
        _renderer_data: renderer_data,
        _resources: resources,
        _main_camera: system::newRcRefCell(default_camera),
        _main_light: system::newRcRefCell(default_light),
        _camera_object_map: HashMap::new(),
        _directional_light_object_map: HashMap::new(),
        _static_render_object_map: HashMap::new(),
        _static_render_elements: Vec::new(),
        _skeletal_render_object_map: HashMap::new(),
        _skeletal_render_elements: Vec::new(),
        _fft_ocean: fft_ocean,
    })
}

impl SceneManagerData {
    pub fn open_scene_manager_data(&mut self, camera_create_info: &CameraCreateInfo) {
        self._main_camera = self.add_camera_object(&String::from("main_camera"), camera_create_info);
        let pitch: f32 = -std::f32::consts::PI * 0.47;
        self._main_light = self.add_light_object(&String::from("main_light"), &DirectionalLightCreateInfo {
            _position: Vector3::zeros(),
            _rotation: Vector3::new(pitch, 0.0, 0.3),
            _light_constants: LightConstants {
                _light_direction: Vector3::new(pitch, 0.0, 0.3),
                ..Default::default()
            },
            ..Default::default()
        });

        let model_data0 = self._resources.borrow().get_model_data("sponza/sponza").clone();
        self.add_static_render_object("sponza", RenderObjectCreateInfo {
            _model_data: Some(model_data0),
            _position: Vector3::new(0.0, 0.0, 0.0),
            _scale: Vector3::new(0.1, 0.1, 0.1),
            ..Default::default()
        });

        let sphere = self._resources.borrow().get_model_data("sphere").clone();
        self.add_static_render_object("sphere", RenderObjectCreateInfo {
            _model_data: Some(sphere),
            _position: Vector3::new(0.0, 1.0, 0.0),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            ..Default::default()
        });

        for i in 0..3 {
            let model_data = self._resources.borrow().get_model_data("skeletal").clone();
            let skeletal_actor = self.add_skeletal_render_object("skeletal", RenderObjectCreateInfo {
                _model_data: Some(model_data),
                _position: Vector3::new(i as f32, 1.5, 0.0),
                _scale: Vector3::new(0.01, 0.01, 0.01),
                ..Default::default()
            });
            skeletal_actor.borrow_mut()._animation_play_info.as_mut().unwrap().set_animation_play_info(&AnimationPlayArgs {
                _speed: (1.0 + i as f32 * 0.1),
                ..Default::default()
            });
        }
    }

    pub fn close_scene_manager_data(&mut self, device: &Device) {
        self.destroy_graphics_data(device);
    }

    pub fn initialize_graphics_data(&mut self, renderer_data: &RendererData) {
        self._fft_ocean.borrow_mut().initialize_fft_ocean(renderer_data, &self._resources);
    }

    pub fn destroy_graphics_data(&mut self, device: &Device) {
        self._fft_ocean.borrow_mut().destroy_fft_ocean(device);
    }

    pub fn get_fft_ocean(&self) -> &RcRefCell<FFTOcean> {
        &self._fft_ocean
    }

    pub fn get_main_camera(&self) -> &RcRefCell<CameraObjectData> {
        &self._main_camera
    }

    pub fn add_camera_object(&mut self, object_name: &str, camera_create_info: &CameraCreateInfo) -> RcRefCell<CameraObjectData> {
        let new_object_name = system::generate_unique_name(&self._camera_object_map, object_name);
        let camera_object_data = newRcRefCell(CameraObjectData::create_camera_object_data(&new_object_name, camera_create_info));
        self._camera_object_map.insert(new_object_name, camera_object_data.clone());
        camera_object_data
    }

    pub fn get_main_light(&self) -> &RcRefCell<DirectionalLightData> {
        &self._main_light
    }

    pub fn add_light_object(&mut self, object_name: &str, light_create_info: &DirectionalLightCreateInfo) -> RcRefCell<DirectionalLightData> {
        let new_object_name = system::generate_unique_name(&self._directional_light_object_map, object_name);
        let light_object_data = newRcRefCell(DirectionalLightData::create_light_data(&new_object_name, light_create_info));
        self._directional_light_object_map.insert(new_object_name, light_object_data.clone());
        light_object_data
    }

    pub fn add_static_render_object(&mut self, object_name: &str, render_object_create_info: RenderObjectCreateInfo) -> RcRefCell<RenderObjectData> {
        let new_object_name = system::generate_unique_name(&self._static_render_object_map, &object_name);
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(&new_object_name, render_object_create_info));
        self._static_render_object_map.insert(new_object_name, render_object_data.clone());
        render_object_data
    }

    pub fn add_skeletal_render_object(&mut self, object_name: &str, render_object_create_info: RenderObjectCreateInfo) -> RcRefCell<RenderObjectData> {
        let new_object_name = system::generate_unique_name(&self._skeletal_render_object_map, &object_name);
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(&new_object_name, render_object_create_info));
        self._skeletal_render_object_map.insert(new_object_name, render_object_data.clone());
        render_object_data
    }

    pub fn get_static_render_object(&self, object_name: &str) -> Option<&RcRefCell<RenderObjectData>> {
        self._static_render_object_map.get(object_name)
    }

    pub fn get_skeletal_render_object(&self, object_name: &str) -> Option<&RcRefCell<RenderObjectData>> {
        self._skeletal_render_object_map.get(object_name)
    }

    pub fn get_static_render_elements(&self) -> &Vec<RenderElementData> {
        &self._static_render_elements
    }

    pub fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData> {
        &self._skeletal_render_elements
    }

    pub fn gather_render_elements(render_object_map: &RenderObjectMap, render_elements: &mut Vec<RenderElementData>) {
        render_elements.clear();
        for (_key, render_object_data) in render_object_map.iter() {
            let render_object_data_ref = render_object_data.borrow();
            let mode_data = render_object_data_ref.get_model_data().borrow();
            let mesh_data = mode_data.get_mesh_data().borrow();
            let geometry_datas = mesh_data.get_geomtry_datas();
            let material_instance_datas = mode_data.get_material_instance_datas();
            for index in 0..geometry_datas.len() {
                render_elements.push(RenderElementData {
                    _render_object: render_object_data.clone(),
                    _geometry_data: geometry_datas[index].clone(),
                    _material_instance_data: material_instance_datas[index].clone(),
                })
            }
        }
    }

    pub fn update_scene_manager_data(&mut self, _elapsed_time: f64, delta_time: f64) {
        self._fft_ocean.borrow_mut().update(delta_time);

        let mut main_camera = self._main_camera.borrow_mut();
        main_camera.update_camera_object_data();
        let camera_position = &main_camera.get_camera_position();

        let mut main_light = self._main_light.borrow_mut();
        main_light.update_light_data(camera_position);

        for (_key, render_object_data) in self._static_render_object_map.iter() {
            render_object_data.borrow_mut().update_render_object_data(delta_time as f32);
        }

        for (_key, render_object_data) in self._skeletal_render_object_map.iter() {
            render_object_data.borrow_mut().update_render_object_data(delta_time as f32);
        }

        SceneManagerData::gather_render_elements(&self._static_render_object_map, &mut self._static_render_elements);
        SceneManagerData::gather_render_elements(&self._skeletal_render_object_map, &mut self._skeletal_render_elements);
    }
}