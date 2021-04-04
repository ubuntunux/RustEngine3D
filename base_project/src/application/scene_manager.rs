use std::collections::HashMap;

use ash::Device;
use nalgebra::{
    Vector3,
    Vector4
};

use rust_engine_3d::constants;
use rust_engine_3d::application::scene_manager::{ SceneManagerBase, SceneManagerData };
use rust_engine_3d::renderer::effect::{EffectCreateInfo, EffectInstance, EffectManagerData, EffectManagerBase};
use rust_engine_3d::renderer::renderer::RendererData;
use rust_engine_3d::renderer::camera::{ CameraCreateInfo, CameraObjectData};
use rust_engine_3d::renderer::light::{ DirectionalLightCreateInfo, DirectionalLightData };
use rust_engine_3d::renderer::render_element::{ RenderElementData };
use rust_engine_3d::renderer::render_object::{ RenderObjectCreateInfo, RenderObjectData, AnimationPlayArgs };
use rust_engine_3d::renderer::light::LightConstants;
use rust_engine_3d::resource::resource::Resources;
use rust_engine_3d::utilities::system::{ self, RcRefCell, newRcRefCell };
use rust_engine_3d::utilities::bounding_box::BoundingBox;

use crate::application_constants;
use crate::renderer::effect::EffectManager;
use crate::renderer::fft_ocean::FFTOcean;
use crate::renderer::precomputed_atmosphere::Atmosphere;
use crate::renderer::renderer::Renderer;

type CameraObjectMap = HashMap<String, RcRefCell<CameraObjectData>>;
type DirectionalLightObjectMap = HashMap<String, RcRefCell<DirectionalLightData>>;
type RenderObjectMap = HashMap<String, RcRefCell<RenderObjectData>>;

#[derive(Clone)]
pub struct SceneManager {
    pub _scene_manager_data: *const SceneManagerData,
    pub _resources: *const Resources,
    pub _renderer: *const Renderer,
    pub _effect_manager: *const EffectManager,
    pub _window_width: u32,
    pub _window_height: u32,
    pub _main_camera: RcRefCell<CameraObjectData>,
    pub _main_light: RcRefCell<DirectionalLightData>,
    pub _capture_height_map: RcRefCell<DirectionalLightData>,
    pub _light_probe_cameras: Vec<RcRefCell<CameraObjectData>>,
    pub _camera_object_map: CameraObjectMap,
    pub _directional_light_object_map: DirectionalLightObjectMap,
    pub _static_render_object_map: RenderObjectMap,
    pub _skeletal_render_object_map: RenderObjectMap,
    pub _static_render_elements: Vec<RenderElementData>,
    pub _static_shadow_render_elements: Vec<RenderElementData>,
    pub _skeletal_render_elements: Vec<RenderElementData>,
    pub _skeletal_shadow_render_elements: Vec<RenderElementData>,
    pub _fft_ocean: RcRefCell<FFTOcean>,
    pub _atmosphere: RcRefCell<Atmosphere>,
}


impl SceneManagerBase for SceneManager {
    fn initialize_scene_manager(
        &mut self,
        window_width: u32,
        window_height: u32,
        scene_manager_data: &SceneManagerData,
        renderer_data: &RendererData,
        resources: &Resources,
        effect_manager_data: *const EffectManagerData,
    ) {
        self._renderer = renderer_data._renderer as *const Renderer;
        self._scene_manager_data = scene_manager_data;
        self._resources = resources;
        self._effect_manager = unsafe { (*effect_manager_data)._effect_manager as *const EffectManager };

        self.resized_window(window_width, window_height);

        self._fft_ocean.borrow_mut().regist_fft_ocean_textures(
            renderer_data,
            unsafe { &mut *(self._resources as *mut Resources) }
        );
    }

    fn initialize_scene_graphics_data(&self) {
        self._fft_ocean.borrow_mut().prepare_framebuffer_and_descriptors(self.get_renderer(), self.get_resources());
        self._atmosphere.borrow_mut().prepare_framebuffer_and_descriptors(self.get_renderer(), self.get_resources());
        self.get_effect_manager_mut().prepare_framebuffer_and_descriptors(self.get_renderer(), self.get_resources());
    }

    fn destroy_scene_graphics_data(&self, device: &Device) {
        self.get_effect_manager_mut().destroy_framebuffer_and_descriptors(device);
        self._fft_ocean.borrow_mut().destroy_fft_ocean(device);
        self._atmosphere.borrow_mut().destroy_atmosphere(device);
    }

    fn get_window_size(&self) -> (u32, u32) {
        (self._window_width, self._window_height)
    }
    fn set_window_size(&mut self, width: u32, height: u32) {
        self._window_width = width;
        self._window_height = height;
    }
    fn resized_window(&mut self, width: u32, height: u32) {
        self.set_window_size(width, height);
        self._main_camera.borrow_mut().set_aspect(width, height);
    }

    fn open_scene_manager_data(&mut self, resources: &Resources) {
        let camera_create_info = CameraCreateInfo {
            window_width: self._window_width,
            window_height: self._window_height,
            position: Vector3::new(2.0, 2.0, -1.0), // Vector3::new(-7.29, 6.345, -0.33),
            rotation: Vector3::new(-0.157, 1.3, 0.0), // Vector3::new(-0.287, -1.5625, 0.0),
            ..Default::default()
        };

        self.initialize_light_probe_cameras();

        self._main_camera = self.add_camera_object(&String::from("main_camera"), &camera_create_info);

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

        self.add_effect(&EffectCreateInfo {
            _effect_data_name: String::from("default"),
            _effect_position: Vector3::new(0.0, 4.0, 0.0),
            ..Default::default()
        });

        self.add_effect(&EffectCreateInfo {
            _effect_data_name: String::from("test"),
            _effect_position: Vector3::new(4.0, 4.0, 0.0),
            ..Default::default()
        });

        self.add_effect(&EffectCreateInfo {
            _effect_data_name: String::from("test2"),
            _effect_position: Vector3::new(8.0, 4.0, 0.0),
            ..Default::default()
        });

        // let model_data0 = resources.get_model_data("sponza/sponza").clone();
        // self.add_static_render_object("sponza", RenderObjectCreateInfo {
        //     _model_data: Some(model_data0),
        //     _position: Vector3::new(0.0, 0.0, 0.0),
        //     _scale: Vector3::new(0.1, 0.1, 0.1),
        //     ..Default::default()
        // });

        let sphere = resources.get_model_data("sphere").clone();
        self.add_static_render_object("sphere", RenderObjectCreateInfo {
            _model_data: Some(sphere),
            _position: Vector3::new(-2.0, 1.0, 0.0),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            ..Default::default()
        });

        // for i in 0..3 {
        //     let model_data = resources.get_model_data("skeletal").clone();
        //     let skeletal_actor = self.add_skeletal_render_object("skeletal", RenderObjectCreateInfo {
        //         _model_data: Some(model_data),
        //         _position: Vector3::new(i as f32, 1.0, 0.0),
        //         _scale: Vector3::new(0.01, 0.01, 0.01),
        //         ..Default::default()
        //     });
        //     skeletal_actor.borrow_mut()._animation_play_info.as_mut().unwrap().set_animation_play_info(&AnimationPlayArgs {
        //         _speed: (1.0 + i as f32 * 0.1),
        //         ..Default::default()
        //     });
        // }
    }

    fn close_scene_manager_data(&mut self, device: &Device) {
        self._camera_object_map.clear();
        self._directional_light_object_map.clear();
        self._static_render_object_map.clear();
        self._skeletal_render_object_map.clear();
        self._static_render_elements.clear();
        self._static_shadow_render_elements.clear();
        self._skeletal_render_elements.clear();
        self._skeletal_shadow_render_elements.clear();

        self.destroy_scene_graphics_data(device);
    }

    fn update_scene_manager_data(&mut self, _elapsed_time: f64, delta_time: f64) {
        self._fft_ocean.borrow_mut().update(delta_time);

        let mut main_camera = self._main_camera.borrow_mut();
        main_camera.update_camera_object_data();
        let camera_position = &main_camera.get_camera_position();

        let mut main_light = self._main_light.borrow_mut();
        main_light.update_light_data(camera_position);

        let mut capture_height_map = self._capture_height_map.borrow_mut();
        capture_height_map.update_light_data(camera_position);

        for (_key, render_object_data) in self._static_render_object_map.iter() {
            render_object_data.borrow_mut().update_render_object_data(delta_time as f32);
        }

        for (_key, render_object_data) in self._skeletal_render_object_map.iter() {
            render_object_data.borrow_mut().update_render_object_data(delta_time as f32);
        }

        self.get_effect_manager_mut().update_effects(delta_time as f32);

        // gather render elements
        SceneManager::gather_render_elements(
            &main_camera,
            &main_light,
            &self._static_render_object_map,
            &mut self._static_render_elements,
            &mut self._static_shadow_render_elements
        );

        SceneManager::gather_render_elements(
            &main_camera,
            &main_light,
            &self._skeletal_render_object_map,
            &mut self._skeletal_render_elements,
            &mut self._skeletal_shadow_render_elements
        );
    }
}

impl SceneManager {
    pub fn create_scene_manager() -> Box<SceneManager> {
        let default_camera = CameraObjectData::create_camera_object_data(&String::from("default_camera"), &CameraCreateInfo::default());
        let light_probe_camera_create_info = CameraCreateInfo {
            fov: 90.0,
            window_width: application_constants::LIGHT_PROBE_SIZE,
            window_height: application_constants::LIGHT_PROBE_SIZE,
            enable_jitter: false,
            ..Default::default()
        };
        let light_probe_cameras = vec![
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera0"), &light_probe_camera_create_info)),
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera1"), &light_probe_camera_create_info)),
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera2"), &light_probe_camera_create_info)),
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera3"), &light_probe_camera_create_info)),
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera4"), &light_probe_camera_create_info)),
            system::newRcRefCell(CameraObjectData::create_camera_object_data(&String::from("light_probe_camera5"), &light_probe_camera_create_info))
        ];
        let default_light = DirectionalLightData::create_light_data(&String::from("default_light"), &DirectionalLightCreateInfo::default());
        let capture_height_map = DirectionalLightData::create_light_data(
            &String::from("capture_height_map"),
            &DirectionalLightCreateInfo {
                _rotation: Vector3::new(std::f32::consts::PI * -0.5, 0.0, 0.0),
                _shadow_dimensions: Vector4::new(
                    application_constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                    application_constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                    -application_constants::CAPTURE_HEIGHT_MAP_DEPTH,
                    application_constants::CAPTURE_HEIGHT_MAP_DEPTH
                ),
                ..Default::default()
            }
        );
        let fft_ocean = system::newRcRefCell(FFTOcean::default());
        let atmosphere = system::newRcRefCell(Atmosphere::create_atmosphere(true));
        Box::new(SceneManager {
            _scene_manager_data: std::ptr::null(),
            _resources: std::ptr::null(),
            _renderer: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _window_width: default_camera._window_width,
            _window_height: default_camera._window_height,
            _main_camera: system::newRcRefCell(default_camera),
            _main_light: system::newRcRefCell(default_light),
            _capture_height_map: system::newRcRefCell(capture_height_map),
            _light_probe_cameras: light_probe_cameras,
            _camera_object_map: HashMap::new(),
            _directional_light_object_map: HashMap::new(),
            _static_render_object_map: HashMap::new(),
            _skeletal_render_object_map: HashMap::new(),
            _static_render_elements: Vec::new(),
            _static_shadow_render_elements: Vec::new(),
            _skeletal_render_elements: Vec::new(),
            _skeletal_shadow_render_elements: Vec::new(),
            _fft_ocean: fft_ocean,
            _atmosphere: atmosphere,
        })
    }
    pub fn get_scene_manager_data(&self) -> &SceneManagerData { unsafe { &*self._scene_manager_data } }
    pub fn get_scene_manager_data_mut(&self) -> &mut SceneManagerData { unsafe { &mut *(self._scene_manager_data as *mut SceneManagerData) } }
    pub fn get_renderer(&self) -> &Renderer { unsafe { &*self._renderer } }
    pub fn get_renderer_mut(&self) -> &mut Renderer { unsafe { &mut *(self._renderer as *mut Renderer) } }
    pub fn get_resources(&self) -> &Resources { unsafe { &*self._resources } }
    pub fn get_resources_mut(&self) -> &mut Resources { unsafe { &mut *(self._resources as *mut Resources) } }
    pub fn get_effect_manager(&self) -> &EffectManager { unsafe { &*self._effect_manager } }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager { unsafe { &mut *(self._effect_manager as *mut EffectManager) } }
    pub fn get_fft_ocean(&self) -> &RcRefCell<FFTOcean>  { &self._fft_ocean }
    pub fn get_atmosphere(&self) -> &RcRefCell<Atmosphere> { &self._atmosphere }
    pub fn get_main_camera(&self) -> &RcRefCell<CameraObjectData> { &self._main_camera }
    pub fn get_light_probe_camera(&self, index: usize) -> &RcRefCell<CameraObjectData> { &self._light_probe_cameras[index] }
    pub fn add_camera_object(&mut self, object_name: &str, camera_create_info: &CameraCreateInfo) -> RcRefCell<CameraObjectData> {
        let new_object_name = system::generate_unique_name(&self._camera_object_map, object_name);
        let camera_object_data = newRcRefCell(CameraObjectData::create_camera_object_data(&new_object_name, camera_create_info));
        self._camera_object_map.insert(new_object_name, camera_object_data.clone());
        camera_object_data
    }
    pub fn get_main_light(&self) -> &RcRefCell<DirectionalLightData> {
        &self._main_light
    }

    pub fn get_capture_height_map(&self) -> &RcRefCell<DirectionalLightData> {
        &self._capture_height_map
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

    pub fn get_static_shadow_render_elements(&self) -> &Vec<RenderElementData> {
        &self._static_shadow_render_elements
    }

    pub fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData> {
        &self._skeletal_render_elements
    }

    pub fn get_skeletal_shadow_render_elements(&self) -> &Vec<RenderElementData> {
        &self._skeletal_shadow_render_elements
    }

    pub fn add_effect(&mut self, effect_create_info: &EffectCreateInfo) -> i64 {
        self.get_effect_manager_mut().create_effect(effect_create_info)
    }

    pub fn get_effect(&self, effect_id: i64) -> Option<&RcRefCell<EffectInstance>> {
        self.get_effect_manager().get_effect(effect_id)
    }

    pub fn view_frustum_culling_geometry(camera: &CameraObjectData, geometry_bound_box: &BoundingBox) -> bool {
        let to_geometry = &geometry_bound_box._center - camera.get_camera_position();
        for plane in camera._view_frustum_planes.iter() {
            let d = plane.dot(&to_geometry);
            if geometry_bound_box._radius < d {
                return true;
            }
        }
        false
    }

    pub fn shadow_culling(light: &DirectionalLightData, geometry_bound_box: &BoundingBox) -> bool {
        let shadow_view_projection = light.get_shadow_view_projection();
        let bound_min: Vector4<f32> = shadow_view_projection * Vector4::new(geometry_bound_box._min.x, geometry_bound_box._min.y, geometry_bound_box._min.z, 1.0);
        let bound_max: Vector4<f32> = shadow_view_projection * Vector4::new(geometry_bound_box._max.x, geometry_bound_box._max.y, geometry_bound_box._max.z, 1.0);
        let minimum: Vector3<f32> = Vector3::new(bound_min.x.min(bound_max.x), bound_min.y.min(bound_max.y), bound_min.z.min(bound_max.z));
        let maximum: Vector3<f32> = Vector3::new(bound_min.x.max(bound_max.x), bound_min.y.max(bound_max.y), bound_min.z.max(bound_max.z));
        if maximum.x < -1.0 || 1.0 < minimum.x || maximum.y < -1.0 || 1.0 < minimum.y || maximum.z < -1.0 || 1.0 < minimum.z {
            return true;
        }
        false
    }

    pub fn gather_render_elements(
        camera: &CameraObjectData,
        light: &DirectionalLightData,
        render_object_map: &RenderObjectMap,
        render_elements: &mut Vec<RenderElementData>,
        render_shadow_elements: &mut Vec<RenderElementData>,
    ) {
        render_elements.clear();
        render_shadow_elements.clear();
        for (_key, render_object_data) in render_object_map.iter() {
            let render_object_data_ref = &render_object_data.borrow();
            let mode_data = render_object_data_ref.get_model_data().borrow();
            let mesh_data = mode_data.get_mesh_data().borrow();
            let geometry_datas = mesh_data.get_geomtry_datas();
            let geometry_bound_boxes = &render_object_data_ref._geometry_bound_boxes;
            let material_instance_datas = mode_data.get_material_instance_datas();
            for index in 0..geometry_datas.len() {
                if false == SceneManager::view_frustum_culling_geometry(camera, &geometry_bound_boxes[index]) {
                    render_elements.push(RenderElementData {
                        _render_object: render_object_data.clone(),
                        _geometry_data: geometry_datas[index].clone(),
                        _material_instance_data: material_instance_datas[index].clone(),
                    })
                }

                if false == SceneManager::shadow_culling(light, &geometry_bound_boxes[index]) {
                    render_shadow_elements.push(RenderElementData {
                        _render_object: render_object_data.clone(),
                        _geometry_data: geometry_datas[index].clone(),
                        _material_instance_data: material_instance_datas[index].clone(),
                    })
                }
            }
        }
    }

    pub fn initialize_light_probe_cameras(&mut self) {
        let pi = std::f32::consts::PI;
        let half_pi = std::f32::consts::PI * 0.5;
        let rotations: [Vector3<f32>; constants::CUBE_LAYER_COUNT] = [
            Vector3::new(0.0, half_pi, 0.0),
            Vector3::new(0.0, -half_pi, 0.0),
            Vector3::new(-half_pi, 0.0, 0.0),
            Vector3::new(half_pi, 0.0, 0.0),
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(0.0, pi, 0.0)
        ];
        let inverse_front = Vector3::new(1.0, 1.0, -1.0);
        for i in 0..constants::CUBE_LAYER_COUNT {
            self._light_probe_cameras[i].borrow_mut()._transform_object.set_rotation(&rotations[i]);
            self._light_probe_cameras[i].borrow_mut()._transform_object.set_scale(&inverse_front);
            self._light_probe_cameras[i].borrow_mut().update_camera_object_data();
        }
    }
}