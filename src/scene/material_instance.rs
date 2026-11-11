use std::collections::HashMap;

use ash::ext;
use ash::{Device, vk};

use crate::scene::material::MaterialData;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::descriptor::{
    DescriptorResourceInfo, DescriptorResourceType, create_descriptor_sets, create_write_descriptor_sets_with_update,
};
use crate::vulkan_context::render_pass::{
    PipelineData, PipelinePushConstantData, RenderPassData, RenderPassPipelineData, get_render_pass_pipeline_data_name,
};
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainArray;

static EMPTY_RENDER_PASS_PIPELINE_DATA_NAMES: Vec<String> = Vec::new();
pub const SKELETAL_GBUFFER_RENDER_PASS_NAME: &str = "render_pass_skeletal_gbuffer";

#[derive(Clone, Debug)]
pub struct PipelineBindingDataCreateInfo<'a> {
    pub _render_pass_pipeline_data: RenderPassPipelineData<'a>,
    pub _descriptor_resource_infos_list: SwapchainArray<Vec<DescriptorResourceInfo<'a>>>,
    pub _push_constant_data_list: Vec<PipelinePushConstantData>,
}

#[derive(Clone, Debug)]
pub struct PipelineBindingData<'a> {
    pub _render_pass_pipeline_data: RenderPassPipelineData<'a>,
    pub _descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    pub _write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet<'a>>>,
    pub _descriptor_set_count: u32,
    pub _descriptor_resource_infos_list: SwapchainArray<Vec<DescriptorResourceInfo<'a>>>,
    pub _push_constant_data_list: Vec<PipelinePushConstantData>,
}

type PipelineBindingDataMap<'a> = HashMap<String, PipelineBindingData<'a>>;

#[derive(Clone, Debug)]
pub struct MaterialInstanceData<'a> {
    pub _material_instance_data_name: String,
    pub _material_data: RcRefCell<MaterialData<'a>>,
    pub _material_parameters: serde_json::Map<String, serde_json::Value>,
    pub _render_pass_pipeline_data_names_map: HashMap<String, Vec<String>>,
    pub _pipeline_binding_data_map: PipelineBindingDataMap<'a>,
    pub _default_pipeline_binding_name: String,
}

impl<'a> PipelineBindingData<'a> {
    pub fn get_render_pass_data(&self) -> &RcRefCell<RenderPassData<'a>> {
        &self._render_pass_pipeline_data._render_pass_data
    }

    pub fn get_pipeline_data(&self) -> &RcRefCell<PipelineData<'a>> {
        &self._render_pass_pipeline_data._pipeline_data
    }

    pub fn get_pipeline_bind_point(&self) -> vk::PipelineBindPoint {
        self._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_bind_point
    }

    pub fn get_pipeline_layout(&self) -> vk::PipelineLayout {
        self._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_layout
    }
}

impl<'a> MaterialInstanceData<'a> {
    pub fn create_material_instance(
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        material_instance_data_name: &String,
        material_data: &RcRefCell<MaterialData<'a>>,
        material_parameters: &serde_json::Map<String, serde_json::Value>,
        pipeline_bind_create_infos: Vec<PipelineBindingDataCreateInfo<'a>>,
    ) -> MaterialInstanceData<'a> {
        log::debug!("create_material_instance: {}", material_instance_data_name);
        log::trace!("    material_data: {}", material_data.borrow()._material_data_name);
        let mut render_pass_pipeline_data_names_map: HashMap<String, Vec<String>> = HashMap::new();
        let mut pipeline_binding_data_map = PipelineBindingDataMap::new();
        let mut default_pipeline_binding_name = String::new();
        for pipeline_bind_create_info in pipeline_bind_create_infos {
            let render_pass_name =
                &pipeline_bind_create_info._render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name;
            let pipeline_name =
                &pipeline_bind_create_info._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name;
            let render_pass_pipeline_data_name = get_render_pass_pipeline_data_name(render_pass_name, pipeline_name);

            if default_pipeline_binding_name.is_empty() {
                default_pipeline_binding_name = render_pass_pipeline_data_name.clone();
            }

            log::trace!("        renderpass/pipeline: {}", render_pass_pipeline_data_name);
            let descriptor_data =
                &pipeline_bind_create_info._render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data;
            let descriptor_sets = create_descriptor_sets(
                device,
                debug_utils_device,
                render_pass_pipeline_data_name.as_str(),
                descriptor_data,
            );
            let descriptor_binding_indices: Vec<u32> = descriptor_data
                ._descriptor_data_create_infos
                .iter()
                .map(|descriptor_data_create_info| descriptor_data_create_info._descriptor_binding_index)
                .collect();
            let write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>> =
                create_write_descriptor_sets_with_update(
                    device,
                    &descriptor_sets,
                    &descriptor_binding_indices,
                    &descriptor_data._descriptor_set_layout_bindings,
                    &pipeline_bind_create_info._descriptor_resource_infos_list,
                );

            let pipeline_binding_data = PipelineBindingData {
                _render_pass_pipeline_data: pipeline_bind_create_info._render_pass_pipeline_data.clone(),
                _descriptor_sets: descriptor_sets,
                _write_descriptor_sets: write_descriptor_sets,
                _descriptor_set_count: descriptor_binding_indices.len() as u32,
                _descriptor_resource_infos_list: pipeline_bind_create_info._descriptor_resource_infos_list,
                _push_constant_data_list: pipeline_bind_create_info._push_constant_data_list.clone(),
            };

            // insert to pipeline_binding_data_map
            pipeline_binding_data_map.insert(render_pass_pipeline_data_name.clone(), pipeline_binding_data.clone());

            // insert to render_pass_pipeline_data_names_map
            if let Some(render_pass_pipeline_data_names) = render_pass_pipeline_data_names_map.get_mut(render_pass_name)
            {
                render_pass_pipeline_data_names.push(render_pass_pipeline_data_name.clone());
            } else {
                render_pass_pipeline_data_names_map
                    .insert(render_pass_name.clone(), vec![render_pass_pipeline_data_name.clone()]);
            }
        }

        MaterialInstanceData {
            _material_instance_data_name: material_instance_data_name.clone(),
            _material_data: material_data.clone(),
            _material_parameters: material_parameters.clone(),
            _render_pass_pipeline_data_names_map: render_pass_pipeline_data_names_map,
            _pipeline_binding_data_map: pipeline_binding_data_map,
            _default_pipeline_binding_name: default_pipeline_binding_name,
        }
    }

    pub fn is_ready(&self) -> bool {
        if !crate::core::engine_service_locator::is_engine_core_valid() {
            return true;
        }
        let engine_resources = crate::core::engine_service_locator::get_engine_resources();
        for (_key, value) in self._material_parameters.iter() {
            if let serde_json::Value::String(texture_name) = value {
                if let Some(entry) = engine_resources._texture_data_map.get(texture_name) {
                    match entry.get_state() {
                        crate::resource::resource::ResourceState::Unloaded => {
                            engine_resources.request_texture_data(texture_name);
                            return false;
                        }
                        crate::resource::resource::ResourceState::Loading => {
                            return false;
                        }
                        crate::resource::resource::ResourceState::Loaded
                        | crate::resource::resource::ResourceState::Failed => {}
                    }
                }
            }
        }
        true
    }

    pub fn set_material_parameter(&mut self, parameter_name: &str, value: serde_json::Value) {
        self._material_parameters.insert(String::from(parameter_name), value);
    }

    pub fn update_texture_descriptor_sets(
        &mut self,
        device: &Device,
        loaded_texture_name: &str,
        texture_data: &TextureData,
    ) {
        let mut uses_texture = false;
        for (_key, value) in self._material_parameters.iter() {
            if let serde_json::Value::String(tex_name) = value {
                if tex_name == loaded_texture_name {
                    uses_texture = true;
                    break;
                }
            }
        }
        if !uses_texture {
            return;
        }

        for pipeline_binding_data in self._pipeline_binding_data_map.values_mut() {
            let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
            let descriptor_data = &pipeline_data._descriptor_data;
            let mut updated_any_binding = false;

            for (descriptor_index, descriptor_data_create_info) in
                descriptor_data._descriptor_data_create_infos.iter().enumerate()
            {
                match descriptor_data_create_info._descriptor_resource_type {
                    DescriptorResourceType::Image
                    | DescriptorResourceType::Texture
                    | DescriptorResourceType::StorageTexture => {
                        let matches_param =
                            match self._material_parameters.get(&descriptor_data_create_info._descriptor_name) {
                                Some(serde_json::Value::String(tex_name)) => tex_name == loaded_texture_name,
                                _ => false,
                            };
                        if matches_param {
                            let new_resource_info = if descriptor_data_create_info.use_sub_image() {
                                DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_sub_image_info(
                                    descriptor_data_create_info._descriptor_image_layer,
                                    descriptor_data_create_info._descriptor_image_mip_level,
                                ))
                            } else {
                                DescriptorResourceInfo::DescriptorImageInfo(texture_data.get_default_image_info())
                            };
                            for swapchain_resources in pipeline_binding_data._descriptor_resource_infos_list.iter_mut()
                            {
                                if descriptor_index < swapchain_resources.len() {
                                    swapchain_resources[descriptor_index] = new_resource_info.clone();
                                    updated_any_binding = true;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            if updated_any_binding {
                unsafe {
                    device.device_wait_idle().expect("vkDeviceWaitIdle failed!");
                }
                let descriptor_binding_indices: Vec<u32> = descriptor_data
                    ._descriptor_data_create_infos
                    .iter()
                    .map(|info| info._descriptor_binding_index)
                    .collect();
                pipeline_binding_data._write_descriptor_sets = create_write_descriptor_sets_with_update(
                    device,
                    &pipeline_binding_data._descriptor_sets,
                    &descriptor_binding_indices,
                    &descriptor_data._descriptor_set_layout_bindings,
                    &pipeline_binding_data._descriptor_resource_infos_list,
                );
            }
        }
    }

    pub fn destroy_material_instance(&self) {
        log::debug!("destroy_material_instance: {}", self._material_instance_data_name);
    }

    pub fn get_default_pipeline_binding_data(&self) -> &PipelineBindingData<'a> {
        self._pipeline_binding_data_map.get(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_default_pipeline_binding_data_mut(&mut self) -> &mut PipelineBindingData<'a> {
        self._pipeline_binding_data_map.get_mut(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_pipeline_binding_data(&self, render_pass_pipeline_data_name: &str) -> &PipelineBindingData<'a> {
        self._pipeline_binding_data_map.get(render_pass_pipeline_data_name).unwrap()
    }

    pub fn get_pipeline_binding_data_mut(
        &mut self,
        render_pass_pipeline_data_name: &str,
    ) -> &mut PipelineBindingData<'a> {
        self._pipeline_binding_data_map.get_mut(render_pass_pipeline_data_name).unwrap()
    }

    pub fn has_render_pass(&self, render_pass_name: &str) -> bool {
        self._render_pass_pipeline_data_names_map.contains_key(render_pass_name)
    }

    pub fn is_skeletal_material_instance(&self) -> bool {
        self.has_render_pass(SKELETAL_GBUFFER_RENDER_PASS_NAME)
    }

    pub fn get_render_pass_pipeline_data_names(&self, render_pass_name: &str) -> &Vec<String> {
        if let Some(pipeline_data_names) = self._render_pass_pipeline_data_names_map.get(render_pass_name) {
            pipeline_data_names
        } else {
            log::warn!(
                "Render pass '{}' not found in material instance '{}' (material '{}')",
                render_pass_name,
                self._material_instance_data_name,
                self._material_data.borrow()._material_data_name
            );
            &EMPTY_RENDER_PASS_PIPELINE_DATA_NAMES
        }
    }
}
