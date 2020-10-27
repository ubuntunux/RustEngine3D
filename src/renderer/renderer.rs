use std::borrow::Cow;
use std::ffi::CStr;
use std::vec::Vec;
use ash::{
    vk,
    Device,
    Entry,
    Instance,
};
use ash::extensions::ext::DebugUtils;
use ash::extensions::khr::{
    Surface,
    Swapchain,
};
use ash::version::{
    InstanceV1_0,
};
use winit;
use winit::dpi;
use winit::window::{
    Window,
    WindowBuilder
};
use winit::event_loop::EventLoop;

use crate::constants;
use crate::resource;
use crate::vulkan_context::{
    command_buffer,
    device,
    geometry_buffer,
    queue,
    swapchain,
    sync,
    texture,
    uniform_buffer,
};
use crate::vulkan_context::swapchain::SwapchainData;
use crate::vulkan_context::uniform_buffer::UniformBufferData;
use crate::vulkan_context::vulkan_context::{
    RenderFeatures,
    SwapchainIndexMap,
    FrameIndexMap,
};
use crate::renderer::{
    image_sampler,
    uniform_buffer_data,
    render_target
};
use crate::renderer::render_target::{
    RenderTargetType,
    RenderTargetDataMap,
};
use crate::renderer::uniform_buffer_data::{
    UniformBufferType,
    UniformBufferDataMap,
};
use crate::renderer::post_process::{
    PostProcessData_SSAO
};
use crate::renderer::image_sampler::{
    ImageSamplerData
};
use crate::utilities::system::{
    self,
    RcRefCell,
};

// -- NOTE : sync with scene_constants.glsl
#[derive(Clone, Debug, Copy)]
pub enum RenderMode {
    RenderModeCommon,
    RenderModeShadow
}

#[derive(Clone, Debug, Copy)]
pub enum RenderObjectType {
    Static,
    Skeletal
}

pub unsafe extern "system" fn vulkan_debug_callback(
    message_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
    message_type: vk::DebugUtilsMessageTypeFlagsEXT,
    p_callback_data: *const vk::DebugUtilsMessengerCallbackDataEXT,
    _user_data: *mut std::os::raw::c_void,
) -> vk::Bool32 {
    let callback_data = *p_callback_data;
    let message_id_number: i32 = callback_data.message_id_number as i32;
    let message_id_name = if callback_data.p_message_id_name.is_null() {
        Cow::from("")
    } else {
        CStr::from_ptr(callback_data.p_message_id_name).to_string_lossy()
    };
    let message = if callback_data.p_message.is_null() {
        Cow::from("")
    } else {
        CStr::from_ptr(callback_data.p_message).to_string_lossy()
    };
    println!(
        "[{:?}]:{:?} [{} ({})] : {}",
        message_severity,
        message_type,
        message_id_name,
        &message_id_number.to_string(),
        message,
    );
    vk::FALSE
}

pub fn get_debug_message_level(debug_message_level: vk::DebugUtilsMessageSeverityFlagsEXT) -> vk::DebugUtilsMessageSeverityFlagsEXT {
    match debug_message_level {
        vk::DebugUtilsMessageSeverityFlagsEXT::INFO => (
            vk::DebugUtilsMessageSeverityFlagsEXT::INFO |
                vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        vk::DebugUtilsMessageSeverityFlagsEXT::WARNING => (
            vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        vk::DebugUtilsMessageSeverityFlagsEXT::ERROR => (
            vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        _ => (
            vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE |
                vk::DebugUtilsMessageSeverityFlagsEXT::INFO |
                vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
    }
}

pub struct RendererData {
    _frame_index: i32,
    _swapchain_index: u32,
    _need_recreate_swapchain: bool,
    pub _window: Window,
    pub _entry: Entry,
    pub _instance: Instance,
    pub _device: Device,
    pub _device_properties: vk::PhysicalDeviceProperties,
    pub _device_memory_properties: vk::PhysicalDeviceMemoryProperties,
    pub _physical_device: vk::PhysicalDevice,
    pub _surface: vk::SurfaceKHR,
    pub _surface_interface: Surface,
    pub _swapchain_data: swapchain::SwapchainData,
    pub _swapchain_support_details: swapchain::SwapchainSupportDetails,
    pub _swapchain_interface: Swapchain,
    pub _debug_util_interface: DebugUtils,
    pub _debug_call_back: vk::DebugUtilsMessengerEXT,
    pub _image_available_semaphores: FrameIndexMap<vk::Semaphore>,
    pub _render_finished_semaphores: FrameIndexMap<vk::Semaphore>,
    pub _queue_family_datas: queue::QueueFamilyDatas,
    pub _frame_fences: Vec<vk::Fence>,
    pub _command_pool: vk::CommandPool,
    pub _command_buffers: SwapchainIndexMap<vk::CommandBuffer>,
    pub _render_features: RenderFeatures,
    pub _image_samplers: ImageSamplerData,
    pub _debug_render_target: RenderTargetType,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _uniform_buffer_data_map: UniformBufferDataMap,
    pub _postprocess_ssao: PostProcessData_SSAO,
    pub _resources: RcRefCell<resource::Resources>
}

pub fn create_renderer_data<T>(
    app_name: &str,
    app_version: u32,
    (window_width, window_height): (u32, u32),
    event_loop: &EventLoop<T>,
    resources: RcRefCell<resource::Resources>
) -> RcRefCell<RendererData> {
    unsafe {
        log::info!("create_renderer_data: {}, width: {}, height: {}", constants::ENGINE_NAME, window_width, window_height);
        let window = WindowBuilder::new()
            .with_title(app_name)
            .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize {width: window_width, height: window_height}))
            .build(&event_loop)
            .unwrap();
        let entry = Entry::new().unwrap();
        let surface_extensions = ash_window::enumerate_required_extensions(&window).unwrap();
        let instance: Instance = device::create_vk_instance(&entry, &app_name, app_version, &surface_extensions);
        let surface = device::create_vk_surface(&entry, &instance, &window);
        let surface_interface = Surface::new(&entry, &instance);
        let (physical_device, swapchain_support_details, physical_device_features) = device::select_physical_device(&instance, &surface_interface, surface).unwrap();
        let device_properties: vk::PhysicalDeviceProperties = instance.get_physical_device_properties(physical_device);
        let device_memory_properties: vk::PhysicalDeviceMemoryProperties = instance.get_physical_device_memory_properties(physical_device);
        let msaa_samples = device::get_max_usable_sample_count(&device_properties);
        let queue_family_indices = queue::get_queue_family_indices(
            &instance,
            &surface_interface,
            surface,
            physical_device,
            constants::IS_CONCURRENT_MODE
        );
        let render_features = RenderFeatures {
            _physical_device_features: physical_device_features.clone(),
            _msaa_samples: msaa_samples,
        };
        let graphics_queue_index = queue_family_indices._graphics_queue_index;
        let present_queue_index = queue_family_indices._present_queue_index;
        let queue_family_index_set: Vec<u32> = if graphics_queue_index == present_queue_index {
            vec![graphics_queue_index]
        } else {
            vec![graphics_queue_index, present_queue_index]
        };
        let device = device::create_device(&instance, physical_device, &render_features, &queue_family_index_set);
        let queue_map = queue::create_queues(&device, &queue_family_index_set);
        let default_queue: &vk::Queue = queue_map.get(&queue_family_index_set[0]).unwrap();
        let queue_family_datas = queue::QueueFamilyDatas {
            _graphics_queue: queue_map.get(&graphics_queue_index).unwrap_or(default_queue).clone(),
            _present_queue: queue_map.get(&present_queue_index).unwrap_or(default_queue).clone(),
            _queue_family_index_list: queue_family_index_set.clone(),
            _queue_family_count: queue_map.len() as u32,
            _queue_family_indices: queue_family_indices.clone()
        };
        let swapchain_interface = Swapchain::new(&instance, &device);
        let swapchain_data: swapchain::SwapchainData = swapchain::create_swapchain_data(
            &device,
            &swapchain_interface,
            surface,
            &swapchain_support_details,
            &queue_family_datas,
            constants::ENABLE_IMMEDIATE_MODE
        );
        let image_available_semaphores = sync::create_semaphores(&device);
        let render_finished_semaphores = sync::create_semaphores(&device);
        let frame_fences = sync::create_fences(&device);
        let command_pool = command_buffer::create_command_pool(&device, &queue_family_datas);
        let command_buffers = command_buffer::create_command_buffers(&device, command_pool, constants::SWAPCHAIN_IMAGE_COUNT as u32);

        // debug utils
        let debug_message_level = get_debug_message_level(constants::DEBUG_MESSAGE_LEVEL);
        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(debug_message_level)
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(vulkan_debug_callback))
            .build();
        let debug_util_interface = DebugUtils::new(&entry, &instance);
        let debug_call_back = debug_util_interface.create_debug_utils_messenger(&debug_info, None).unwrap();
        let mut renderer_data = RendererData {
            _frame_index: 0,
            _swapchain_index: 0,
            _need_recreate_swapchain: false,
            _window: window,
            _entry: entry,
            _instance: instance,
            _device: device,
            _device_properties: device_properties,
            _device_memory_properties: device_memory_properties,
            _physical_device: physical_device,
            _surface: surface,
            _surface_interface: surface_interface,
            _swapchain_data: swapchain_data,
            _swapchain_support_details: swapchain_support_details,
            _swapchain_interface: swapchain_interface,
            _debug_util_interface: debug_util_interface,
            _debug_call_back: debug_call_back,
            _image_available_semaphores: image_available_semaphores,
            _render_finished_semaphores: render_finished_semaphores,
            _queue_family_datas: queue_family_datas,
            _frame_fences: frame_fences,
            _command_pool: command_pool,
            _command_buffers: command_buffers,
            _render_features: render_features,
            _image_samplers: ImageSamplerData::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _render_target_data_map: RenderTargetDataMap::new(),
            _uniform_buffer_data_map: UniformBufferDataMap::new(),
            _postprocess_ssao: PostProcessData_SSAO::default(),
            _resources: resources.clone(),
        };

        renderer_data.initialize_renderer();

        system::newRcRefCell(renderer_data)
    }
}

impl RendererData {
    pub fn get_need_recreate_swapchain(&self) -> bool {
        self._need_recreate_swapchain
    }

    pub fn set_need_recreate_swapchain(&mut self, value: bool) {
        self._need_recreate_swapchain = value;
    }

    pub fn recreate_swapchain(&self) {
        log::info!("recreate_swapchain");
    }

    pub fn destroy_renderer_data(&mut self) {
        unsafe {
            for uniform_buffer_data in self._uniform_buffer_data_map.values() {
                uniform_buffer::destroy_uniform_buffer_data(self.get_device(), uniform_buffer_data);
            }
            self._uniform_buffer_data_map.clear();

            image_sampler::destroy_image_samplers(self.get_device(), &self._image_samplers);

            for render_target_data in self._render_target_data_map.values() {
                texture::destroy_texture_data(self.get_device(), render_target_data);
            }
            self._render_target_data_map.clear();

            sync::destroy_semaphores(&self._device, &self._image_available_semaphores);
            sync::destroy_semaphores(&self._device, &self._render_finished_semaphores);
            sync::destroy_fences(&self._device, &self._frame_fences);
            command_buffer::destroy_command_buffers(&self._device, self._command_pool, &self._command_buffers);
            command_buffer::destroy_command_pool(&self._device, self._command_pool);
            swapchain::destroy_swapchain_data(&self._device, &self._swapchain_interface, &self._swapchain_data);
            device::destroy_device(&self._device);
            device::destroy_vk_surface(&self._surface_interface, self._surface);
            self._debug_util_interface.destroy_debug_utils_messenger(self._debug_call_back, None);
            device::destroy_vk_instance(&self._instance);
        }
    }

    pub fn get_instance(&self) -> &Instance { &self._instance }
    pub fn get_device(&self) -> &Device { &self._device }
    pub fn get_device_properties(&self) -> &vk::PhysicalDeviceProperties { &self._device_properties }
    pub fn get_device_memory_properties(&self) -> &vk::PhysicalDeviceMemoryProperties { &self._device_memory_properties }
    pub fn get_physical_device(&self) -> vk::PhysicalDevice { self._physical_device }
    pub fn get_swap_chain_data(&self) -> &SwapchainData { &self._swapchain_data }
    pub fn get_swap_chain_image_views(&self) -> &SwapchainIndexMap<vk::ImageView> { &self._swapchain_data._swapchain_image_views }
    pub fn get_swap_chain_support_details(&self) -> &swapchain::SwapchainSupportDetails { &self._swapchain_support_details }
    pub fn get_swap_chain_index(&self) -> u32 { self._swapchain_index }
    pub fn get_command_pool(&self) -> vk::CommandPool { self._command_pool }
    pub fn get_command_buffers(&self) -> &SwapchainIndexMap<vk::CommandBuffer> { &self._command_buffers }
    pub fn get_command_buffer(&self, index: usize) -> vk::CommandBuffer { self._command_buffers[index] }
    pub fn get_current_command_buffer(&self) -> vk::CommandBuffer { self._command_buffers[self._swapchain_index as usize] }
    pub fn get_graphics_queue(&self) -> vk::Queue { self._queue_family_datas._graphics_queue }
    pub fn get_present_queue(&self) -> vk::Queue { self._queue_family_datas._present_queue }
    pub fn get_uniform_buffer_data(&self, uniform_buffer_type: UniformBufferType) -> &UniformBufferData {
        &self._uniform_buffer_data_map.get(&uniform_buffer_type).unwrap()
    }

    pub fn next_debug_render_target(&mut self) {
        self._debug_render_target = if RenderTargetType::MaxBound == self._debug_render_target {
            unsafe { std::mem::transmute(0) }
        } else {
            let enum_to_int: i32 = self._debug_render_target.clone() as i32;
            unsafe { std::mem::transmute(enum_to_int + 1) }
        };
        log::info!("Current DebugRenderTarget: {:?}", self._debug_render_target);
    }

    pub fn prev_debug_render_target(&mut self) {
        let enum_to_int: i32 = self._debug_render_target.clone() as i32;
        self._debug_render_target = if 0 == enum_to_int {
            unsafe { std::mem::transmute(RenderTargetType::MaxBound as i32 - 1) }
        } else {

            unsafe { std::mem::transmute(enum_to_int - 1) }
        };
        log::info!("Current DebugRenderTarget: {:?}", self._debug_render_target);
    }

    pub fn create_render_target<T>(&self, texture_data_name: &String, texture_create_info: &texture::TextureCreateInfo<T>) -> texture::TextureData {
        texture::create_render_target(
            texture_data_name,
            self.get_instance(),
            self.get_device(),
            self.get_physical_device(),
            self.get_device_memory_properties(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            texture_create_info
        )
    }

    pub fn create_texture<T: Copy>(&self, texture_data_name: &String, texture_create_info: &texture::TextureCreateInfo<T>) -> texture::TextureData {
        texture::create_texture_data(
            texture_data_name,
            self.get_instance(),
            self.get_device(),
            self.get_physical_device(),
            self.get_device_memory_properties(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            texture_create_info
        )
    }

    pub fn destroy_texture(&self, texture_data: &texture::TextureData) {
        texture::destroy_texture_data(self.get_device(), texture_data);
    }

    pub fn get_render_target(&self, render_target_type: RenderTargetType) -> &texture::TextureData {
        &self._render_target_data_map.get(&render_target_type).unwrap()
    }

    pub fn create_geometry_buffer(
        &self,
        geometry_name: &String,
        geometry_create_info: &geometry_buffer::GeometryCreateInfo
    ) -> geometry_buffer::GeometryData {
        geometry_buffer::create_geometry_data(
            self.get_device(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            self.get_device_memory_properties(),
            geometry_name,
            geometry_create_info
        )
    }

    pub fn destroy_geomtry_buffer(&self, geometry_data: &geometry_buffer::GeometryData) {
        geometry_buffer::destroy_geometry_data(self.get_device(), geometry_data);
    }

    // renderPipeline :: RendererData -> VkCommandBuffer -> Int -> Text.Text -> Text.Text -> Text.Text -> GeometryData -> IO ()
    // renderPipeline rendererData commandBuffer swapChainIndex renderPassName pipelineName materialInstanceName geometryData = do
    //     materialInstanceData <- getMaterialInstanceData (_resources rendererData) materialInstanceName
    //     let pipelineBindingData = MaterialInstance.getPipelineBindingData materialInstanceData (renderPassName, pipelineName)
    //         renderPassData = MaterialInstance._renderPassData pipelineBindingData
    //         pipelineData = MaterialInstance._pipelineData pipelineBindingData
    //     beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
    //     bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData
    //     drawElements rendererData commandBuffer geometryData
    //     endRenderPass rendererData commandBuffer
    //
    // renderPipeline1 :: RendererData -> VkCommandBuffer -> Int -> Text.Text -> GeometryData -> IO ()
    // renderPipeline1 rendererData commandBuffer swapChainIndex renderPassName geometryData =
    //     renderPipeline rendererData commandBuffer swapChainIndex renderPassName renderPassName renderPassName geometryData
    //
    // beginRenderPassPipeline :: RendererData -> VkCommandBuffer -> Int -> RenderPass.RenderPassData -> RenderPass.PipelineData -> IO ()
    // beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData = do
    //     Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass.getRenderPassFrameBufferName renderPassData)
    //     let renderPassBeginInfo = atSwapChainIndex swapChainIndex (_renderPassBeginInfos frameBufferData)
    //         pipelineDynamicStates = RenderPass._pipelineDynamicStates pipelineData
    //     withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
    //         vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
    //     when (elem VK_DYNAMIC_STATE_VIEWPORT pipelineDynamicStates) $
    //         withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
    //             vkCmdSetViewport commandBuffer 0 1 viewPortPtr
    //     when (elem VK_DYNAMIC_STATE_SCISSOR pipelineDynamicStates) $
    //         withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
    //             vkCmdSetScissor commandBuffer 0 1 scissorRectPtr
    //     vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS (RenderPass._pipeline pipelineData)
    //
    // beginRenderPassPipeline' :: RendererData -> VkCommandBuffer -> Int -> RenderPass.RenderPassPipelineDataName -> IO (RenderPass.RenderPassData, RenderPass.PipelineData)
    // beginRenderPassPipeline' rendererData commandBuffer swapChainIndex renderPassPipelineDataName = do
    //     (renderPassData, pipelineData) <- getRenderPassPipelineData (_resources rendererData) renderPassPipelineDataName
    //     beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
    //     return (renderPassData, pipelineData)
    //
    // bindDescriptorSets :: RendererData -> VkCommandBuffer -> Int -> MaterialInstance.PipelineBindingData -> IO ()
    // bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData = do
    //     let pipelineLayout = RenderPass._pipelineLayout . MaterialInstance._pipelineData $ pipelineBindingData
    //         descriptorSetsPtr = MaterialInstance._descriptorSetsPtr pipelineBindingData
    //     vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 (ptrAtIndex descriptorSetsPtr swapChainIndex) 0 VK_NULL
    //
    // updateDescriptorSet :: RendererData -> Int -> MaterialInstance.PipelineBindingData -> Int -> Descriptor.DescriptorResourceInfo -> IO ()
    // updateDescriptorSet rendererData swapChainIndex pipelineBindingData descriptorOffset descriptorResourceInfo = do
    //     let writeDescriptorSetPtr = atSwapChainIndex swapChainIndex (MaterialInstance._writeDescriptorSetPtrs pipelineBindingData)
    //         writeDescriptorSetPtrOffset = ptrAtIndex writeDescriptorSetPtr descriptorOffset
    //     Descriptor.updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo
    //     vkUpdateDescriptorSets (_device rendererData) 1 writeDescriptorSetPtrOffset 0 VK_NULL
    //
    // updateDescriptorSets :: RendererData -> Int -> MaterialInstance.PipelineBindingData -> [(Int, Descriptor.DescriptorResourceInfo)] -> IO ()
    // updateDescriptorSets rendererData swapChainIndex pipelineBindingData descriptorInfos = do
    //     let writeDescriptorSetPtr = atSwapChainIndex swapChainIndex (MaterialInstance._writeDescriptorSetPtrs pipelineBindingData)
    //         descriptorWriteCount = MaterialInstance._descriptorSetCount pipelineBindingData
    //     forM_ descriptorInfos $ \(descriptorOffset, descriptorResourceInfo) -> do
    //         Descriptor.updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo
    //     vkUpdateDescriptorSets (_device rendererData) (fromIntegral descriptorWriteCount) writeDescriptorSetPtr 0 VK_NULL
    //
    // uploadPushConstantData :: RendererData -> VkCommandBuffer -> RenderPass.PipelineData -> PushConstantData -> IO ()
    // uploadPushConstantData rendererData commandBuffer pipelineData pushConstantData =
    //     with pushConstantData $ \pushConstantDataPtr ->
    //         vkCmdPushConstants commandBuffer (RenderPass._pipelineLayout pipelineData) VK_SHADER_STAGE_ALL 0 (bSizeOf pushConstantData) (castPtr pushConstantDataPtr)
    //
    // drawElements rendererData commandBuffer geometryData = do
    //     vkCmdBindVertexBuffers commandBuffer 0 1 (_vertexBufferPtr geometryData) (_vertexOffsetPtr rendererData)
    //     vkCmdBindIndexBuffer commandBuffer (_indexBuffer geometryData) 0 VK_INDEX_TYPE_UINT32
    //     vkCmdDrawIndexed commandBuffer (_vertexIndexCount geometryData) 1 0 0 0
    //
    // endRenderPass _ commandBuffer = vkCmdEndRenderPass commandBuffer
    //
    // deviceWaitIdle rendererData =
    //     throwingVK "vkDeviceWaitIdle failed!" (vkDeviceWaitIdle $ getDevice rendererData)
    //
    pub fn initialize_renderer(&mut self) {
        self._swapchain_index = 0;
        self._frame_index = 0;
        self._need_recreate_swapchain = false;

        uniform_buffer_data::regist_uniform_datas(
            &self._device,
            &self._device_memory_properties,
            &mut self._uniform_buffer_data_map
        );

        self._image_samplers = image_sampler::create_image_samplers(self.get_device());

        render_target::create_render_targets(self);
    }

    // createRenderer :: GLFW.Window
    //                -> String
    //                -> String
    //                -> Bool
    //                -> Bool
    //                -> [CString]
    //                -> Resources
    //                -> IO RendererData
    // createRenderer window progName engineName enableValidationLayer isConcurrentMode requireExtensions resources = do
    //     let validationLayers = if enableValidationLayer then Constants.vulkanLayers else []
    //     if enableValidationLayer
    //     then logInfo $ "Enable validation layers : " ++ show validationLayers
    //     else logInfo $ "Disabled validation layers"
    //
    //     defaultRendererData <- defaultRendererData resources
    //
    //     vkInstance <- createVulkanInstance progName engineName validationLayers requireExtensions
    //     vkSurface <- createVkSurface vkInstance window
    //     (physicalDevice, Just swapChainSupportDetails, supportedFeatures) <-
    //         selectPhysicalDevice vkInstance (Just vkSurface)
    //     deviceProperties <- getPhysicalDeviceProperties physicalDevice
    //     msaaSamples <- getMaxUsableSampleCount deviceProperties
    //     queueFamilyIndices <- getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode
    //     let renderFeatures = RenderFeatures
    //             { _anisotropyEnable = getField @"samplerAnisotropy" supportedFeatures
    //             , _msaaSamples = msaaSamples }
    //     let graphicsQueueIndex = _graphicsQueueIndex queueFamilyIndices
    //         presentQueueIndex = _presentQueueIndex queueFamilyIndices
    //         queueFamilyIndexList = Set.toList $ Set.fromList [graphicsQueueIndex, presentQueueIndex]
    //     device <- createDevice physicalDevice renderFeatures queueFamilyIndexList
    //     queueMap <- createQueues device queueFamilyIndexList
    //     let defaultQueue = (Map.elems queueMap) !! 0
    //         queueFamilyDatas = QueueFamilyDatas
    //             { _graphicsQueue = Maybe.fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex queueMap
    //             , _presentQueue = Maybe.fromMaybe defaultQueue $ Map.lookup presentQueueIndex queueMap
    //             , _queueFamilyIndexList = queueFamilyIndexList
    //             , _queueFamilyCount = fromIntegral $ length queueMap
    //             , _queueFamilyIndices = queueFamilyIndices }
    //     commandPool <- createCommandPool device queueFamilyDatas
    //     imageAvailableSemaphores <- createSemaphores device
    //     renderFinishedSemaphores <- createSemaphores device
    //     frameFencesPtr <- createFrameFences device
    //
    //     swapChainData <- createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface Constants.enableImmediateMode
    //     swapChainDataRef <- newIORef swapChainData
    //     swapChainSupportDetailsRef <- newIORef swapChainSupportDetails
    //
    //     let commandBufferCount = Constants.swapChainImageCount
    //     commandBuffersPtr <- mallocArray commandBufferCount::IO (Ptr VkCommandBuffer)
    //     createCommandBuffers device commandPool commandBufferCount commandBuffersPtr
    //     commandBuffers <- peekArray commandBufferCount commandBuffersPtr
    //
    //     let rendererData = defaultRendererData
    //             { _imageAvailableSemaphores = imageAvailableSemaphores
    //             , _renderFinishedSemaphores = renderFinishedSemaphores
    //             , _vkInstance = vkInstance
    //             , _vkSurface = vkSurface
    //             , _device = device
    //             , _physicalDevice = physicalDevice
    //             , _queueFamilyDatas = queueFamilyDatas
    //             , _frameFencesPtr = frameFencesPtr
    //             , _commandPool = commandPool
    //             , _commandBuffersPtr = commandBuffersPtr
    //             , _commandBufferCount = commandBufferCount
    //             , _swapChainDataRef = swapChainDataRef
    //             , _swapChainSupportDetailsRef = swapChainSupportDetailsRef
    //             , _renderFeatures = renderFeatures
    //             }
    //
    //     initializeRenderer rendererData
    //
    //
    // resizeWindow :: GLFW.Window -> RendererData -> IO ()
    // resizeWindow window rendererData@RendererData {..} = do
    //     logInfo "<< resizeWindow >>"
    //
    //     deviceWaitIdle rendererData
    //
    //     -- destroy swapchain & graphics resources
    //     unloadGraphicsDatas _resources rendererData
    //
    //     destroyRenderTargets rendererData _renderTargetDataMap
    //
    //     -- recreate swapchain & graphics resources
    //     recreateSwapChain rendererData window
    //
    //     renderTargets <- createRenderTargets rendererData _renderTargetDataMap
    //
    //     loadGraphicsDatas _resources rendererData
    //
    //
    // recreateSwapChain :: RendererData -> GLFW.Window -> IO ()
    // recreateSwapChain rendererData@RendererData {..} window = do
    //     logInfo "<< recreateSwapChain >>"
    //     destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    //     swapChainData <- get_swap_chain_data rendererData
    //     destroySwapChainData _device swapChainData
    //
    //     newSwapChainSupportDetails <- querySwapChainSupport _physicalDevice _vkSurface
    //     newSwapChainData <- createSwapChainData _device newSwapChainSupportDetails _queueFamilyDatas _vkSurface Constants.enableImmediateMode
    //     writeIORef _swapChainDataRef newSwapChainData
    //     writeIORef _swapChainSupportDetailsRef newSwapChainSupportDetails
    //
    //     createCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    //
    //
    // presentSwapChain :: RendererData -> Ptr VkCommandBuffer -> Ptr VkFence -> VkSemaphore -> VkSemaphore -> IO VkResult
    // presentSwapChain rendererData@RendererData {..} commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore = do
    //     let QueueFamilyDatas {..} = _queueFamilyDatas
    //     swapChainData@SwapChainData {..} <- get_swap_chain_data rendererData
    //
    //     let submitInfo = createVk @VkSubmitInfo
    //               $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
    //               &* set @"pNext" VK_NULL
    //               &* set @"waitSemaphoreCount" 1
    //               &* setListRef @"pWaitSemaphores" [imageAvailableSemaphore]
    //               &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
    //               &* set @"commandBufferCount" 1
    //               &* set @"pCommandBuffers" commandBufferPtr
    //               &* set @"signalSemaphoreCount" 1
    //               &* setListRef @"pSignalSemaphores" [renderFinishedSemaphore]
    //
    //     vkResetFences _device 1 frameFencePtr
    //
    //     frameFence <- peek frameFencePtr
    //
    //     let waitingForFence = False
    //
    //     withPtr submitInfo $ \submitInfoPtr ->
    //         vkQueueSubmit _graphicsQueue 1 submitInfoPtr (if waitingForFence then frameFence else VK_NULL) >>=
    //           flip validationVK "vkQueueSubmit failed!"
    //
    //     when waitingForFence $
    //         vkWaitForFences _device 1 frameFencePtr VK_TRUE (maxBound :: Word64) >>=
    //             flip validationVK "vkWaitForFences failed!"
    //
    //     let presentInfo = createVk @VkPresentInfoKHR
    //           $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
    //           &* set @"pNext" VK_NULL
    //           &* set @"pImageIndices" _swapChainIndexPtr
    //           &* set @"waitSemaphoreCount" 1
    //           &* setListRef @"pWaitSemaphores" [renderFinishedSemaphore]
    //           &* set @"swapchainCount" 1
    //           &* setListRef @"pSwapchains" [_swapChain]
    //
    //     result <- withPtr presentInfo $ \presentInfoPtr -> do
    //         vkQueuePresentKHR _presentQueue presentInfoPtr
    //
    //     -- waiting
    //     deviceWaitIdle rendererData
    //     return result
    //
    //
    // uploadUniformBufferData :: (Storable a) => RendererData -> Int -> UniformBufferType -> a -> IO ()
    // uploadUniformBufferData rendererData@RendererData {..} swapChainIndex uniformBufferType uploadData = do
    //     uniformBufferData <- getUniformBufferData rendererData uniformBufferType
    //     let uniformBufferMemory = atSwapChainIndex swapChainIndex (_uniformBufferMemories uniformBufferData)
    //     updateBufferData _device uniformBufferMemory uploadData
    //

    pub fn render_scene(&self) {
        // renderScene :: RendererData -> SceneManager.SceneManagerData -> Double -> Float -> IO ()
        // renderScene rendererData@RendererData {..} sceneManagerData elapsedTime deltaTime = do
        //     -- frame index
        //     frameIndex <- readIORef _frameIndexRef
        //     let frameFencePtr = ptrAtIndex _frameFencesPtr frameIndex
        //         imageAvailableSemaphore = atFrameIndex frameIndex _imageAvailableSemaphores
        //         renderFinishedSemaphore = atFrameIndex frameIndex _renderFinishedSemaphores
        //     swapChainData@SwapChainData {..} <- get_swap_chain_data rendererData
        //
        //     -- Begin Render
        //     acquireNextImageResult <- vkAcquireNextImageKHR _device _swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE _swapChainIndexPtr
        //     swapChainIndex <- get_swap_chain_index rendererData
        //     let commandBufferPtr = ptrAtIndex _commandBuffersPtr swapChainIndex
        //     commandBuffer <- peek commandBufferPtr
        //
        //     result <- case acquireNextImageResult of
        //         VK_SUCCESS -> do
        //             mainCamera <- SceneManager.getMainCamera sceneManagerData
        //             cameraPosition <- Camera.getCameraPosition mainCamera
        //             cameraPositionPrev <- Camera.getCameraPositionPrev mainCamera
        //             viewMatrix <- Camera.getViewMatrix mainCamera
        //             invViewMatrix <- Camera.getInvViewMatrix mainCamera
        //             viewOriginMatrix <- Camera.getViewOriginMatrix mainCamera
        //             invViewOriginMatrix <- Camera.getInvViewOriginMatrix mainCamera
        //             projectionMatrix <- Camera.getProjectionMatrix mainCamera
        //             invProjectionMatrix <- Camera.getInvProjectionMatrix mainCamera
        //             viewProjectionMatrix <- Camera.getViewProjectionMatrix mainCamera
        //             invViewProjectionMatrix <- Camera.getInvViewProjectionMatrix mainCamera
        //             viewOriginProjectionMatrix <- Camera.getViewOriginProjectionMatrix mainCamera
        //             invViewOriginProjectionMatrix <- Camera.getInvViewOriginProjectionMatrix mainCamera
        //             viewOriginProjectionMatrixPrev <- Camera.getViewOriginProjectionMatrixPrev mainCamera
        //
        //             mainLight <- SceneManager.getMainLight sceneManagerData
        //             mainLightConstants <- Light.getLightConstants mainLight
        //             quadGeometryData <- Mesh.getDefaultGeometryData =<< getMeshData _resources "quad"
        //
        //             rotation <- TransformObject.getRotation $ Light._directionalLightTransformObject mainLight
        //
        //             -- Upload Uniform Buffers
        //             let screenWidth = fromIntegral $ getField @"width" _swapChainExtent :: Float
        //                 screenHeight = fromIntegral $ getField @"height" _swapChainExtent :: Float
        //                 sceneConstants = SceneConstants
        //                     { _SCREEN_SIZE = vec2 screenWidth screenHeight
        //                     , _BACKBUFFER_SIZE = vec2 screenWidth screenHeight
        //                     , _TIME = realToFrac elapsedTime
        //                     , _DELTA_TIME = scalar deltaTime
        //                     , _JITTER_FRAME = float_zero
        //                     , _SceneConstantsDummy0 = 0
        //                     }
        //                 viewConstants = ViewConstants
        //                     { _VIEW  = viewMatrix
        //                     , _INV_VIEW = invViewMatrix
        //                     , _VIEW_ORIGIN = viewOriginffMatrix
        //                     , _INV_VIEW_ORIGIN = invViewOriginMatrix
        //                     , _PROJECTION = projectionMatrix
        //                     , _INV_PROJECTION = invProjectionMatrix
        //                     , _VIEW_PROJECTION = viewProjectionMatrix
        //                     , _INV_VIEW_PROJECTION = invViewProjectionMatrix
        //                     , _VIEW_ORIGIN_PROJECTION = viewOriginProjectionMatrix
        //                     , _INV_VIEW_ORIGIN_PROJECTION = invViewOriginProjectionMatrix
        //                     , _VIEW_ORIGIN_PROJECTION_PREV = viewOriginProjectionMatrixPrev
        //                     , _CAMERA_POSITION = cameraPosition
        //                     , _VIEWCONSTANTS_DUMMY0 = 0.0
        //                     , _CAMERA_POSITION_PREV = cameraPositionPrev
        //                     , _VIEWCONSTANTS_DUMMY1 = 0.0
        //                     , _NEAR_FAR = vec2 Constants.near Constants.far
        //                     , _JITTER_DELTA = vec2 0.0 0.0
        //                     , _JITTER_OFFSET = vec2 0.0 0.0
        //                     , _VIEWCONSTANTS_DUMMY2 = 0.0
        //                     , _VIEWCONSTANTS_DUMMY3 = 0.0
        //                     }
        //             uploadUniformBufferData rendererData swapChainIndex UniformBuffer_SceneConstants sceneConstants
        //             uploadUniformBufferData rendererData swapChainIndex UniformBuffer_ViewConstants viewConstants
        //             uploadUniformBufferData rendererData swapChainIndex UniformBuffer_LightConstants mainLightConstants
        //             uploadUniformBufferData rendererData swapChainIndex UniformBuffer_SSAOConstants (PostProcess._ssao_kernel_samples _postprocess_ssao)
        //
        //             -- Begin command buffer
        //             let commandBufferBeginInfo = createVk @VkCommandBufferBeginInfo
        //                     $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        //                     &* set @"pNext" VK_NULL
        //                     &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
        //             withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
        //                 result <- vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
        //                 validationVK result "vkBeginCommandBuffer failed!"
        //
        //             -- Render
        //             staticRenderElements <- SceneManager.getStaticObjectRenderElements sceneManagerData
        //             skeletalRenderElements <- SceneManager.getSkeletalObjectRenderElements sceneManagerData
        //             renderShadow rendererData commandBuffer swapChainIndex Constants.RenderObject_Static staticRenderElements
        //             renderShadow rendererData commandBuffer swapChainIndex Constants.RenderObject_Skeletal skeletalRenderElements
        //             renderSolid rendererData commandBuffer swapChainIndex Constants.RenderObject_Static staticRenderElements
        //             renderSolid rendererData commandBuffer swapChainIndex Constants.RenderObject_Skeletal skeletalRenderElements
        //             renderPostProcess rendererData commandBuffer swapChainIndex quadGeometryData
        //
        //             -- Render Final
        //             renderPipeline1 rendererData commandBuffer swapChainIndex "render_final" quadGeometryData
        //
        //             -- Render Debug
        //             --writeIORef _debugRenderTargetRef RenderTarget_Shadow
        //             debugRenderTarget <- readIORef _debugRenderTargetRef
        //             when (RenderTarget_BackBuffer /= debugRenderTarget) $ do
        //                 renderDebugMaterialInstance <- getMaterialInstanceData _resources "render_debug"
        //                 let renderDebugPipelineBindingData = MaterialInstance.getPipelineBindingData renderDebugMaterialInstance ("render_debug", "render_debug")
        //                     renderDebugRenderPassData = MaterialInstance._renderPassData renderDebugPipelineBindingData
        //                     renderDebugPipelineData = MaterialInstance._pipelineData renderDebugPipelineBindingData
        //                 beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderDebugRenderPassData renderDebugPipelineData
        //
        //                 imageInfo <- getRenderTarget rendererData debugRenderTarget
        //                 updateDescriptorSet rendererData swapChainIndex renderDebugPipelineBindingData 0 (Descriptor.DescriptorImageInfo $ Texture._descriptorImageInfo imageInfo)
        //
        //                 bindDescriptorSets rendererData commandBuffer swapChainIndex renderDebugPipelineBindingData
        //                 drawElements rendererData commandBuffer quadGeometryData
        //                 endRenderPass rendererData commandBuffer
        //
        //             -- End command buffer
        //             vkEndCommandBuffer commandBuffer >>= flip validationVK "vkEndCommandBuffer failed!"
        //
        //             -- End Render
        //             presentResult <- presentSwapChain rendererData commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore
        //             return presentResult
        //         otherwise -> return acquireNextImageResult
        //
        //     let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result)
        //     writeIORef _needRecreateSwapChainRef needRecreateSwapChain
        //     writeIORef _frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
    }
    //
    // renderSolid :: RendererData
    //             -> VkCommandBuffer
    //             -> Int
    //             -> Constants.RenderObjectType
    //             -> [RenderElement.RenderElementData]
    //             -> IO ()
    // renderSolid rendererData commandBuffer swapChainIndex renderObjectType renderElements = do
    //     let renderPassPipelineDataName = case renderObjectType of
    //             Constants.RenderObject_Static -> ("render_pass_static_opaque", "render_object")
    //             Constants.RenderObject_Skeletal -> ("render_pass_skeletal_opaque", "render_object")
    //     forM_ (zip [(0::Int)..] renderElements) $ \(index, renderElement) -> do
    //         let renderObject = RenderElement._renderObject renderElement
    //             geometryBufferData = RenderElement._geometryData renderElement
    //             materialInstanceData = RenderElement._materialInstanceData renderElement
    //             pipelineBindingData = MaterialInstance.getPipelineBindingData materialInstanceData renderPassPipelineDataName
    //             renderPassData = MaterialInstance._renderPassData pipelineBindingData
    //             pipelineData = MaterialInstance._pipelineData pipelineBindingData
    //             pipelineLayout = RenderPass._pipelineLayout pipelineData
    //
    //         when (0 == index) $ do
    //             beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
    //
    //         bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData
    //
    //         modelMatrix <- TransformObject.getMatrix (RenderObject._transformObject renderObject)
    //         let pushConstantData = PushConstantData { modelMatrix = modelMatrix }
    //         uploadPushConstantData rendererData commandBuffer pipelineData pushConstantData
    //
    //         drawElements rendererData commandBuffer geometryBufferData
    //     endRenderPass rendererData commandBuffer
    //
    //
    // renderShadow :: RendererData
    //              -> VkCommandBuffer
    //              -> Int
    //              -> Constants.RenderObjectType
    //              -> [RenderElement.RenderElementData]
    //              -> IO ()
    // renderShadow rendererData commandBuffer swapChainIndex renderObjectType renderElements = do
    //     let (renderPassPipelineDataName, materialInstanceName) = case renderObjectType of
    //             Constants.RenderObject_Static -> (("render_pass_static_shadow", "render_object"), "render_static_shadow")
    //             Constants.RenderObject_Skeletal -> (("render_pass_skeletal_shadow", "render_object"), "render_skeletal_shadow")
    //     materialInstance <- getMaterialInstanceData (_resources rendererData) materialInstanceName
    //     let pipelineBindingData = MaterialInstance.getPipelineBindingData materialInstance renderPassPipelineDataName
    //         renderPassData = MaterialInstance._renderPassData pipelineBindingData
    //         pipelineData = MaterialInstance._pipelineData pipelineBindingData
    //     beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
    //     bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData
    //
    //     forM_ (zip [(0::Int)..] renderElements) $ \(index, renderElement) -> do
    //         let renderObject = RenderElement._renderObject renderElement
    //             geometryBufferData = RenderElement._geometryData renderElement
    //
    //         modelMatrix <- TransformObject.getMatrix (RenderObject._transformObject renderObject)
    //         let pushConstantData = PushConstantData { modelMatrix = modelMatrix }
    //         uploadPushConstantData rendererData commandBuffer pipelineData pushConstantData
    //
    //         drawElements rendererData commandBuffer geometryBufferData
    //     endRenderPass rendererData commandBuffer
    //
    //
    // renderPostProcess :: RendererData
    //                   -> VkCommandBuffer
    //                   -> Int
    //                   -> GeometryData
    //                   -> IO ()
    // renderPostProcess rendererData@RendererData {..} commandBuffer swapChainIndex quadGeometryData = do
    //     renderPipeline1 rendererData commandBuffer swapChainIndex "render_ssao" quadGeometryData
    //     renderPipeline1 rendererData commandBuffer swapChainIndex "composite_gbuffer" quadGeometryData
    //     renderPipeline1 rendererData commandBuffer swapChainIndex "render_motion_blur" quadGeometryData
}