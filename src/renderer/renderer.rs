use std::sync::Arc;
use std::vec::Vec;
use std::borrow::Cow;

use vulkano;
use vulkano::device::{
    Device,
    DeviceExtensions,
    Queue
};
use vulkano::image::ImageUsage;
use vulkano::instance::{
    Instance,
    PhysicalDevice
};
use vulkano::swapchain;
use vulkano::swapchain::{
    AcquireError,
    ColorSpace,
    FullscreenExclusive,
    PresentMode,
    SurfaceTransform,
    Swapchain,
    SwapchainCreationError,
};
use vulkano::sync;
use vulkano::sync::{
    FlushError,
    GpuFuture
};
use vulkano_win::VkSurfaceBuild;
use winit::event_loop::EventLoop;
use winit::window::Window;
use winit::window::WindowBuilder;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector3;

use crate::frame;
use crate::constants;
use crate::resource;

#[derive(Clone)]
pub struct RendererData {
    pub _frame_index: i32,
    pub _swapchain_index: u32,
    // _vertex_offset: vk::DeviceSize,
    _need_recreate_swapchain: bool,
    // _image_available_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
    // _render_finished_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
    // _vk_instance: vk::Instance,
    pub _surface: Arc<vulkano::swapchain::Surface<Window>>,
    pub _device: Arc<Device>,
    // _physical_device: vk::PhysicalDevice,
    pub _swapchain: Arc<vulkano::swapchain::Swapchain<Window>>,
    pub _images: Vec<Arc<vulkano::image::swapchain::SwapchainImage<Window>>>,
    // _swapchain_data: SwapChainData,
    // _swapchain_support_details: SwapChainSupportDetails,
    pub _queue: Arc<Queue>,
    // _queue_family_datas: QueueFamilyDatas,
    // _frame_fences: vk::Fence,
    // _command_pool: vk::CommandPool,
    pub _command_buffer_count: i32,
    // _command_buffers: vk::CommandBuffer,
    // _render_features: vk::RenderFeatures,
    // _image_samplers: vk::ImageSamplers,
    // _debug_render_target: RenderTargetType,
    // _render_target_data_map: RenderTargetDataMap,
    // _uniform_buffer_data_map: UniformBufferDataMap,
    // _postprocess_ssao: PostProcessData,
    // _resources: Box<resource::Resources>
}

pub fn create_renderer_data<T> (event_loop: &EventLoop<T>) -> Box<RendererData> {
    let required_extensions = vulkano_win::required_extensions();
    let name: Option<Cow<str>> = Some("RustEngine3D".into());
    let version = Some(vulkano::instance::Version {major: 0, minor: 1, patch: 0});
    let info = vulkano::instance::ApplicationInfo {
        application_name: name.clone(),
        application_version: version,
        engine_name: name.clone(),
        engine_version: version
    };
    let instance = Instance::new(Some(&info), &required_extensions, constants::VULKAN_LAYERS.to_vec()).unwrap();
    let physical = PhysicalDevice::enumerate(&instance).next().unwrap();
    let surface = WindowBuilder::new()
        .build_vk_surface(&event_loop, instance.clone())
        .unwrap();
    let queue_family = physical
        .queue_families()
        .find(|&q| q.supports_graphics() && surface.is_supported(q).unwrap_or(false))
        .expect("couldn't find a graphical queue family");
    let device_ext = DeviceExtensions {
        khr_swapchain: true,
        ext_debug_utils: constants::EXT_DEBUG_UTILS,
        ..DeviceExtensions::none()
    };
    let (device, mut queues) = Device::new(
        physical,
        physical.supported_features(),
        &device_ext,
        [(queue_family, 0.5)].iter().cloned(),
    ).unwrap();
    let queue = queues.next().unwrap();
    let (swapchain, images) = {
        let caps = surface.capabilities(physical).unwrap();
        let alpha = caps.supported_composite_alpha.iter().next().unwrap();
        let format = caps.supported_formats[0].0;
        let dimensions: [u32; 2] = surface.window().inner_size().into();
        Swapchain::new(
            device.clone(),
            surface.clone(),
            caps.min_image_count,
            format,
            dimensions,
            1,
            ImageUsage::color_attachment(),
            &queue,
            SurfaceTransform::Identity,
            alpha,
            PresentMode::Fifo,
            FullscreenExclusive::Default,
            true,
            ColorSpace::SrgbNonLinear,
        ).unwrap()
    };

    Box::new(RendererData {
        _frame_index: 0,
        _swapchain_index: 0,
        // _vertex_offset: vk::DeviceSize,
        _need_recreate_swapchain: false,
        // _image_available_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
        // _render_finished_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
        // _vk_instance: vk::Instance,
        _surface: surface,
        _device: device,
        // _physical_device: vk::PhysicalDevice,
        _images: images,
        _swapchain: swapchain,
        // _swapchain_data: SwapChainData,
        // _swapchain_support_details: SwapChainSupportDetails,
        _queue: queue,
        // _queue_family_datas: QueueFamilyDatas,
        // _frame_fences: vk::Fence,
        // _command_pool: vk::CommandPool,
        _command_buffer_count: 0,
        // _command_buffers: vk::CommandBuffer,
        // _render_features: vk::RenderFeatures,
        // _image_samplers: vk::ImageSamplers,
        // _debug_render_target: RenderTargetType,
        // _render_target_data_map: RenderTargetDataMap,
        // _uniform_buffer_data_map: UniformBufferDataMap,
        // _postprocess_ssao: PostProcessData,
        // _resources: Box<resource::Resources>
    })
}

impl RendererData {
    pub fn resize_window(&self) {
        // resizeWindow :: GLFW.Window -> RendererData -> IO ()
        // resizeWindow window rendererData@RendererData {..} = do
        // logInfo "<< resizeWindow >>"
        //
        // deviceWaitIdle rendererData
        //
        // // destroy swapchain & graphics resources
        // unloadGraphicsDatas _resources rendererData
        //
        // destroyRenderTargets rendererData _renderTargetDataMap
        //
        // // recreate swapchain & graphics resources
        // recreateSwapChain rendererData window
        // renderTargets <- createRenderTargets rendererData _renderTargetDataMap
        // loadGraphicsDatas _resources rendererData
    }

    pub fn recreate_swapchain(&mut self) {
        let dimensions: [u32; 2] = self._surface.window().inner_size().into();
        let (new_swapchain, new_images) =
            match self._swapchain.recreate_with_dimensions(dimensions) {
                Ok(r) => r,
                Err(SwapchainCreationError::UnsupportedDimensions) => return,
                Err(e) => panic!("Failed to recreate swapchain: {:?}", e),
            };

        self._swapchain = new_swapchain;
        self._images = new_images;
    }

    pub fn get_need_recreate_swapchain(&self) -> bool {
        self._need_recreate_swapchain
    }

    pub fn set_need_recreate_swapchain(&mut self, value: bool) {
        self._need_recreate_swapchain = value;
    }

    pub fn render_scene(&mut self) {
        let mut frame_system = frame::FrameSystem::new(self._queue.clone(), self._swapchain.format());
        let triangle_draw_system = frame::TriangleDrawSystem::new(self._queue.clone(), frame_system.deferred_subpass());
        let mut previous_frame_end = Some(sync::now(self._device.clone()).boxed());
        let mut swapchain = self._swapchain.clone();
        let mut images = self._images.clone();

        previous_frame_end.as_mut().unwrap().cleanup_finished();

        let (image_num, suboptimal, acquire_future) =
            match swapchain::acquire_next_image(swapchain.clone(), None) {
                Ok(r) => r,
                Err(AcquireError::OutOfDate) => {
                    self.set_need_recreate_swapchain(true);
                    return;
                }
                Err(e) => panic!("Failed to acquire next image: {:?}", e),
            };

        if suboptimal {
            self.set_need_recreate_swapchain(true);
        }

        let future = previous_frame_end.take().unwrap().join(acquire_future);
        let mut frame = frame_system.frame(future, images[image_num].clone(), Matrix4::identity());
        let mut after_future = None;
        while let Some(pass) = frame.next_pass() {
            match pass {
                frame::Pass::Deferred(mut draw_pass) => {
                    let cb = triangle_draw_system.draw(draw_pass.viewport_dimensions());
                    draw_pass.execute(cb);
                }
                frame::Pass::Lighting(mut lighting) => {
                    lighting.ambient_light([0.1, 0.1, 0.1]);
                    lighting.directional_light(Vector3::new(0.2, -0.1, -0.7), [0.6, 0.6, 0.6]);
                    lighting.point_light(Vector3::new(0.5, -0.5, -0.1), [1.0, 0.0, 0.0]);
                    lighting.point_light(Vector3::new(-0.9, 0.2, -0.15), [0.0, 1.0, 0.0]);
                    lighting.point_light(Vector3::new(0.0, 0.5, -0.05), [0.0, 0.0, 1.0]);
                }
                frame::Pass::Finished(af) => {
                    after_future = Some(af);
                }
            }
        }

        let future = after_future
            .unwrap()
            .then_swapchain_present(self._queue.clone(), swapchain.clone(), image_num)
            .then_signal_fence_and_flush();

        match future {
            Ok(future) => {
                previous_frame_end = Some(future.boxed());
            }
            Err(FlushError::OutOfDate) => {
                self.set_need_recreate_swapchain(true);
                previous_frame_end = Some(sync::now(self._device.clone()).boxed());
            }
            Err(e) => {
                println!("Failed to flush future: {:?}", e);
                previous_frame_end = Some(sync::now(self._device.clone()).boxed());
            }
        }
    }
}




//
//
// instance RendererInterface RendererData where
//     getPhysicalDevice rendererData = (_physicalDevice rendererData)
//     getDevice rendererData = (_device rendererData)
//     getSwapChainData rendererData = read$ _swapChainDataRef rendererData
//     getSwapChainImageViews rendererData = _swapChainImageViews <$> getSwapChainData rendererData
//     getSwapChainSupportDetails rendererData = read$ _swapChainSupportDetailsRef rendererData
//     getSwapChainIndex renderData = fromIntegral <$> peek (_swapChainIndexPtr renderData)
//     getCommandPool rendererData = (_commandPool rendererData)
//     getCommandBuffers rendererData = peekArray (_commandBufferCount rendererData) (_commandBuffersPtr rendererData)
//     getCommandBuffer rendererData index = peekElemOff (_commandBuffersPtr rendererData) index
//     getCurrentCommandBuffer rendererData = getCommandBuffer rendererData =<< getSwapChainIndex rendererData
//     getGraphicsQueue rendererData = (_graphicsQueue (_queueFamilyDatas rendererData))
//     getPresentQueue rendererData = (_presentQueue (_queueFamilyDatas rendererData))
//
//     getUniformBufferData :: RendererData -> UniformBufferType -> IO UniformBufferData
//     getUniformBufferData rendererData uniformBufferType =
//         Maybe.fromJust <$> HashTable.lookup (_uniformBufferDataMap rendererData) uniformBufferType
//
//     nextDebugRenderTarget rendererData = do
//         debugRenderTarget <- read(_debugRenderTargetRef rendererData)
//         let nextValue = if debugRenderTarget == (maxBound::RenderTargetType) then (minBound::RenderTargetType) else succ debugRenderTarget
//         logInfo $ "Current DebugRenderTarget : " ++ show nextValue
//         write(_debugRenderTargetRef rendererData) nextValue
//
//     prevDebugRenderTarget rendererData = do
//         debugRenderTarget <- read(_debugRenderTargetRef rendererData)
//         let prevValue = if debugRenderTarget == (minBound::RenderTargetType) then (maxBound::RenderTargetType) else pred debugRenderTarget
//         logInfo $ "Current DebugRenderTarget : " ++ show prevValue
//         write(_debugRenderTargetRef rendererData) prevValue
//
//     createRenderTarget rendererData textureDataName textureCreateInfo =
//         Texture.createRenderTarget
//             textureDataName
//             (_physicalDevice rendererData)
//             (_device rendererData)
//             (_commandPool rendererData)
//             (_graphicsQueue $ _queueFamilyDatas rendererData)
//             textureCreateInfo
//
//     createTexture rendererData textureDataName textureCreateInfo =
//         Texture.createTextureData
//             textureDataName
//             (_physicalDevice rendererData)
//             (_device rendererData)
//             (_commandPool rendererData)
//             (_graphicsQueue $ _queueFamilyDatas rendererData)
//             textureCreateInfo
//
//     destroyTexture rendererData textureData =
//         Texture.destroyTextureData (_device rendererData) textureData
//
//     getRenderTarget rendererData renderTargetType =
//         Maybe.fromJust <$> HashTable.lookup (_renderTargetDataMap rendererData) renderTargetType
//
//     createGeometryBuffer rendererData bufferName geometryCreateInfo = do
//         createGeometryData
//             (getPhysicalDevice rendererData)
//             (getDevice rendererData)
//             (getGraphicsQueue rendererData)
//             (getCommandPool rendererData)
//             bufferName
//             geometryCreateInfo
//
//     destroyGeometryBuffer rendererData geometryBuffer =
//         destroyGeometryData (_device rendererData) geometryBuffer
//
//     renderPipeline :: RendererData -> VkCommandBuffer -> Int -> Text.Text -> Text.Text -> Text.Text -> GeometryData -> IO ()
//     renderPipeline rendererData commandBuffer swapChainIndex renderPassName pipelineName materialInstanceName geometryData = do
//         materialInstanceData <- getMaterialInstanceData (_resources rendererData) materialInstanceName
//         let pipelineBindingData = MaterialInstance.getPipelineBindingData materialInstanceData (renderPassName, pipelineName)
//             renderPassData = MaterialInstance._renderPassData pipelineBindingData
//             pipelineData = MaterialInstance._pipelineData pipelineBindingData
//         beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
//         bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData
//         drawElements rendererData commandBuffer geometryData
//         endRenderPass rendererData commandBuffer
//
//     renderPipeline1 :: RendererData -> VkCommandBuffer -> Int -> Text.Text -> GeometryData -> IO ()
//     renderPipeline1 rendererData commandBuffer swapChainIndex renderPassName geometryData =
//         renderPipeline rendererData commandBuffer swapChainIndex renderPassName renderPassName renderPassName geometryData
//
//     beginRenderPassPipeline :: RendererData -> VkCommandBuffer -> Int -> RenderPass.RenderPassData -> RenderPass.PipelineData -> IO ()
//     beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData = do
//         Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass.getRenderPassFrameBufferName renderPassData)
//         let renderPassBeginInfo = atSwapChainIndex swapChainIndex (_renderPassBeginInfos frameBufferData)
//             pipelineDynamicStates = RenderPass._pipelineDynamicStates pipelineData
//         withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
//             vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
//         when (elem VK_DYNAMIC_STATE_VIEWPORT pipelineDynamicStates) $
//             withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
//                 vkCmdSetViewport commandBuffer 0 1 viewPortPtr
//         when (elem VK_DYNAMIC_STATE_SCISSOR pipelineDynamicStates) $
//             withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
//                 vkCmdSetScissor commandBuffer 0 1 scissorRectPtr
//         vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS (RenderPass._pipeline pipelineData)
//
//     beginRenderPassPipeline' :: RendererData -> VkCommandBuffer -> Int -> RenderPass.RenderPassPipelineDataName -> IO (RenderPass.RenderPassData, RenderPass.PipelineData)
//     beginRenderPassPipeline' rendererData commandBuffer swapChainIndex renderPassPipelineDataName = do
//         (renderPassData, pipelineData) <- getRenderPassPipelineData (_resources rendererData) renderPassPipelineDataName
//         beginRenderPassPipeline rendererData commandBuffer swapChainIndex renderPassData pipelineData
//         return (renderPassData, pipelineData)
//
//     bindDescriptorSets :: RendererData -> VkCommandBuffer -> Int -> MaterialInstance.PipelineBindingData -> IO ()
//     bindDescriptorSets rendererData commandBuffer swapChainIndex pipelineBindingData = do
//         let pipelineLayout = RenderPass._pipelineLayout . MaterialInstance._pipelineData $ pipelineBindingData
//             descriptorSetsPtr = MaterialInstance._descriptorSetsPtr pipelineBindingData
//         vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 (ptrAtIndex descriptorSetsPtr swapChainIndex) 0 VK_NULL
//
//     updateDescriptorSet :: RendererData -> Int -> MaterialInstance.PipelineBindingData -> Int -> Descriptor.DescriptorResourceInfo -> IO ()
//     updateDescriptorSet rendererData swapChainIndex pipelineBindingData descriptorOffset descriptorResourceInfo = do
//         let writeDescriptorSetPtr = atSwapChainIndex swapChainIndex (MaterialInstance._writeDescriptorSetPtrs pipelineBindingData)
//             writeDescriptorSetPtrOffset = ptrAtIndex writeDescriptorSetPtr descriptorOffset
//         Descriptor.updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo
//         vkUpdateDescriptorSets (_device rendererData) 1 writeDescriptorSetPtrOffset 0 VK_NULL
//
//     updateDescriptorSets :: RendererData -> Int -> MaterialInstance.PipelineBindingData -> [(Int, Descriptor.DescriptorResourceInfo)] -> IO ()
//     updateDescriptorSets rendererData swapChainIndex pipelineBindingData descriptorInfos = do
//         let writeDescriptorSetPtr = atSwapChainIndex swapChainIndex (MaterialInstance._writeDescriptorSetPtrs pipelineBindingData)
//             descriptorWriteCount = MaterialInstance._descriptorSetCount pipelineBindingData
//         forM_ descriptorInfos $ \(descriptorOffset, descriptorResourceInfo) -> do
//             Descriptor.updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo
//         vkUpdateDescriptorSets (_device rendererData) (fromIntegral descriptorWriteCount) writeDescriptorSetPtr 0 VK_NULL
//
//     uploadPushConstantData :: RendererData -> VkCommandBuffer -> RenderPass.PipelineData -> PushConstantData -> IO ()
//     uploadPushConstantData rendererData commandBuffer pipelineData pushConstantData =
//         with pushConstantData $ \pushConstantDataPtr ->
//             vkCmdPushConstants commandBuffer (RenderPass._pipelineLayout pipelineData) VK_SHADER_STAGE_ALL 0 (bSizeOf pushConstantData) (castPtr pushConstantDataPtr)
//
//     drawElements rendererData commandBuffer geometryData = do
//         vkCmdBindVertexBuffers commandBuffer 0 1 (_vertexBufferPtr geometryData) (_vertexOffsetPtr rendererData)
//         vkCmdBindIndexBuffer commandBuffer (_indexBuffer geometryData) 0 VK_INDEX_TYPE_UINT32
//         vkCmdDrawIndexed commandBuffer (_vertexIndexCount geometryData) 1 0 0 0
//
//     endRenderPass _ commandBuffer = vkCmdEndRenderPass commandBuffer
//
//     deviceWaitIdle rendererData =
//         throwingVK "vkDeviceWaitIdle failed!" (vkDeviceWaitIdle $ getDevice rendererData)
//
// defaultRendererData :: Resources -> IO RendererData
// defaultRendererData resources = do
//     imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
//         writeField @"width" extentPtr $ 0
//         writeField @"height" extentPtr $ 0
//     surfaceCapabilities <- newVkData @VkSurfaceCapabilitiesKHR $ \surfaceCapabilitiesPtr -> do
//         return ()
//     let defaultSwapChainData = SwapChainData
//             { _swapChain = VK_NULL
//             , _swapChainImages = SwapChainIndexMapEmpty
//             , _swapChainImageFormat = VK_FORMAT_UNDEFINED
//             , _swapChainImageViews = SwapChainIndexMapEmpty
//             , _swapChainExtent = imageExtent }
//         defaultSwapChainSupportDetails = SwapChainSupportDetails
//             { _capabilities = surfaceCapabilities
//             , _formats = []
//             , _presentModes = [] }
//         defaultQueueFamilyIndices = QueueFamilyIndices
//             { _graphicsQueueIndex = 0
//             , _presentQueueIndex = 0
//             , _computeQueueIndex = 0
//             , _transferQueueIndex = 0
//             , _sparseBindingQueueIndex = 0 }
//         defaultQueueFamilyDatas = QueueFamilyDatas
//             { _graphicsQueue = VK_NULL
//             , _presentQueue = VK_NULL
//             , _queueFamilyIndexList = []
//             , _queueFamilyCount = 0
//             , _queueFamilyIndices = defaultQueueFamilyIndices }
//         defaultRenderFeatures = RenderFeatures
//             { _anisotropyEnable = VK_FALSE
//             , _msaaSamples = VK_SAMPLE_COUNT_1_BIT }
//     postprocess_ssao <- PostProcess.initializePostProcessData_SSAO
//     swapChainIndexPtr <- new (0 :: Word32)
//     vertexOffsetPtr <- new (0 :: VkDeviceSize)
//     frameIndexRef <- new(0::Int)
//     needRecreateSwapChainRef <- newFalse
//     imageSamplers <- newdefaultImageSamplers
//     renderPassDataListRef <- new(DList.fromList [])
//     swapChainDataRef <- newdefaultSwapChainData
//     swapChainSupportDetailsRef <- newdefaultSwapChainSupportDetails
//     debugRenderTargetRef <- newRenderTarget_BackBuffer
//     renderTargetDataMap <- HashTable.new
//     uniformBufferDataMap <- HashTable.new
//
//     return RendererData
//         { _frameIndexRef = frameIndexRef
//         , _swapChainIndexPtr = swapChainIndexPtr
//         , _vertexOffsetPtr = vertexOffsetPtr
//         , _needRecreateSwapChainRef = needRecreateSwapChainRef
//         , _imageAvailableSemaphores = FrameIndexMapEmpty
//         , _renderFinishedSemaphores = FrameIndexMapEmpty
//         , _vkInstance = VK_NULL
//         , _vkSurface = VK_NULL
//         , _device = VK_NULL
//         , _physicalDevice = VK_NULL
//         , _swapChainDataRef = swapChainDataRef
//         , _swapChainSupportDetailsRef = swapChainSupportDetailsRef
//         , _queueFamilyDatas = defaultQueueFamilyDatas
//         , _frameFencesPtr = VK_NULL
//         , _commandPool = VK_NULL
//         , _commandBufferCount = 0
//         , _commandBuffersPtr = VK_NULL
//         , _renderFeatures = defaultRenderFeatures
//         , _imageSamplers = imageSamplers
//         , _debugRenderTargetRef = debugRenderTargetRef
//         , _renderTargetDataMap = renderTargetDataMap
//         , _uniformBufferDataMap = uniformBufferDataMap
//         , _postprocess_ssao = postprocess_ssao
//         , _resources = resources
//         }
//
// initializeRenderer :: RendererData -> IO RendererData
// initializeRenderer rendererData@RendererData {..} = do
//     poke _swapChainIndexPtr 0
//     write_frameIndexRef (0::Int)
//     write_needRecreateSwapChainRef False
//
//     registUniformBufferDatas _physicalDevice _device _uniformBufferDataMap
//
//     imageSamplers <- createImageSamplers _device
//     write_imageSamplers imageSamplers
//
//     renderTargets <- createRenderTargets rendererData _renderTargetDataMap
//
//     return rendererData
//
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
//     device <- createDevice physicalDevice queueFamilyIndexList
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
//     swapChainDataRef <- newswapChainData
//     swapChainSupportDetailsRef <- newswapChainSupportDetails
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
// destroyRenderer :: RendererData -> IO ()
// destroyRenderer rendererData@RendererData {..} = do
//     destroyUniformBufferDatas _device _uniformBufferDataMap
//
//     imageSamplers <- read_imageSamplers
//     destroyImageSamplers _device imageSamplers
//
//     destroyRenderTargets rendererData _renderTargetDataMap
//
//     destroySemaphores _device _renderFinishedSemaphores
//     destroySemaphores _device _imageAvailableSemaphores
//     destroyFrameFences _device _frameFencesPtr
//     destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
//     destroyCommandPool _device _commandPool
//     swapChainData <- (getSwapChainData rendererData)
//     destroySwapChainData _device swapChainData
//     destroyDevice _device
//     destroyVkSurface _vkInstance _vkSurface
//     destroyVulkanInstance _vkInstance
//     free _commandBuffersPtr
//     free _swapChainIndexPtr
//     free _vertexOffsetPtr
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
//     swapChainData <- getSwapChainData rendererData
//     destroySwapChainData _device swapChainData
//
//     newSwapChainSupportDetails <- querySwapChainSupport _physicalDevice _vkSurface
//     newSwapChainData <- createSwapChainData _device newSwapChainSupportDetails _queueFamilyDatas _vkSurface Constants.enableImmediateMode
//     write_swapChainDataRef newSwapChainData
//     write_swapChainSupportDetailsRef newSwapChainSupportDetails
//
//     createCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
//
//
// presentSwapChain :: RendererData -> Ptr VkCommandBuffer -> Ptr VkFence -> VkSemaphore -> VkSemaphore -> IO VkResult
// presentSwapChain rendererData@RendererData {..} commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore = do
//     let QueueFamilyDatas {..} = _queueFamilyDatas
//     swapChainData@SwapChainData {..} <- getSwapChainData rendererData
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
//
// renderScene :: RendererData -> SceneManager.SceneManagerData -> Double -> Float -> IO ()
// renderScene rendererData@RendererData {..} sceneManagerData elapsedTime deltaTime = do
//     -- frame index
//     frameIndex <- read_frameIndexRef
//     let frameFencePtr = ptrAtIndex _frameFencesPtr frameIndex
//         imageAvailableSemaphore = atFrameIndex frameIndex _imageAvailableSemaphores
//         renderFinishedSemaphore = atFrameIndex frameIndex _renderFinishedSemaphores
//     swapChainData@SwapChainData {..} <- getSwapChainData rendererData
//
//     -- Begin Render
//     acquireNextImageResult <- vkAcquireNextImageKHR _device _swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE _swapChainIndexPtr
//     swapChainIndex <- getSwapChainIndex rendererData
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
//                     , _VIEW_ORIGIN = viewOriginMatrix
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
//             --write_debugRenderTargetRef RenderTarget_Shadow
//             debugRenderTarget <- read_debugRenderTargetRef
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
//     write_needRecreateSwapChainRef needRecreateSwapChain
//     write_frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
//
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