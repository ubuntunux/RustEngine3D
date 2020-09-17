
//
//
// class ApplicationInterface a where
// getDeltaTime :: a -> IO Double
// getElapsedTime :: a -> IO Double
//
// instance ApplicationInterface ApplicationData where
// getDeltaTime applicationData = do
// timeData <- readIORef (_timeData applicationData)
// return $ _deltaTime timeData
//
// getElapsedTime applicationData = do
// timeData <- readIORef (_timeData applicationData)
// return $ _elapsedTime timeData
//
// -- TODO: Use Queue or Stack
// mouseButtonCallback :: IORef MouseInputData -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
// mouseButtonCallback mouseInputDataRef window mouseButton mouseButtonState modifierKeys = do
// mouseInputData <- readIORef mouseInputDataRef
// let (down, up) = if GLFW.MouseButtonState'Pressed == mouseButtonState
// then (True, False)
// else (False, True)
// writeIORef mouseInputDataRef $ getMouseInputData mouseInputData mouseButton (down, up)
// where
// getMouseInputData :: MouseInputData -> GLFW.MouseButton -> (Bool, Bool) -> MouseInputData
// getMouseInputData mouseInputData GLFW.MouseButton'1 (down, up) = mouseInputData { _btn_l_down = down, _btn_l_up = up }
// getMouseInputData mouseInputData GLFW.MouseButton'2 (down, up) = mouseInputData { _btn_r_down = down, _btn_r_up = up }
// getMouseInputData mouseInputData GLFW.MouseButton'3 (down, up) = mouseInputData { _btn_m_down = down, _btn_m_up = up }
// getMouseInputData mouseInputData _ (down, up) = mouseInputData
//
// -- TODO: Use Queue or Stack
// scrollCallback :: IORef MouseMoveData -> GLFW.Window -> Double -> Double -> IO ()
// scrollCallback mouseMoveDataRef window xoffset yoffset = do
// mouseMoveData <- readIORef mouseMoveDataRef
// writeIORef mouseMoveDataRef $ mouseMoveData
// { _scroll_xoffset = realToFrac xoffset
// , _scroll_yoffset = realToFrac yoffset
// }
//
// -- TODO: Use Queue or Stack
// cursorPosCallback :: IORef MouseMoveData -> GLFW.Window -> Double -> Double -> IO ()
// cursorPosCallback mouseMoveDataRef windows posX posY = do
// mouseMoveData <- readIORef mouseMoveDataRef
// let newPos = vec2 (round posX) (round posY)
// posDelta = newPos - _mousePosPrev mouseMoveData
// writeIORef mouseMoveDataRef $ mouseMoveData
// { _mousePos = newPos
// , _mousePosDelta = posDelta
// }
//
// -- TODO: Use Queue or Stack
// keyCallBack :: IORef KeyboardInputData -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
// keyCallBack keyboardInputDataRef window key scanCode keyState modifierKeys = do
// keyboardInputData <- readIORef keyboardInputDataRef
// let keyboardPressed = GLFW.KeyState'Pressed == keyState || GLFW.KeyState'Repeating == keyState
// keyboardReleased = GLFW.KeyState'Released == keyState
// keyPressedMap = _keyPressedMap keyboardInputData
// keyReleasedMap = _keyReleasedMap keyboardInputData
// HashTable.insert keyPressedMap key keyboardPressed
// HashTable.insert keyReleasedMap key (not keyboardPressed)
// writeIORef keyboardInputDataRef $ keyboardInputData
// { _keyboardPressed = keyboardPressed
// , _keyboardDown = keyboardPressed
// , _keyboardUp = keyboardReleased
// , _modifierKeys = modifierKeys }
//
// -- TODO: Use Queue or Stack
// charCallBack :: GLFW.Window -> Char -> IO ()
// charCallBack windows key = do
// -- logInfo $ show key
// return ()
//
// windowSizeCallback :: IORef Bool -> IORef (Int, Int) -> GLFW.Window -> Int -> Int -> IO ()
// windowSizeCallback windowSizeChangedRef windowSizeRef window sizeX sizeY = do
// atomicWriteIORef windowSizeChangedRef True
// atomicWriteIORef windowSizeRef (sizeX, sizeY)
//
//
// createGLFWWindow :: String
// -> IORef (Int, Int)
// -> IORef Bool
// -> IORef KeyboardInputData
// -> IORef MouseInputData
// -> IORef MouseMoveData
// -> IO GLFW.Window
// createGLFWWindow title windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef mouseMoveDataRef = do
// GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")
// logInfo "Initialized GLFW."
// Just version <- GLFW.getVersionString
// logInfo $ ("GLFW Version: " ++) version
// (width, height) <- readIORef windowSizeRef
// GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
// GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
// GLFW.windowHint $ WindowHint'Resizable True
// Just window <- GLFW.createWindow width height title Nothing Nothing
// GLFW.setWindowSizeCallback window $ Just (windowSizeCallback windowSizeChangedRef windowSizeRef)
// GLFW.setKeyCallback window $ Just (keyCallBack keyboardInputDataRef)
// GLFW.setCharCallback window $ Just charCallBack
// GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback mouseInputDataRef)
// GLFW.setCursorPosCallback window $ Just (cursorPosCallback mouseMoveDataRef)
// GLFW.setScrollCallback window $ Just (scrollCallback mouseMoveDataRef)
// return window
//
// destroyGLFWWindow :: GLFW.Window -> IO ()
// destroyGLFWWindow window = do
// GLFW.destroyWindow window >> logInfo "Closed GLFW window."
// GLFW.terminate >> logInfo "Terminated GLFW."
//
// updateEvent :: ApplicationData -> IO ()
// updateEvent applicationData@ApplicationData {..} = do
// -- TODO: Use Queue or Stack for IO Events
// deltaTime <- realToFrac <$> getDeltaTime applicationData
// keyboardInputData <- readIORef _keyboardInputData
// mouseInputData <- readIORef _mouseInputData
// mouseMoveData <- readIORef _mouseMoveData
// pressed_key_A <- getKeyPressed keyboardInputData GLFW.Key'A
// pressed_key_D <- getKeyPressed keyboardInputData GLFW.Key'D
// pressed_key_W <- getKeyPressed keyboardInputData GLFW.Key'W
// pressed_key_S <- getKeyPressed keyboardInputData GLFW.Key'S
// pressed_key_Q <- getKeyPressed keyboardInputData GLFW.Key'Q
// pressed_key_E <- getKeyPressed keyboardInputData GLFW.Key'E
// pressed_key_Z <- getKeyPressed keyboardInputData GLFW.Key'Z
// pressed_key_C <- getKeyPressed keyboardInputData GLFW.Key'C
// released_key_LeftBracket <- getKeyReleased keyboardInputData GLFW.Key'LeftBracket
// released_key_RightBracket <- getKeyReleased keyboardInputData GLFW.Key'RightBracket
// mainCamera <- readIORef (SceneManager._mainCamera $ _sceneManagerData)
// cameraMoveSpeed <- readIORef _cameraMoveSpeed
// let mousePosDelta = _mousePosDelta mouseMoveData
// mousePosDeltaX = fromIntegral . unScalar $ (mousePosDelta .! Idx 0) :: Float
// mousePosDeltaY = fromIntegral . unScalar $ (mousePosDelta .! Idx 1) :: Float
// scroll_xoffset = _scroll_xoffset mouseMoveData
// scroll_yoffset = _scroll_yoffset mouseMoveData
// btn_left = _btn_l_down mouseInputData
// btn_middle = _btn_m_down mouseInputData
// btn_right = _btn_r_down mouseInputData
// modifierKeysShift = (GLFW.modifierKeysShift._modifierKeys $ keyboardInputData)
// modifiedCameraMoveSpeed = max 0.1 $ min 100.0 (cameraMoveSpeed + scroll_yoffset)
// cameraMoveSpeedMiltiplier = (if modifierKeysShift then 2.0 else 1.0) * modifiedCameraMoveSpeed
// moveSpeed = Constants.cameraMoveSpeed * cameraMoveSpeedMiltiplier * deltaTime
// panSpeed = Constants.cameraPanSpeed * cameraMoveSpeedMiltiplier
// rotationSpeed = Constants.cameraRotationSpeed
// cameraTransformObject = _transformObject mainCamera
//
// if released_key_LeftBracket then
// Renderer.prevDebugRenderTarget _rendererData
// else when released_key_RightBracket $ do
// Renderer.nextDebugRenderTarget _rendererData
//
// when (0.0 /= scroll_yoffset) $
// writeIORef _cameraMoveSpeed modifiedCameraMoveSpeed
//
// if btn_left && btn_right then do
// moveLeft cameraTransformObject (-panSpeed * mousePosDeltaX)
// moveUp cameraTransformObject (panSpeed * mousePosDeltaY)
// else when btn_right $ do
// rotationPitch cameraTransformObject (-rotationSpeed * mousePosDeltaY)
// rotationYaw cameraTransformObject (-rotationSpeed * mousePosDeltaX)
//
// if pressed_key_Z then
// rotationRoll cameraTransformObject (-rotationSpeed * 0.5)
// else when pressed_key_C $
// rotationRoll cameraTransformObject (rotationSpeed * 0.5)
//
// if pressed_key_W then
// moveFront cameraTransformObject (-moveSpeed)
// else when pressed_key_S $
// moveFront cameraTransformObject moveSpeed
//
// if pressed_key_A then
// moveLeft cameraTransformObject (-moveSpeed)
// else when pressed_key_D $
// moveLeft cameraTransformObject moveSpeed
//
// if pressed_key_Q then
// moveUp cameraTransformObject (-moveSpeed)
// else when pressed_key_E $
// moveUp cameraTransformObject moveSpeed
//
//
// initializeApplication :: IO ApplicationData
// initializeApplication = do
// let (width, height) = (1024 :: Int, 786)
// mousePos = vec2 (div width 2) (div height 2)
// keyboardInputData <- newKeyboardInputData
// keyboardInputDataRef <- newIORef keyboardInputData
// mouseMoveDataRef <- newIORef $ newMouseMoveData mousePos
// mouseInputDataRef <- newIORef newMouseInputData
// windowSizeChangedRef <- newIORef False
// windowSizeRef <- newIORef (width, height)
// window <- createGLFWWindow "Vulkan Application" windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef mouseMoveDataRef
// logInfo "<< Initialized GLFW window >>"
// requireExtensions <- GLFW.getRequiredInstanceExtensions
// instanceExtensionNames <- getInstanceExtensionSupport
// checkExtensionResult <- checkExtensionSupport instanceExtensionNames requireExtensions
// unless checkExtensionResult (throwVKMsg "Failed to initialize GLFW window.")
//
// let progName = Constants.engineName
// engineName = Constants.engineName
// enableValidationLayer = Constants.enableValidationLayer
// isConcurrentMode = Constants.isConcurrentMode
//
// resources <- Resource.createResources
// rendererData <- Renderer.createRenderer
// window
// progName
// engineName
// enableValidationLayer
// isConcurrentMode
// requireExtensions
// resources
// sceneManagerData <- SceneManager.newSceneManagerData rendererData resources
//
// -- init system variables
// currentTime <- getSystemTime
// timeData <- newIORef TimeData
// { _accFrameTime = 0.0
// , _accFrameCount = 0
// , _averageFrameTime = 0.0
// , _averageFPS = 0.0
// , _currentTime = currentTime
// , _elapsedTime = 0.0
// , _deltaTime = 0.0
// }
// cameraMoveSpeed <- newIORef 1.0
//
// let applicationData = ApplicationData
// { _window = window
// , _windowSizeChanged = windowSizeChangedRef
// , _windowSize = windowSizeRef
// , _timeData = timeData
// , _cameraMoveSpeed = cameraMoveSpeed
// , _keyboardInputData = keyboardInputDataRef
// , _mouseMoveData = mouseMoveDataRef
// , _mouseInputData = mouseInputDataRef
// , _sceneManagerData = sceneManagerData
// , _rendererData = rendererData
// , _resources = resources
// }
//
// -- initlaize managers
// Resource.initializeResources resources rendererData
//
// let aspect = if 0 /= height then (fromIntegral width / fromIntegral height)::Float else 1.0
// cameraCreateData = getDefaultCameraCreateData { aspect = aspect, position = vec3 0 0 10 }
//
// SceneManager.openSceneManagerData sceneManagerData cameraCreateData
//
// return applicationData
//
// updateLoop :: ApplicationData -> IORef Command -> IORef Command -> (ApplicationData -> Command -> IO ()) -> IO ()
// updateLoop applicationData commandToEditor commandToApp loopAction = do
// recvCommand <- readIORef commandToApp
// atomicWriteIORef commandToApp Command_None
// moveInputData <- readIORef (_mouseInputData applicationData)
// moveMoveData <- readIORef (_mouseMoveData applicationData)
// keyboardInputData <- readIORef (_keyboardInputData applicationData)
// escReleased <- getKeyReleased keyboardInputData GLFW.Key'Escape
// exit <- GLFW.windowShouldClose (_window applicationData)
// let closeApp = (Command_Close_App == recvCommand) || escReleased || exit
// when (not closeApp) $ do
// -- clear IO events
// writeIORef (_keyboardInputData applicationData) keyboardInputData
// { _keyboardDown = False
// , _keyboardUp = False
// }
// writeIORef (_mouseMoveData applicationData) moveMoveData
// { _scroll_xoffset = 0.0
// , _scroll_yoffset = 0.0
// , _mousePosDelta = vec2 0 0
// , _mousePosPrev = _mousePos moveMoveData
// }
// clearHashTable (_keyReleasedMap keyboardInputData) (\_ -> return ())
//
// -- receive events
// GLFW.pollEvents
//
// -- update
// updateEvent applicationData
// loopAction applicationData recvCommand
// updateLoop applicationData commandToEditor commandToApp loopAction
//
// terminateApplication :: ApplicationData -> IO ()
// terminateApplication applicationData = do
// logInfo "<< Terminate >>"
//
// -- waiting
// Renderer.deviceWaitIdle (_rendererData applicationData)
//
// Resource.destroyResources (_resources applicationData) (_rendererData applicationData)
// Renderer.destroyRenderer (_rendererData applicationData)
// destroyGLFWWindow (_window applicationData)
//
//
// updateTimeData :: IORef TimeData -> IO ()
// updateTimeData timeDataRef = do
// currentTime <- getSystemTime
// timeData <- readIORef timeDataRef
// let previousTime = _currentTime timeData
// deltaTime = currentTime - previousTime
// elapsedTime = (_elapsedTime timeData) + deltaTime
// accFrameTime = (_accFrameTime timeData) + deltaTime
// accFrameCount = (_accFrameCount timeData) + 1
// (accFrameTime, accFrameCount, averageFrameTime, averageFPS) <-
// if (1.0 < accFrameTime) then do
// let averageFrameTime = accFrameTime / (fromIntegral accFrameCount) * 1000.0
// averageFPS = 1000.0 / averageFrameTime
// logInfo $ show averageFPS ++ "fps / " ++ show averageFrameTime ++ "ms"
// return (0.0, 0, averageFrameTime, averageFPS)
// else
// return (accFrameTime, accFrameCount, (_averageFrameTime timeData), (_averageFPS timeData))
//
// writeIORef timeDataRef TimeData
// { _deltaTime = deltaTime
// , _currentTime = currentTime
// , _elapsedTime = elapsedTime
// , _accFrameTime = accFrameTime
// , _accFrameCount = accFrameCount
// , _averageFrameTime = averageFrameTime
// , _averageFPS = averageFPS
// }
//
// runApplication :: IORef Command -> IORef Command -> IO ()
// runApplication commandToEditor commandToApp = do
// applicationData <- initializeApplication
//
// -- Main Loop
// updateLoop applicationData commandToEditor commandToApp $ \applicationData recvCommand -> do
// elapsedTime <- getElapsedTime applicationData
// updateTimeData $ _timeData applicationData
// deltaTime <- realToFrac <$> getDeltaTime applicationData
//
// let rendererData = _rendererData applicationData
// resources = _resources applicationData
// sceneManagerData = _sceneManagerData applicationData
// isFirstUpdate = (0.0 == elapsedTime)
//
// -- resize window
// needRecreateSwapChain <- readIORef (Renderer._needRecreateSwapChainRef rendererData)
// windowSizeChanged <- readIORef (_windowSizeChanged applicationData)
// when (windowSizeChanged || needRecreateSwapChain || Command_Resize_Window == recvCommand) $ do
// when (not isFirstUpdate) $
// Renderer.resizeWindow (_window applicationData) rendererData
// writeIORef (_windowSizeChanged applicationData) False
// writeIORef (Renderer._needRecreateSwapChainRef rendererData) False
// (width, height) <- readIORef (_windowSize applicationData)
// let aspect = if 0 /= height then (fromIntegral width / fromIntegral height)::Float else 1.0
// mainCamera <- SceneManager.getMainCamera sceneManagerData
// setAspect mainCamera aspect
//
// -- update renderer data
// SceneManager.updateSceneManagerData sceneManagerData elapsedTime deltaTime
//
// -- render scene
// Renderer.renderScene rendererData sceneManagerData elapsedTime deltaTime
//
// terminateApplication applicationData

use vulkano::buffer::{BufferUsage, CpuAccessibleBuffer};
use vulkano::command_buffer::{AutoCommandBufferBuilder, DynamicState};
use vulkano::device::{Device, DeviceExtensions};
use vulkano::framebuffer::{Framebuffer, FramebufferAbstract, RenderPassAbstract, Subpass};
use vulkano::image::{ImageUsage, SwapchainImage};
use vulkano::instance::{Instance, PhysicalDevice};
use vulkano::pipeline::viewport::Viewport;
use vulkano::pipeline::GraphicsPipeline;
use vulkano::swapchain;
use vulkano::swapchain::{
    AcquireError, ColorSpace, FullscreenExclusive, PresentMode, SurfaceTransform, Swapchain,
    SwapchainCreationError,
};
use vulkano::sync;
use vulkano::sync::{FlushError, GpuFuture};

use vulkano_win::VkSurfaceBuild;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};

use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct TimeData
    { _acc_frame_time: f64
    , _acc_frame_count: i32
    , _average_frame_time: f64
    , _average_fps : f64
    , _current_time : f64
    , _elapsed_time : f64
    , _delta_time : f64
    }

#[derive(Default, Debug, Clone)]
pub struct ApplicationData
    { _window: bool // GLFW.Window
    , _window_size_changed: bool
    , _window_size: (i32, i32)
    , _time_data: TimeData
    , _camera_move_speed: f32
    , _keyboard_input_data: bool // KeyboardInputData
    , _mouse_move_data: bool // MouseMoveData
    , _mouse_input_data: bool // MouseInputData
    , _scene_manager_data: bool // SceneManager.SceneManagerData
    , _renderer_data: bool // Renderer.RendererData
    , _resources: bool // Resource.Resources
    }

pub fn run_application() {
    let required_extensions = vulkano_win::required_extensions();
    let instance = Instance::new(None, &required_extensions, None).unwrap();
    let physical = PhysicalDevice::enumerate(&instance).next().unwrap();
    println!("Using device: {} (type: {:?})", physical.name(), physical.ty());
    let event_loop = EventLoop::new();
    let surface = WindowBuilder::new()
        .build_vk_surface(&event_loop, instance.clone())
        .unwrap();
    let queue_family = physical
        .queue_families()
        .find(|&q| {q.supports_graphics() && surface.is_supported(q).unwrap_or(false)})
        .unwrap();
    let device_ext = DeviceExtensions {
        khr_swapchain: true,
        ..DeviceExtensions::none()
    };
    let (device, mut queues) = Device::new(
        physical,
        physical.supported_features(),
        &device_ext,
        [(queue_family, 0.5)].iter().cloned(),
    ).unwrap();
    let queue = queues.next().unwrap();
    let (mut swapchain, images) = {
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

    let vertex_buffer = {
        #[derive(Default, Debug, Clone)]
        struct Vertex {
            position: [f32; 2],
        }
        vulkano::impl_vertex!(Vertex, position);

        CpuAccessibleBuffer::from_iter(
            device.clone(),
            BufferUsage::all(),
            false,
            [ Vertex { position: [-0.5, -0.25] },
                Vertex { position: [0.0, 0.5] },
                Vertex { position: [0.25, -0.1] }
            ].iter().cloned(),
        ).unwrap()
    };

    mod vs {
        vulkano_shaders::shader! {
            ty: "vertex",
            src: "
				#version 450
				layout(location = 0) in vec2 position;
				void main() {
					gl_Position = vec4(position, 0.0, 1.0);
				}
			"
        }
    }

    mod fs {
        vulkano_shaders::shader! {
            ty: "fragment",
            src: "
				#version 450
				layout(location = 0) out vec4 f_color;
				void main() {
					f_color = vec4(1.0, 0.0, 0.0, 1.0);
				}
			"
        }
    }

    let vs = vs::Shader::load(device.clone()).unwrap();
    let fs = fs::Shader::load(device.clone()).unwrap();
    let render_pass = Arc::new(
        vulkano::single_pass_renderpass!(
            device.clone(),
            attachments: {
                color: {
                    load: Clear,
                    store: Store,
                    format: swapchain.format(),
                    samples: 1,
                }
            },
            pass: {
                color: [color],
                depth_stencil: {}
            }
        ).unwrap(),
    );

    let pipeline = Arc::new(
        GraphicsPipeline::start()
            .vertex_input_single_buffer()
            .vertex_shader(vs.main_entry_point(), ())
            .triangle_list()
            .viewports_dynamic_scissors_irrelevant(1)
            .fragment_shader(fs.main_entry_point(), ())
            .render_pass(Subpass::from(render_pass.clone(), 0).unwrap())
            .build(device.clone())
            .unwrap(),
    );
    let mut dynamic_state = DynamicState {
        line_width: None,
        viewports: None,
        scissors: None,
        compare_mask: None,
        write_mask: None,
        reference: None,
    };
    let mut framebuffers = window_size_dependent_setup(&images, render_pass.clone(), &mut dynamic_state);
    let mut recreate_swapchain = false;
    let mut previous_frame_end = Some(sync::now(device.clone()).boxed());

    event_loop.run(move |event, _, control_flow| {
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => { *control_flow = ControlFlow::Exit; }
            Event::WindowEvent {
                event: WindowEvent::Resized(_),
                ..
            } => { recreate_swapchain = true; }
            Event::RedrawEventsCleared => {
                previous_frame_end.as_mut().unwrap().cleanup_finished();
                if recreate_swapchain {
                    let dimensions: [u32; 2] = surface.window().inner_size().into();
                    let (new_swapchain, new_images) =
                        match swapchain.recreate_with_dimensions(dimensions) {
                            Ok(r) => r,
                            Err(SwapchainCreationError::UnsupportedDimensions) => return,
                            Err(e) => panic!("Failed to recreate swapchain: {:?}", e),
                        };
                    swapchain = new_swapchain;
                    framebuffers = window_size_dependent_setup(
                        &new_images,
                        render_pass.clone(),
                        &mut dynamic_state,
                    );
                    recreate_swapchain = false;
                }

                let (image_num, suboptimal, acquire_future) =
                    match swapchain::acquire_next_image(swapchain.clone(), None) {
                        Ok(r) => r,
                        Err(AcquireError::OutOfDate) => {
                            recreate_swapchain = true;
                            return;
                        }
                        Err(e) => panic!("Failed to acquire next image: {:?}", e),
                    };

                if suboptimal {
                    recreate_swapchain = true;
                }

                let clear_values = vec![[0.0, 0.0, 1.0, 1.0].into()];

                let mut builder = AutoCommandBufferBuilder::primary_one_time_submit(
                    device.clone(),
                    queue.family(),
                ).unwrap();

                builder
                    .begin_render_pass(framebuffers[image_num].clone(), false, clear_values)
                    .unwrap()
                    .draw(
                        pipeline.clone(),
                        &dynamic_state,
                        vertex_buffer.clone(),
                        (),
                        (),
                    )
                    .unwrap()
                    .end_render_pass()
                    .unwrap();
                let command_buffer = builder.build().unwrap();
                let future = previous_frame_end
                    .take()
                    .unwrap()
                    .join(acquire_future)
                    .then_execute(queue.clone(), command_buffer)
                    .unwrap()
                    .then_swapchain_present(queue.clone(), swapchain.clone(), image_num)
                    .then_signal_fence_and_flush();

                match future {
                    Ok(future) => {
                        previous_frame_end = Some(future.boxed());
                    }
                    Err(FlushError::OutOfDate) => {
                        recreate_swapchain = true;
                        previous_frame_end = Some(sync::now(device.clone()).boxed());
                    }
                    Err(e) => {
                        println!("Failed to flush future: {:?}", e);
                        previous_frame_end = Some(sync::now(device.clone()).boxed());
                    }
                }
            }
            _ => (),
        }
    });
}

fn window_size_dependent_setup(
    images: &[Arc<SwapchainImage<Window>>],
    render_pass: Arc<dyn RenderPassAbstract + Send + Sync>,
    dynamic_state: &mut DynamicState,
) -> Vec<Arc<dyn FramebufferAbstract + Send + Sync>> {
    let dimensions = images[0].dimensions();

    let viewport = Viewport {
        origin: [0.0, 0.0],
        dimensions: [dimensions[0] as f32, dimensions[1] as f32],
        depth_range: 0.0..1.0,
    };
    dynamic_state.viewports = Some(vec![viewport]);

    images
        .iter()
        .map(|image| {
            Arc::new(
                Framebuffer::start(render_pass.clone())
                    .add(image.clone())
                    .unwrap()
                    .build()
                    .unwrap(),
            ) as Arc<dyn FramebufferAbstract + Send + Sync>
        })
        .collect::<Vec<_>>()
}
