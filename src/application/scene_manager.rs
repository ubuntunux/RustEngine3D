use std::rc::Rc;

use crate::renderer;
use crate::resource;

// type CameraObjectMap = HashTable.BasicHashTable Text.Text Camera.CameraObjectData
// type DirectionalLightObjectMap = HashTable.BasicHashTable Text.Text Light.DirectionalLightData
// type RenderObjectMap = HashTable.BasicHashTable Text.Text RenderObject.RenderObjectData

#[derive(Clone)]
pub struct SceneManagerData {
    _renderer_data: Rc<renderer::RendererData>, //Renderer.RendererData,
    _resources: Box<resource::Resources>, //Resource.Resources,
    _main_camera: bool, //Camera.CameraObjectData,
    _main_light: bool, //Light.DirectionalLightData,
    _camera_object_map: bool, //CameraObjectMap,
    _directional_light_object_map: bool, //DirectionalLightObjectMap,
    _static_render_object_map: bool, //RenderObjectMap,
    _static_render_elements: bool, //[RenderElement.RenderElementData],
    _skeletal_render_object_map: bool, //RenderObjectMap,
    _skeletal_render_elements: bool, //[RenderElement.RenderElementData]
}

pub fn create_scene_manager_data(
    renderer_data: Rc<renderer::RendererData>,
    resources: Box<resource::Resources>
) -> Box<SceneManagerData> {
    Box::new(SceneManagerData {
        _renderer_data: renderer_data,
        _resources: resources,
        _main_camera: false,
        _main_light: false,
        _camera_object_map: false,
        _directional_light_object_map: false,
        _static_render_object_map: false,
        _static_render_elements: false,
        _skeletal_render_object_map: false,
        _skeletal_render_elements: false,
    })
}

impl SceneManagerData {
}


// class SceneManagerInterface a where
//     newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO a
//     openSceneManagerData :: a -> Camera.CameraCreateData -> IO ()
//     getMainCamera :: a -> IO Camera.CameraObjectData
//     addCameraObject :: a -> Text.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
//     getMainLight :: a -> IO Light.DirectionalLightData
//     addDirectionalLightObject :: a -> Text.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
//     addRenderObject :: a -> Text.Text -> RenderObject.RenderObjectCreateData -> IO RenderObject.RenderObjectData
//     getStaticRenderObject :: a -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
//     getSkeletalRenderObject :: a -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
//     getStaticObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
//     getSkeletalObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
//     updateSceneManagerData :: a -> Double -> Float -> IO ()
//
// instance SceneManagerInterface SceneManagerData where
//     newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO SceneManagerData
//     newSceneManagerData rendererData resources = do
//         mainCamera <- newIORef (undefined::Camera.CameraObjectData)
//         mainLight <- newIORef (undefined::Light.DirectionalLightData)
//         cameraObjectMap <- HashTable.new
//         directionalLightObjectMap <- HashTable.new
//         staticRenderObjectMap <- HashTable.new
//         staticRenderElements <- newIORef []
//         skeletalRenderObjectMap <- HashTable.new
//         skeletalRenderElements <- newIORef []
//         return SceneManagerData
//             { _rendererData = rendererData
//             , _resources = resources
//             , _mainCamera = mainCamera
//             , _mainLight = mainLight
//             , _cameraObjectMap = cameraObjectMap
//             , _directionalLightObjectMap = directionalLightObjectMap
//             , _staticRenderObjectMap = staticRenderObjectMap
//             , _staticRenderElements = staticRenderElements
//             , _skeletalRenderObjectMap = skeletalRenderObjectMap
//             , _skeletalRenderElements = skeletalRenderElements
//             }
//
//     openSceneManagerData :: SceneManagerData -> Camera.CameraCreateData -> IO ()
//     openSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
//         mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
//         writeIORef _mainCamera mainCamera
//
//         mainLight <- addDirectionalLightObject sceneManagerData "MainLight" $ Light.defaultDirectionalLightCreateInfo
//             { Light._directionalLightPosition' = vec3 0 0 0
//             , Light._directionalLightRotation' = vec3 (-3.141592*0.47) 0 0.3
//             , Light._directionalLightConstants' = UniformBufferDatas.defaultLightConstants
//                 { UniformBufferDatas._LIGHT_DIRECTION = vec3 (-3.141592 * 0.47) 0 0.3
//                 }
//             }
//         writeIORef _mainLight mainLight
//
//         modelData0 <- Resource.getModelData _resources "sponza/sponza"
//         modelData1 <- Resource.getModelData _resources "test_skeletal"
//         addRenderObject sceneManagerData "sponza" $ RenderObject.defaultRenderObjectCreateData
//                     { RenderObject._modelData' = modelData0
//                     , RenderObject._position' = vec3 0 0 0
//                     , RenderObject._scale' = vec3 0.1 0.1 0.1
//                     }
//         addRenderObject sceneManagerData "test_skeletal" $ RenderObject.defaultRenderObjectCreateData
//                     { RenderObject._modelData' = modelData1
//                     , RenderObject._position' = vec3 0 1.5 0
//                     , RenderObject._scale' = vec3 1.0 1.0 1.0
//                     , RenderObject._has_animation_data' = True
//                     }
//         return ()
//
//     getMainCamera :: SceneManagerData -> IO Camera.CameraObjectData
//     getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)
//
//     addCameraObject :: SceneManagerData -> Text.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
//     addCameraObject sceneManagerData objectName cameraCreateData = do
//         newObjectName <- System.generateUniqueName (_cameraObjectMap sceneManagerData) objectName
//         cameraObjectData <- Camera.createCameraObjectData newObjectName cameraCreateData
//         HashTable.insert (_cameraObjectMap sceneManagerData) newObjectName cameraObjectData
//         return cameraObjectData
//
//     getMainLight :: SceneManagerData -> IO Light.DirectionalLightData
//     getMainLight sceneManagerData = readIORef (_mainLight sceneManagerData)
//
//     addDirectionalLightObject :: SceneManagerData -> Text.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
//     addDirectionalLightObject sceneManagerData objectName lightCreateInfo = do
//         newObjectName <- System.generateUniqueName (_directionalLightObjectMap sceneManagerData) objectName
//         lightObjectData <- Light.createLightData newObjectName lightCreateInfo
//         HashTable.insert (_directionalLightObjectMap sceneManagerData) newObjectName lightObjectData
//         return lightObjectData
//
//     addRenderObject :: SceneManagerData -> Text.Text -> RenderObject.RenderObjectCreateData -> IO RenderObject.RenderObjectData
//     addRenderObject sceneManagerData objectName renderObjectCreateData = do
//         if (RenderObject._has_animation_data' renderObjectCreateData) then
//             registRenderObject (_skeletalRenderObjectMap sceneManagerData) objectName
//         else
//             registRenderObject (_staticRenderObjectMap sceneManagerData) objectName
//         where
//             registRenderObject renderObjectMap objectName = do
//                 newObjectName <- System.generateUniqueName renderObjectMap objectName
//                 renderObjectData <- RenderObject.createRenderObjectData newObjectName renderObjectCreateData
//                 HashTable.insert renderObjectMap newObjectName renderObjectData
//                 return renderObjectData
//
//     getStaticRenderObject :: SceneManagerData -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
//     getStaticRenderObject sceneManagerData objectName = HashTable.lookup (_staticRenderObjectMap sceneManagerData) objectName
//
//     getSkeletalRenderObject :: SceneManagerData -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
//     getSkeletalRenderObject sceneManagerData objectName = HashTable.lookup (_skeletalRenderObjectMap sceneManagerData) objectName
//
//     getStaticObjectRenderElements :: SceneManagerData -> IO [RenderElement.RenderElementData]
//     getStaticObjectRenderElements sceneManagerData = readIORef (_staticRenderElements sceneManagerData)
//
//     getSkeletalObjectRenderElements :: SceneManagerData -> IO [RenderElement.RenderElementData]
//     getSkeletalObjectRenderElements sceneManagerData = readIORef (_skeletalRenderElements sceneManagerData)
//
//     updateSceneManagerData :: SceneManagerData -> Double -> Float -> IO ()
//     updateSceneManagerData sceneManagerData@SceneManagerData {..} elapsedTime deltaTime = do
//         mainCamera <- getMainCamera sceneManagerData
//         Camera.updateCameraObjectData mainCamera
//         cameraPosition <- Camera.getCameraPosition mainCamera
//
//         mainLight <- getMainLight sceneManagerData
//         Light.updateLightData mainLight cameraPosition
//
//         flip HashTable.mapM_ _staticRenderObjectMap $ \(objectName, renderObjectData) -> do
//             RenderObject.updateRenderObjectData renderObjectData
//
//         flip HashTable.mapM_ _skeletalRenderObjectMap $ \(objectName, renderObjectData) -> do
//             RenderObject.updateRenderObjectData renderObjectData
//
//         gatherRenderElementsOfRenderObject _staticRenderObjectMap _staticRenderElements
//         gatherRenderElementsOfRenderObject _skeletalRenderObjectMap _skeletalRenderElements
//
//         where
//             gatherRenderElementsOfRenderObject :: RenderObjectMap -> IORef [RenderElement.RenderElementData] -> IO ()
//             gatherRenderElementsOfRenderObject renderObjectMap renderElements = do
//                 writeIORef renderElements []
//                 flip HashTable.mapM_ renderObjectMap $ \(objectName, renderObjectData) -> do
//                     renderObjectRenderElements <- readIORef renderElements
//                     geometryBufferDatas <- readIORef (Mesh._geometryBufferDatas . Model._meshData . RenderObject._modelData $ renderObjectData)
//                     materialInstanceDatas <- readIORef (Model._materialInstanceDatas . RenderObject._modelData $ renderObjectData)
//                     renderElementList <- forM [0..(length geometryBufferDatas - 1)] $ \index -> do
//                         return RenderElement.RenderElementData
//                             { _renderObject = renderObjectData
//                             , _geometryData = geometryBufferDatas !! index
//                             , _materialInstanceData = materialInstanceDatas !! index
//                             }
//                     writeIORef renderElements (renderObjectRenderElements ++ renderElementList)
//
//
