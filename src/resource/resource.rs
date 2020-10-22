use std::rc::Rc;
use std::cell::RefCell;
use std::path::{ PathBuf };
use std::collections::HashMap;

use crate::vulkan_context::descriptor;
use crate::vulkan_context::render_pass::RenderPassData;
use crate::vulkan_context::texture::TextureData;
use crate::renderer::{ model, MeshData, material_instance, material };
use crate::application::SceneManagerData;
use crate::vulkan_context::framebuffer::FramebufferData;

const GATHER_ALL_FILES: bool = false;
const MATERIAL_FILE_PATH: &str = "Resource/Materials";
const MATERIAL_INSTANCE_FILE_PATH: &str = "Resource/MaterialInstances";
const MESH_SOURCE_FILE_PATH: &str = "Resource/Externals/Meshes";
const MESH_FILE_PATH: &str = "Resource/Meshes";
const EXT_OBJ: &str = ".obj";
const EXT_COLLADA: &str = ".dae";
const MESH_SOURCE_EXTS: [&str; 2] = [EXT_OBJ, EXT_COLLADA];
const EXT_JSON: &str = ".json";
const EXT_MESH: &str = ".mesh";
const USE_JSON_FOR_MESH: bool = false;
const MODEL_FILE_PATH: &str = "Resource/Models";
const CUBE_TEXTURE_FACES: [&str; 6] = ["right", "left", "top", "bottom", "front", "back"];
const TEXTURE_SOURCE_FILE_PATH: &str = "Resource/Externals/Textures";
const TEXTURE_FILE_PATH: &str = "Resource/Textures";
const DEFAULT_MESH_NAME: &str = "quad";
const DEFAULT_MODEL_NAME: &str = "quad";
const DEFAULT_TEXTURE_NAME: &str = "common/default";
const DEFAULT_MATERIAL_NAME: &str = "render_pass_static_opaque";
const DEFAULT_MATERIAL_INSTANCE_NAME: &str = "default";
const DEFAULT_RENDER_PASS_NAME: &str = "render_pass_static_opaque";

pub type ResourceDataMap<T> = HashMap<String, Rc<RefCell<T>>>;
pub type FramebufferDataMap = ResourceDataMap<FramebufferData>;
pub type MaterialDataMap = ResourceDataMap<material::MaterialData>;
pub type MaterialInstanceDataMap = ResourceDataMap<material_instance::MaterialInstanceData>;
pub type SceneManagerDataMap = ResourceDataMap<SceneManagerData>;
pub type MeshDataMap = ResourceDataMap<MeshData>;
pub type ModelDataMap = ResourceDataMap<model::ModelData>;
pub type TextureDataMap = ResourceDataMap<TextureData>;
pub type RenderPassDataMap = ResourceDataMap<RenderPassData>;
pub type DescriptorDataMap = ResourceDataMap<descriptor::DescriptorData>;
pub type MetaDataMap = ResourceDataMap<MetaData>;


#[derive(Debug, Clone)]
pub struct MetaData {
    _is_engine_resource: bool,
    _meta_file_path: bool,
    _resource_data_type: ResourceData,
    _resource_version: i32,
    _resource_file_path: PathBuf,
    _resource_modify_time: String,
    _source_file_path: PathBuf,
    _source_modify_time: String,
    _source_changed: bool
}

#[derive(Clone, Debug, Copy)]
pub enum ResourceData {
    ResourceDataMesh,
}

#[derive(Debug, Clone)]
pub struct Resources {
    pub _meta_data_map: MetaDataMap,
    pub _mesh_data_map: MeshDataMap,
    pub _model_data_map: ModelDataMap,
    pub _texture_data_map: TextureDataMap,
    pub _framebuffer_data_map: FramebufferDataMap,
    pub _render_pass_data_map: RenderPassDataMap,
    pub _material_data_map: MaterialDataMap,
    pub _material_instance_data_map: MaterialInstanceDataMap,
    pub _descriptor_data_map: DescriptorDataMap
}

pub fn create_resources() -> Rc<RefCell<Resources>> {
    Rc::new(RefCell::new(Resources {
        _meta_data_map: MetaDataMap::new(),
        _mesh_data_map: MeshDataMap::new(),
        _model_data_map: ModelDataMap::new(),
        _texture_data_map: TextureDataMap::new(),
        _framebuffer_data_map: FramebufferDataMap::new(),
        _render_pass_data_map: RenderPassDataMap::new(),
        _material_data_map: MaterialDataMap::new(),
        _material_instance_data_map: MaterialInstanceDataMap::new(),
        _descriptor_data_map: DescriptorDataMap::new()
    }))
}

impl Resources {
    pub fn get_resource_data<T>(resource_data_map: &ResourceDataMap<T>, resource_name: &String, default_resource_name: &String) -> Rc<RefCell<T>> {
        let maybe_data = resource_data_map.get(resource_name);
        let data = match maybe_data {
            None => resource_data_map.get(default_resource_name).unwrap(),
            _ => maybe_data.unwrap(),
        };
        data.clone()
    }

// getResourceNameFromFilePath :: FilePath -> FilePath -> String
// getResourceNameFromFilePath resourcePath resourceFilePath = Text.pack $ drop (length resourcePath + 1) (dropExtension resourceFilePath)
//
// getUniqueResourceName :: ResourceDataMap r -> FilePath -> FilePath -> IO String
// getUniqueResourceName resourceDataMap resourcePath resourceFilePath = do
//     let resourceName = getResourceNameFromFilePath resourcePath resourceFilePath
//     generateUniqueName resourceDataMap resourceName
//
// getResourceFileName :: FilePath -> FilePath -> String -> FilePath
// getResourceFileName resourceFilePath resourceName resourceExt = joinPath [resourceFilePath, addExtension resourceName resourceExt]

// instance ResourceInterface Resources where
//     initializeResources :: Resources -> RendererData -> IO ()
//     initializeResources resources rendererData = do
//         logInfo "initializeResources"
// --        loadTextureDatas resources rendererData
// --        loadRenderPassDatas resources rendererData
// --        loadFramebufferDatas resources rendererData
// --        loadMaterialDatas resources rendererData
// --        loadMaterialInstanceDatas resources rendererData
//         loadMeshDatas resources rendererData
//         loadModelDatas resources rendererData
//
//     destroyResources :: Resources -> RendererData -> IO ()
//     destroyResources resources rendererData = do
//         logInfo "destroyResources"
//         unloadModelDatas resources rendererData
//         unloadMeshDatas resources rendererData
//         unloadMaterialInstanceDatas resources rendererData
//         unloadMaterialDatas resources rendererData
//         unloadFramebufferDatas resources rendererData
//         unloadRenderPassDatas resources rendererData
//         unloadTextureDatas resources rendererData
//         unloadDescriptorDatas resources rendererData
//
//     createResource :: Resources -> IO ()
//     createResource resources = return ()
//
//     registResource :: Resources -> IO ()
//     registResource resources = return ()
//
//     unregistResource :: Resources -> IO ()
//     unregistResource resources = return ()
//
//     -- GraphicsDatas
//     loadGraphicsDatas :: Resources -> RendererData -> IO ()
//     loadGraphicsDatas resources rendererData = do
//         logInfo "Resources::loadGraphicsDatas"
//         loadRenderPassDatas resources rendererData
//         loadFramebufferDatas resources rendererData
//         loadMaterialDatas resources rendererData
//         loadMaterialInstanceDatas resources rendererData
//         updateMaterialInstanceDatas resources rendererData
//
//     unloadGraphicsDatas :: Resources -> RendererData -> IO ()
//     unloadGraphicsDatas resources rendererData = do
//         logInfo "Resources::unloadGraphicsDatas"
//         unloadMaterialInstanceDatas resources rendererData
//         unloadMaterialDatas resources rendererData
//         unloadFramebufferDatas resources rendererData
//         unloadRenderPassDatas resources rendererData
//         unloadDescriptorDatas resources rendererData
//
//     -- SceneManagerData
//     loadSceneManagerDatas :: Resources -> RendererData -> IO ()
//     loadSceneManagerDatas resources rendererData = return ()
//
//     unloadSceneManagerDatas :: Resources -> RendererData -> IO ()
//     unloadSceneManagerDatas resources rendererData = return ()
//
//     -- Model Loader
//     loadModelDatas :: Resources -> RendererData -> IO ()
//     loadModelDatas resources rendererData = do
//         modelFiles <- walkDirectory modelFilePath [".model"]
//         forM_ modelFiles $ \modelFile -> do
//             modelName <- getUniqueResourceName (_modelDataMap resources) modelFilePath modelFile
//             contents <- ByteString.readFile modelFile
//             registModelData (_modelDataMap resources) modelName contents
//         where
//             registModelData modelDataMap modelName contents = do
//                 let Just (Aeson.Object modelCreateInfoMap) = Aeson.decodeStrict contents
//                     Just (Aeson.Array materialInstanceNames) = HashMap.lookup "material_instances" modelCreateInfoMap
//                     materialInstanceCount = Vector.length materialInstanceNames
//                     Just (Aeson.String meshName) = HashMap.lookup "mesh" modelCreateInfoMap
//                 meshData <- getMeshData resources meshName
//                 geometryDataCount <- getGeometryDataCount meshData
//                 let materialInstanceNameList = (Vector.take geometryDataCount materialInstanceNames) Vector.++ (Vector.replicate (max 0 (geometryDataCount - materialInstanceCount)) (Aeson.String defaultMaterialInstanceName))
//                 materialInstanceDatas <- forM (Vector.toList materialInstanceNameList) $ \(Aeson.String materialInstanceName) ->
//                     getMaterialInstanceData resources materialInstanceName
//                 modelData <- Model.newModelData modelName meshData materialInstanceDatas
//                 HashTable.insert modelDataMap modelName modelData
//
//     unloadModelDatas :: Resources -> RendererData -> IO ()
//     unloadModelDatas resources rendererData = do
//         clearHashTable (_modelDataMap resources) (\(k, v) -> Model.destroyModelData v)
//
//     getModelData :: Resources -> String -> IO Model.ModelData
//     getModelData resources resourceName = do
//         getResourceData (_modelDataMap resources) resourceName defaultModelName
//
//     -- Mesh Loader
//     loadMeshDatas :: Resources -> RendererData -> IO ()
//     loadMeshDatas resources rendererData = do
//         registMeshData (_meshDataMap resources) "quad" GeometryBuffer.quadGeometryCreateInfos
//         registMeshData (_meshDataMap resources) "cube" GeometryBuffer.cubeGeometryCreateInfos
//
//         let resourceExt = if useJsonForMesh then jsonExt else meshExt
//         meshFiles <- walkDirectory meshFilePath [resourceExt]
//         let meshFileMap = Map.fromList $ map (\meshFile -> (getResourceNameFromFilePath meshFilePath meshFile, meshFile)) meshFiles
//         meshSourceFiles <- walkDirectory meshSourceFilePath meshSourceExts
//         forM_ meshSourceFiles $ \meshSourceFile -> do
//             meshName <- getUniqueResourceName (_meshDataMap resources) meshSourceFilePath meshSourceFile
//             let resourceName = Text.unpack meshName
//                 file_ext = map Char.toLower (takeExtension meshSourceFile)
//             geometryCreateInfos <- case Map.lookup meshName meshFileMap of
//                 Just meshFile ->
//                     -- Load mesh
//                     if useJsonForMesh then
//                         Maybe.fromJust <$> (Aeson.decodeFileStrict meshFile)
//                     else do
//                         contents <- Binary.decodeFile meshFile::IO [([Word8], [Word8], [Word8])]
//                         forM contents $ \(vertexDatas, indexDatas, boundingBoxData) -> do
//                             boundingBox <- alloca $ \ptr -> do
//                                 pokeArray ptr boundingBoxData
//                                 peek (castPtr ptr::Ptr BoundingBox)
//                             return GeometryBuffer.GeometryCreateInfo
//                                 { GeometryBuffer._geometryCreateInfoVertices = (SVector.unsafeCast (SVector.fromList vertexDatas)::SVector.Vector GeometryBuffer.VertexData)
//                                 , GeometryBuffer._geometryCreateInfoIndices = (SVector.unsafeCast (SVector.fromList indexDatas)::SVector.Vector Word32)
//                                 , GeometryBuffer._geometryCreateInfoBoundingBox = boundingBox
//                                 }
//                 otherwise -> do
//                     -- Convert to mesh from source
//                     geometryCreateInfos <-
//                         if ext_obj == file_ext then
//                             ObjLoader.loadMesh meshSourceFile
//                         else if ext_collada == file_ext then do
//                             ColladaLoader.loadCollada meshSourceFile
//                             error "error"
//                         else
//                             return []
//                     -- Save mesh
//                     createDirectoryIfMissing True (takeDirectory $ combine meshFilePath resourceName)
//                     if useJsonForMesh then
//                         Aeson.encodeFile (getResourceFileName meshFilePath resourceName resourceExt) geometryCreateInfos
//                     else do
//                         contents <- forM geometryCreateInfos $ \geometryCreateInfo -> do
//                             let vertexDatas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoVertices geometryCreateInfo)::SVector.Vector Word8)
//                                 indexDatas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoIndices geometryCreateInfo)::SVector.Vector Word8)
//                                 boundingBox = GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo
//                             boundingBoxData <- alloca $ \ptr -> do
//                                 poke ptr (GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo)
//                                 let count = sizeOf (undefined::BoundingBox) `div` sizeOf (undefined::Word8)
//                                 peekArray count (castPtr ptr::Ptr Word8)
//                             return ((vertexDatas, indexDatas, boundingBoxData)::([Word8], [Word8], [Word8]))
//                         Binary.encodeFile (getResourceFileName meshFilePath resourceName resourceExt) contents
//                     return geometryCreateInfos
//             registMeshData (_meshDataMap resources) meshName geometryCreateInfos
//         where
//             registMeshData :: MeshDataMap -> String -> [GeometryBuffer.GeometryCreateInfo] -> IO ()
//             registMeshData meshDataMap meshName geometryCreateInfos = do
//                 geometryBufferDatas <- forM (zip ([0..]::[Int]) geometryCreateInfos) $ \(index, geometryCreateInfo) -> do
//                     createGeometryBuffer rendererData (Text.append meshName (Text.pack $ show index)) geometryCreateInfo
//                 meshData <- newMeshData meshName geometryBufferDatas
//                 HashTable.insert (_meshDataMap resources) meshName meshData
//
//     unloadMeshDatas :: Resources -> RendererData -> IO ()
//     unloadMeshDatas resources rendererData = do
//         HashTable.mapM_ (\(k, v) -> (destroyGeometryData rendererData k v)) (_meshDataMap resources)
//         where
//             destroyGeometryData rendererData name meshData = do
//                 geometryDataCount <- getGeometryDataCount meshData
//                 forM_  [0..(geometryDataCount - 1)] $ \index -> do
//                     geometryData <- getGeometryData meshData index
//                     destroyGeometryBuffer rendererData geometryData
//
//     getMeshData :: Resources -> String -> IO MeshData
//     getMeshData resources resourceName =
//         getResourceData (_meshDataMap resources) resourceName defaultMeshName
//
//     -- TextureLoader
//     loadTextureDatas :: Resources -> RendererData -> IO ()
//     loadTextureDatas resources rendererData = do
//         textureDatas <- generateTextures rendererData
//         forM_ textureDatas $ \textureData -> do
//             HashTable.insert (_textureDataMap resources) (_textureDataName textureData) textureData
//
//         -- generate necessary texture datas
//         generateImages rendererData textureSourceFilePath
//
//         -- load texture from files
//         textureFiles <- walkDirectory textureSourceFilePath [".jpg", ".png", ".tga", ".bmp"]
//         forM_ textureFiles $ \textureFile -> do
//             let (textureDataName, cubeTextureFiles) = getTextureDataName textureFiles textureFile
//                 isCubeTexture = not $ null cubeTextureFiles
//             existingResourceData <- HashTable.lookup (_textureDataMap resources) textureDataName
//             when (Nothing == existingResourceData) $ do
//                 ((imageWidth, imageHeight, imageData), imageFormat) <-
//                     if isCubeTexture then
//                         loadImageDatas cubeTextureFiles
//                     else
//                         loadImageData textureFile
//                 let textureCreateInfo = defaultTextureCreateInfo
//                         { _textureCreateInfoWidth = fromIntegral imageWidth
//                         , _textureCreateInfoHeight = fromIntegral imageHeight
//                         , _textureCreateInfoData = imageData
//                         , _textureCreateInfoFormat = imageFormat
//                         , _textureCreateInfoViewType = if isCubeTexture then VK_IMAGE_VIEW_TYPE_CUBE else VK_IMAGE_VIEW_TYPE_2D
//                         }
//                 textureData <- createTexture rendererData textureDataName textureCreateInfo
//                 HashTable.insert (_textureDataMap resources) textureDataName textureData
//         where
//             getTextureDataName :: [FilePath] -> FilePath -> (String, [String])
//             getTextureDataName textureFiles textureFile =
//                 let textureDataName = getResourceNameFromFilePath textureSourceFilePath textureFile
//                     (textureFileName, _) = Text.breakOnEnd "_" (Text.pack textureFile)
//                     fileExt = Text.pack $ takeExtension textureFile
//                     cubeFaceFiles = [Text.unpack $ Text.concat [textureFileName, face, fileExt] | face <- cubeTextureFaces]
//                     isCubeTexture = ("" /= textureFileName) && (all id [elem filePath textureFiles | filePath <- cubeFaceFiles])
//                     cubeTextureFileName = Text.dropWhileEnd (== '_') textureFileName
//                 in if isCubeTexture then
//                         (getResourceNameFromFilePath textureSourceFilePath $ Text.unpack cubeTextureFileName, cubeFaceFiles)
//                     else
//                         (textureDataName, [])
//             loadImageData :: FilePath -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
//             loadImageData textureFile = do
//                 imageRawData <- Image.readImage textureFile
//                 case imageRawData of
//                     Left err -> throwVKMsg err
//                     Right dynamicImage ->
//                         pure $ case dynamicImage of
//                             Image.ImageRGBA8 image -> (image8ToTuple image, VK_FORMAT_R8G8B8A8_UNORM)
//                             Image.ImageRGBF image -> (imageFloatToTuple image, VK_FORMAT_R32G32B32_SFLOAT)
//                             Image.ImageRGBA16 image -> (image16ToTuple image, VK_FORMAT_R16G16B16A16_UNORM)
//                             otherwise -> (image8ToTuple (Image.convertRGBA8 dynamicImage), VK_FORMAT_R8G8B8A8_UNORM)
//                             where
//                                 image8ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
//                                     let imageData8 = imageData::SVector.Vector Word8 in (imageWidth, imageHeight, imageData8)
//                                 image16ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
//                                     let imageData16 = imageData::SVector.Vector Word16 in (imageWidth, imageHeight, SVector.unsafeCast imageData16)
//                                 imageFloatToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
//                                     let imageDataFloat = imageData::SVector.Vector Float in (imageWidth, imageHeight, SVector.unsafeCast imageDataFloat)
//             loadImageDatas :: [FilePath] -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
//             loadImageDatas textureFiles = do
//                 imageDatas <- mapM loadImageData textureFiles
//                 return $ flip foldl1 imageDatas (\((width, height, accImageData), format) ((_, _, imageData), _) ->
//                     ((width, height, accImageData SVector.++ imageData), format))
//
//     unloadTextureDatas :: Resources -> RendererData -> IO ()
//     unloadTextureDatas resources rendererData =
//         clearHashTable (_textureDataMap resources) (\(k, v) -> destroyTexture rendererData v)
//
//     getTextureData :: Resources -> String -> IO TextureData
//     getTextureData resources resourceName =
//         getResourceData (_textureDataMap resources) resourceName defaultTextureName
//
//     -- Framebuffer
//     loadFramebufferDatas :: Resources -> RendererData -> IO ()
//     loadFramebufferDatas resources rendererData = do
//         renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
//         let frameBufferCreateInfoList = map (\renderPassDataCreateInfo -> (_renderPassCreateInfoName renderPassDataCreateInfo, _renderPassFramebufferCreateInfo renderPassDataCreateInfo)) renderPassDataCreateInfos
//             frameBufferCreateInfoMap = Map.fromList frameBufferCreateInfoList
//         HashTable.mapM_ (\(k, v) -> registFramebufferData frameBufferCreateInfoMap k) (_renderPassDataMap resources)
//         where
//             registFramebufferData :: Map.Map String FramebufferDataCreateInfo -> String -> IO ()
//             registFramebufferData frameBufferCreateInfoMap renderPassName = do
//                 Just renderPassData <- getRenderPassData resources renderPassName
//                 let frameBufferName = _renderPassFramebufferName (renderPassData::RenderPassData)
//                     Just frameBufferDataCreateInfo = Map.lookup frameBufferName frameBufferCreateInfoMap
//                 frameBufferData <- createFramebufferData (getDevice rendererData) (_renderPass renderPassData) frameBufferDataCreateInfo
//                 HashTable.insert (_frameBufferDataMap resources) frameBufferName frameBufferData
//
//     unloadFramebufferDatas :: Resources -> RendererData -> IO ()
//     unloadFramebufferDatas resources rendererData =
//         clearHashTable (_frameBufferDataMap resources) (\(k, v) -> destroyFramebufferData (getDevice rendererData) v)
//
//     getFramebufferData :: Resources -> String -> IO (Maybe FramebufferData)
//     getFramebufferData resources resourceName =
//        HashTable.lookup (_frameBufferDataMap resources) resourceName
//
//     -- RenderPassLoader
//     loadRenderPassDatas :: Resources -> RendererData -> IO ()
//     loadRenderPassDatas resources rendererData = do
//         renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
//         mapM_ registRenderPassData renderPassDataCreateInfos
//         where
//             registRenderPassData renderPassDataCreateInfo = do
//                 descriptorDatas <- forM (_pipelineDataCreateInfos renderPassDataCreateInfo) $ \pipelineDataCreateInfo -> do
//                     getDescriptorData resources rendererData (_renderPassCreateInfoName renderPassDataCreateInfo) pipelineDataCreateInfo
//                 defaultRenderPassData <- create_render_pass_data (getDevice rendererData) renderPassDataCreateInfo descriptorDatas
//                 HashTable.insert (_renderPassDataMap resources) (_renderPassDataName defaultRenderPassData) defaultRenderPassData
//
//     unloadRenderPassDatas :: Resources -> RendererData -> IO ()
//     unloadRenderPassDatas resources rendererData =
//         clearHashTable (_renderPassDataMap resources) (\(k, v) -> destroyRenderPassData (getDevice rendererData) v)
//
//     getRenderPassData :: Resources -> String -> IO (Maybe RenderPassData)
//     getRenderPassData resources resourceName =
//         HashTable.lookup (_renderPassDataMap resources) resourceName
//
//     getDefaultRenderPassData :: Resources -> IO (Maybe RenderPassData)
//     getDefaultRenderPassData resources =
//         getRenderPassData resources defaultRenderPassName
//
//     getRenderPassPipelineData :: Resources -> RenderPassPipelineDataName -> IO (RenderPassData, PipelineData)
//     getRenderPassPipelineData resources (renderPassDataName, pipelineDataName) = do
//         Just renderPassData <- getRenderPassData resources renderPassDataName
//         pipelineData <- getPipelineData renderPassData pipelineDataName
//         return (renderPassData, pipelineData)
//
//     -- MaterialDatas
//     loadMaterialDatas :: Resources -> RendererData -> IO ()
//     loadMaterialDatas resources rendererData = do
//         materialFiles <- walkDirectory materialFilePath [".mat"]
//         forM_ materialFiles $ \materialFile -> do
//             materialName <- getUniqueResourceName (_materialDataMap resources) materialFilePath materialFile
//             contents <- ByteString.readFile materialFile
//             registMaterialData rendererData (_materialDataMap resources) materialName contents
//         where
//             registMaterialData rendererData materialDataMap materialName contents = do
//                 let Just (Aeson.Object materialCreateInfo) = Aeson.decodeStrict contents
//                     Just (Aeson.Array pipelineCreateInfoArray) = HashMap.lookup "pipelines" materialCreateInfo
//                     Aeson.Object materialParameterMap = HashMap.lookupDefault (Aeson.Object HashMap.empty) "material_parameters" materialCreateInfo
//                 renderPassPipelineDataList <- forM pipelineCreateInfoArray $ \(Aeson.Object pipelineCreateInfo) -> do
//                     let Just (Aeson.String renderPassDataName) = HashMap.lookup "renderPass" pipelineCreateInfo
//                         Just (Aeson.String pipelineDataName) = HashMap.lookup "pipleline" pipelineCreateInfo
//                     getRenderPassPipelineData resources (renderPassDataName, pipelineDataName)
//                 material <- Material.createMaterial materialName (Vector.toList renderPassPipelineDataList) materialParameterMap
//                 HashTable.insert (_materialDataMap resources) materialName material
//
//     unloadMaterialDatas :: Resources -> RendererData -> IO ()
//     unloadMaterialDatas resources rendererData =
//         clearHashTable (_materialDataMap resources) (\(k, v) -> Material.destroyMaterial v)
//
//     getMaterialData :: Resources -> String -> IO Material.MaterialData
//     getMaterialData resources resourceName =
//         getResourceData (_materialDataMap resources) resourceName defaultMaterialName
//
//     -- MaterialInstanceDatas
//     loadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
//     loadMaterialInstanceDatas resources rendererData = do
//         materialInstanceFiles <- walkDirectory materialInstanceFilePath [".matinst"]
//         forM_ materialInstanceFiles $ \materialInstanceFile -> do
//             materialInstanceName <- getUniqueResourceName (_materialInstanceDataMap resources) materialInstanceFilePath materialInstanceFile
//             contents <- ByteString.readFile materialInstanceFile
//             registMaterialInstanceData rendererData (_materialInstanceDataMap resources) materialInstanceName contents
//         where
//             registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
//                 let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
//                     Just (Aeson.String materialDataName) = HashMap.lookup "material_name" materialInstanceCreateInfoMap
//                     Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap
//
//                 materialData <- getMaterialData resources materialDataName
//                 let defaultMaterialParameterMap = Material._materialParameterMap materialData
//                 pipelineBindingCreateInfoList <- forM (Map.toList $ Material._renderPassPipelineDataMap materialData) $ \(key, (renderPassData, pipelineData)) -> do
//                     let descriptorDataCreateInfoList = Descriptor._descriptorDataCreateInfoList $ _descriptorData pipelineData
//                     descriptorResourceInfosList <- forM Constants.swapChainImageIndices $ \swapChainIndex -> do
//                         descriptorResourceInfos <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
//                             let materialParameterName = Descriptor._descriptorName' descriptorDataCreateInfo
//                                 materialParameterType = Descriptor._descriptorType' descriptorDataCreateInfo
//                                 materialParameterResourceType = Descriptor._descriptorResourceType' descriptorDataCreateInfo
//                                 maybeMaterialParameter = lookupWithDefaultMap materialParameterName materialParameterMap defaultMaterialParameterMap
//                             case (materialParameterType, materialParameterResourceType) of
//                                 (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, Descriptor.DescriptorResourceType_UniformBuffer) -> do
//                                     uniformBufferData <- getUniformBufferData rendererData (fromText materialParameterName)
//                                     return $ Descriptor.DescriptorBufferInfo (atSwapchainIndex swapChainIndex (_descriptorBufferInfos uniformBufferData))
//                                 (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_Texture) -> do
//                                     textureData <- case maybeMaterialParameter of
//                                         Just (Aeson.String value) -> getTextureData resources value
//                                         otherwise -> getTextureData resources defaultTextureName
//                                     return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
//                                 (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
//                                     textureData <- getRenderTarget rendererData (fromText materialParameterName)
//                                     return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
//                                 otherwise -> return Descriptor.InvalidDescriptorInfo
//                         return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
//                     return (renderPassData, pipelineData, descriptorResourceInfosList)
//                 materialInstance <- MaterialInstance.createMaterialInstance (getDevice rendererData) materialInstanceName materialData pipelineBindingCreateInfoList
//                 HashTable.insert (_materialInstanceDataMap resources) materialInstanceName materialInstance
//
//     unloadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
//     unloadMaterialInstanceDatas resources rendererData =
//         clearHashTable (_materialInstanceDataMap resources) (\(k, v) -> MaterialInstance.destroyMaterialInstance (getDevice rendererData) v)
//
//     updateMaterialInstanceDatas :: Resources -> RendererData -> IO ()
//     updateMaterialInstanceDatas resources rendererData =
//         flip HashTable.mapM_ (_modelDataMap resources) $ \(k, modelData) -> do
//             materialInstances <- Model.getMaterialInstanceDataList modelData
//             newMaterialInstances <- forM materialInstances $ \materialInstance ->
//                 getMaterialInstanceData resources (MaterialInstance._materialInstanceDataName materialInstance)
//             Model.setMaterialInstanceDataList modelData newMaterialInstances
//
//     getMaterialInstanceData :: Resources -> String -> IO MaterialInstance.MaterialInstanceData
//     getMaterialInstanceData resources resourceName =
//         getResourceData (_materialInstanceDataMap resources) resourceName defaultMaterialInstanceName
//
//     -- DescriptorDatas
//     getDescriptorData :: Resources -> RendererData -> String -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
//     getDescriptorData resources rendererData renderPassName pipelineDataCreateInfo = do
//         let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
//             descriptorDataCreateInfoList = _descriptorDataCreateInfoList (pipelineDataCreateInfo::PipelineDataCreateInfo)
//             maxDescriptorPoolCount = Constants.maxDescriptorPoolAllocCount * Constants.descriptorSetCountAtOnce
//         maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resources) descriptorName
//         case maybeDescriptorData of
//             (Just descriptorData) -> return descriptorData
//             otherwise -> do
//                 descriptorData <- Descriptor.createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList maxDescriptorPoolCount
//                 HashTable.insert (_descriptorDataMap resources) descriptorName descriptorData
//                 return descriptorData
//
//     unloadDescriptorDatas :: Resources -> RendererData -> IO ()
//     unloadDescriptorDatas resources rendererData = do
//         clearHashTable (_descriptorDataMap resources) (\(k, v) -> Descriptor.destroyDescriptorData (getDevice rendererData) v)

}