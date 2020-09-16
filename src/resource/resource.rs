{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}

module HulkanEngine3D.Resource.Resource
    ( Resources (..)
    , ResourceInterface (..)
    ) where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.ByteString as ByteString
import qualified Data.Binary as Binary
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector
import Foreign
import System.Directory
import System.FilePath.Posix
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
--import qualified Data.Map as Map

import Graphics.Vulkan.Core_1_0
import qualified Codec.Picture as Image

import qualified HulkanEngine3D.Constants as Constants
import {-# SOURCE #-} HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Mesh
import qualified HulkanEngine3D.Render.Model as Model
import qualified HulkanEngine3D.Render.Material as Material
import qualified HulkanEngine3D.Render.MaterialInstance as MaterialInstance
import HulkanEngine3D.Render.Renderer
import qualified HulkanEngine3D.Resource.ObjLoader as ObjLoader
import qualified HulkanEngine3D.Resource.ColladaLoader as ColladaLoader
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo as RenderPassCreateInfo
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import qualified HulkanEngine3D.Vulkan.GeometryBuffer as GeometryBuffer
import HulkanEngine3D.Resource.ResourceData
import HulkanEngine3D.Resource.TextureGenerator
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.BoundingBox


gatherAllFiles :: Bool
gatherAllFiles = False

materialFilePath :: FilePath
materialFilePath = "Resource/Materials"

materialInstanceFilePath :: FilePath
materialInstanceFilePath = "Resource/MaterialInstances"

meshSourceFilePath :: FilePath
meshSourceFilePath = "Resource/Externals/Meshes"

meshFilePath :: FilePath
meshFilePath = "Resource/Meshes"

ext_obj :: String
ext_obj = ".obj"

ext_collada :: String
ext_collada = ".dae"

meshSourceExts :: [String]
meshSourceExts = [ext_obj, ext_collada]

jsonExt :: String
jsonExt = ".json"

meshExt :: String
meshExt = ".mesh"

useJsonForMesh :: Bool
useJsonForMesh = False

modelFilePath :: FilePath
modelFilePath = "Resource/Models"

cubeTextureFaces :: [Text.Text]
cubeTextureFaces = ["right", "left", "top", "bottom", "front", "back"]

textureSourceFilePath :: FilePath
textureSourceFilePath = "Resource/Externals/Textures"

textureFilePath :: FilePath
textureFilePath = "Resource/Textures"

defaultMeshName :: Text.Text
defaultMeshName = "quad"

defaultModelName :: Text.Text
defaultModelName = "quad"

defaultTextureName :: Text.Text
defaultTextureName = "common/default"

defaultMaterialName :: Text.Text
defaultMaterialName = "render_pass_static_opaque"

defaultMaterialInstanceName :: Text.Text
defaultMaterialInstanceName = "default"

defaultRenderPassName :: Text.Text
defaultRenderPassName = "render_pass_static_opaque"


type ResourceDataMap a = HashTable.BasicHashTable Text.Text a
type FrameBufferDataMap = ResourceDataMap FrameBufferData
type MaterialDataMap = ResourceDataMap Material.MaterialData
type MaterialInstanceDataMap = ResourceDataMap MaterialInstance.MaterialInstanceData
type SceneManagerDataMap = ResourceDataMap SceneManagerData
type MeshDataMap = ResourceDataMap MeshData
type ModelDataMap = ResourceDataMap Model.ModelData
type TextureDataMap = ResourceDataMap TextureData
type RenderPassDataMap = ResourceDataMap RenderPassData
type DescriptorDataMap = ResourceDataMap Descriptor.DescriptorData
type MetaDataMap = ResourceDataMap MetaData

data MetaData = MetaData
    { _isEngineResource :: Bool
    , _metaFilePath :: Bool
    , _resourceDataType :: ResourceData
    , _resourceVersion :: Int
    , _resourceFilePath :: FilePath
    , _resourceModifyTime :: Text.Text
    , _sourceFilePath :: FilePath
    , _sourceModifyTime :: Text.Text
    , _sourceChanged :: Bool
    } deriving (Eq, Show)


data ResourceData = ResourceData { _data :: MeshData } deriving (Eq, Show)

data Resources = Resources
    { _metaDataMap :: MetaDataMap
    , _meshDataMap :: MeshDataMap
    , _modelDataMap :: ModelDataMap
    , _textureDataMap :: TextureDataMap
    , _frameBufferDataMap :: FrameBufferDataMap
    , _renderPassDataMap :: RenderPassDataMap
    , _materialDataMap :: MaterialDataMap
    , _materialInstanceDataMap :: MaterialInstanceDataMap
    , _descriptorDataMap :: DescriptorDataMap
    } deriving (Show)


getResourceData :: ResourceDataMap r -> Text.Text -> Text.Text -> IO r
getResourceData resourceDataMap resourceName defaultResourceName = do
    maybeData <- HashTable.lookup resourceDataMap resourceName
    case maybeData of
        Nothing -> getDefaultResourceData
        otherwise -> return (Maybe.fromJust maybeData)
    where
        getDefaultResourceData = Maybe.fromJust <$> HashTable.lookup resourceDataMap defaultResourceName

getResourceNameFromFilePath :: FilePath -> FilePath -> Text.Text
getResourceNameFromFilePath resourcePath resourceFilePath = Text.pack $ drop (length resourcePath + 1) (dropExtension resourceFilePath)

getUniqueResourceName :: ResourceDataMap r -> FilePath -> FilePath -> IO Text.Text
getUniqueResourceName resourceDataMap resourcePath resourceFilePath = do
    let resourceName = getResourceNameFromFilePath resourcePath resourceFilePath
    generateUniqueName resourceDataMap resourceName

getResourceFileName :: FilePath -> FilePath -> String -> FilePath
getResourceFileName resourceFilePath resourceName resourceExt = joinPath [resourceFilePath, addExtension resourceName resourceExt]

class ResourceInterface a where
    createResources :: IO a
    initializeResources :: a -> RendererData -> IO ()
    destroyResources :: a -> RendererData -> IO ()

    createResource :: a -> IO ()
    registResource :: a -> IO ()
    unregistResource :: a -> IO ()

    loadGraphicsDatas :: a -> RendererData -> IO ()
    unloadGraphicsDatas :: a -> RendererData -> IO ()

    loadSceneManagerDatas :: a -> RendererData -> IO ()
    unloadSceneManagerDatas :: a -> RendererData -> IO ()

    loadModelDatas :: a -> RendererData -> IO ()
    unloadModelDatas :: a -> RendererData -> IO ()
    getModelData :: a -> Text.Text -> IO Model.ModelData

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO MeshData

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO TextureData

    loadFrameBufferDatas :: a -> RendererData -> IO ()
    unloadFrameBufferDatas :: a -> RendererData -> IO ()
    getFrameBufferData :: a -> Text.Text -> IO (Maybe FrameBufferData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)
    getRenderPassPipelineData :: a -> RenderPassPipelineDataName -> IO (RenderPassData, PipelineData)

    loadMaterialDatas :: a -> RendererData -> IO ()
    unloadMaterialDatas :: a -> RendererData -> IO ()
    getMaterialData :: a -> Text.Text -> IO Material.MaterialData

    loadMaterialInstanceDatas :: a -> RendererData -> IO ()
    unloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    updateMaterialInstanceDatas :: a -> RendererData -> IO ()
    getMaterialInstanceData :: a -> Text.Text -> IO MaterialInstance.MaterialInstanceData

    getDescriptorData :: a -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
    unloadDescriptorDatas :: a -> RendererData -> IO ()


instance ResourceInterface Resources where
    createResources :: IO Resources
    createResources = do
        metaDataMap <- HashTable.new
        frameBufferDataMap <- HashTable.new
        modelDataMap <- HashTable.new
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        materialDataMap <- HashTable.new
        materialInstanceDataMap <- HashTable.new
        descriptorDataMap <- HashTable.new
        return Resources
            { _metaDataMap = metaDataMap
            , _frameBufferDataMap = frameBufferDataMap
            , _modelDataMap = modelDataMap
            , _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            , _materialDataMap = materialDataMap
            , _materialInstanceDataMap = materialInstanceDataMap
            , _descriptorDataMap = descriptorDataMap
            }

    initializeResources :: Resources -> RendererData -> IO ()
    initializeResources resources rendererData = do
        logInfo "initializeResources"
--        loadTextureDatas resources rendererData
--        loadRenderPassDatas resources rendererData
--        loadFrameBufferDatas resources rendererData
--        loadMaterialDatas resources rendererData
--        loadMaterialInstanceDatas resources rendererData
        loadMeshDatas resources rendererData
        loadModelDatas resources rendererData

    destroyResources :: Resources -> RendererData -> IO ()
    destroyResources resources rendererData = do
        logInfo "destroyResources"
        unloadModelDatas resources rendererData
        unloadMeshDatas resources rendererData
        unloadMaterialInstanceDatas resources rendererData
        unloadMaterialDatas resources rendererData
        unloadFrameBufferDatas resources rendererData
        unloadRenderPassDatas resources rendererData
        unloadTextureDatas resources rendererData
        unloadDescriptorDatas resources rendererData

    createResource :: Resources -> IO ()
    createResource resources = return ()

    registResource :: Resources -> IO ()
    registResource resources = return ()

    unregistResource :: Resources -> IO ()
    unregistResource resources = return ()

    -- GraphicsDatas
    loadGraphicsDatas :: Resources -> RendererData -> IO ()
    loadGraphicsDatas resources rendererData = do
        logInfo "Resources::loadGraphicsDatas"
        loadRenderPassDatas resources rendererData
        loadFrameBufferDatas resources rendererData
        loadMaterialDatas resources rendererData
        loadMaterialInstanceDatas resources rendererData
        updateMaterialInstanceDatas resources rendererData

    unloadGraphicsDatas :: Resources -> RendererData -> IO ()
    unloadGraphicsDatas resources rendererData = do
        logInfo "Resources::unloadGraphicsDatas"
        unloadMaterialInstanceDatas resources rendererData
        unloadMaterialDatas resources rendererData
        unloadFrameBufferDatas resources rendererData
        unloadRenderPassDatas resources rendererData
        unloadDescriptorDatas resources rendererData

    -- SceneManagerData
    loadSceneManagerDatas :: Resources -> RendererData -> IO ()
    loadSceneManagerDatas resources rendererData = return ()

    unloadSceneManagerDatas :: Resources -> RendererData -> IO ()
    unloadSceneManagerDatas resources rendererData = return ()

    -- Model Loader
    loadModelDatas :: Resources -> RendererData -> IO ()
    loadModelDatas resources rendererData = do
        modelFiles <- walkDirectory modelFilePath [".model"]
        forM_ modelFiles $ \modelFile -> do
            modelName <- getUniqueResourceName (_modelDataMap resources) modelFilePath modelFile
            contents <- ByteString.readFile modelFile
            registModelData (_modelDataMap resources) modelName contents
        where
            registModelData modelDataMap modelName contents = do
                let Just (Aeson.Object modelCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.Array materialInstanceNames) = HashMap.lookup "material_instances" modelCreateInfoMap
                    materialInstanceCount = Vector.length materialInstanceNames
                    Just (Aeson.String meshName) = HashMap.lookup "mesh" modelCreateInfoMap
                meshData <- getMeshData resources meshName
                geometryDataCount <- getGeometryDataCount meshData
                let materialInstanceNameList = (Vector.take geometryDataCount materialInstanceNames) Vector.++ (Vector.replicate (max 0 (geometryDataCount - materialInstanceCount)) (Aeson.String defaultMaterialInstanceName))
                materialInstanceDatas <- forM (Vector.toList materialInstanceNameList) $ \(Aeson.String materialInstanceName) ->
                    getMaterialInstanceData resources materialInstanceName
                modelData <- Model.newModelData modelName meshData materialInstanceDatas
                HashTable.insert modelDataMap modelName modelData

    unloadModelDatas :: Resources -> RendererData -> IO ()
    unloadModelDatas resources rendererData = do
        clearHashTable (_modelDataMap resources) (\(k, v) -> Model.destroyModelData v)

    getModelData :: Resources -> Text.Text -> IO Model.ModelData
    getModelData resources resourceName = do
        getResourceData (_modelDataMap resources) resourceName defaultModelName

    -- Mesh Loader
    loadMeshDatas :: Resources -> RendererData -> IO ()
    loadMeshDatas resources rendererData = do
        registMeshData (_meshDataMap resources) "quad" GeometryBuffer.quadGeometryCreateInfos
        registMeshData (_meshDataMap resources) "cube" GeometryBuffer.cubeGeometryCreateInfos

        let resourceExt = if useJsonForMesh then jsonExt else meshExt
        meshFiles <- walkDirectory meshFilePath [resourceExt]
        let meshFileMap = Map.fromList $ map (\meshFile -> (getResourceNameFromFilePath meshFilePath meshFile, meshFile)) meshFiles
        meshSourceFiles <- walkDirectory meshSourceFilePath meshSourceExts
        forM_ meshSourceFiles $ \meshSourceFile -> do
            meshName <- getUniqueResourceName (_meshDataMap resources) meshSourceFilePath meshSourceFile
            let resourceName = Text.unpack meshName
                file_ext = map Char.toLower (takeExtension meshSourceFile)
            geometryCreateInfos <- case Map.lookup meshName meshFileMap of
                Just meshFile ->
                    -- Load mesh
                    if useJsonForMesh then
                        Maybe.fromJust <$> (Aeson.decodeFileStrict meshFile)
                    else do
                        contents <- Binary.decodeFile meshFile::IO [([Word8], [Word8], [Word8])]
                        forM contents $ \(vertexDatas, indexDatas, boundingBoxData) -> do
                            boundingBox <- alloca $ \ptr -> do
                                pokeArray ptr boundingBoxData
                                peek (castPtr ptr::Ptr BoundingBox)
                            return GeometryBuffer.GeometryCreateInfo
                                { GeometryBuffer._geometryCreateInfoVertices = (SVector.unsafeCast (SVector.fromList vertexDatas)::SVector.Vector GeometryBuffer.VertexData)
                                , GeometryBuffer._geometryCreateInfoIndices = (SVector.unsafeCast (SVector.fromList indexDatas)::SVector.Vector Word32)
                                , GeometryBuffer._geometryCreateInfoBoundingBox = boundingBox
                                }
                otherwise -> do
                    -- Convert to mesh from source
                    geometryCreateInfos <-
                        if ext_obj == file_ext then
                            ObjLoader.loadMesh meshSourceFile
                        else if ext_collada == file_ext then do
                            ColladaLoader.loadCollada meshSourceFile
                            error "error"
                        else
                            return []
                    -- Save mesh
                    createDirectoryIfMissing True (takeDirectory $ combine meshFilePath resourceName)
                    if useJsonForMesh then
                        Aeson.encodeFile (getResourceFileName meshFilePath resourceName resourceExt) geometryCreateInfos
                    else do
                        contents <- forM geometryCreateInfos $ \geometryCreateInfo -> do
                            let vertexDatas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoVertices geometryCreateInfo)::SVector.Vector Word8)
                                indexDatas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoIndices geometryCreateInfo)::SVector.Vector Word8)
                                boundingBox = GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo
                            boundingBoxData <- alloca $ \ptr -> do
                                poke ptr (GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo)
                                let count = sizeOf (undefined::BoundingBox) `div` sizeOf (undefined::Word8)
                                peekArray count (castPtr ptr::Ptr Word8)
                            return ((vertexDatas, indexDatas, boundingBoxData)::([Word8], [Word8], [Word8]))
                        Binary.encodeFile (getResourceFileName meshFilePath resourceName resourceExt) contents
                    return geometryCreateInfos
            registMeshData (_meshDataMap resources) meshName geometryCreateInfos
        where
            registMeshData :: MeshDataMap -> Text.Text -> [GeometryBuffer.GeometryCreateInfo] -> IO ()
            registMeshData meshDataMap meshName geometryCreateInfos = do
                geometryBufferDatas <- forM (zip ([0..]::[Int]) geometryCreateInfos) $ \(index, geometryCreateInfo) -> do
                    createGeometryBuffer rendererData (Text.append meshName (Text.pack $ show index)) geometryCreateInfo
                meshData <- newMeshData meshName geometryBufferDatas
                HashTable.insert (_meshDataMap resources) meshName meshData

    unloadMeshDatas :: Resources -> RendererData -> IO ()
    unloadMeshDatas resources rendererData = do
        HashTable.mapM_ (\(k, v) -> (destroyGeometryData rendererData k v)) (_meshDataMap resources)
        where
            destroyGeometryData rendererData name meshData = do
                geometryDataCount <- getGeometryDataCount meshData
                forM_  [0..(geometryDataCount - 1)] $ \index -> do
                    geometryData <- getGeometryData meshData index
                    destroyGeometryBuffer rendererData geometryData

    getMeshData :: Resources -> Text.Text -> IO MeshData
    getMeshData resources resourceName =
        getResourceData (_meshDataMap resources) resourceName defaultMeshName

    -- TextureLoader
    loadTextureDatas :: Resources -> RendererData -> IO ()
    loadTextureDatas resources rendererData = do
        textureDatas <- generateTextures rendererData
        forM_ textureDatas $ \textureData -> do
            HashTable.insert (_textureDataMap resources) (_textureDataName textureData) textureData

        -- generate necessary texture datas
        generateImages rendererData textureSourceFilePath

        -- load texture from files
        textureFiles <- walkDirectory textureSourceFilePath [".jpg", ".png", ".tga", ".bmp"]
        forM_ textureFiles $ \textureFile -> do
            let (textureDataName, cubeTextureFiles) = getTextureDataName textureFiles textureFile
                isCubeTexture = not $ null cubeTextureFiles
            existingResourceData <- HashTable.lookup (_textureDataMap resources) textureDataName
            when (Nothing == existingResourceData) $ do
                ((imageWidth, imageHeight, imageData), imageFormat) <-
                    if isCubeTexture then
                        loadImageDatas cubeTextureFiles
                    else
                        loadImageData textureFile
                let textureCreateInfo = defaultTextureCreateInfo
                        { _textureCreateInfoWidth = fromIntegral imageWidth
                        , _textureCreateInfoHeight = fromIntegral imageHeight
                        , _textureCreateInfoData = imageData
                        , _textureCreateInfoFormat = imageFormat
                        , _textureCreateInfoViewType = if isCubeTexture then VK_IMAGE_VIEW_TYPE_CUBE else VK_IMAGE_VIEW_TYPE_2D
                        }
                textureData <- createTexture rendererData textureDataName textureCreateInfo
                HashTable.insert (_textureDataMap resources) textureDataName textureData
        where
            getTextureDataName :: [FilePath] -> FilePath -> (Text.Text, [String])
            getTextureDataName textureFiles textureFile =
                let textureDataName = getResourceNameFromFilePath textureSourceFilePath textureFile
                    (textureFileName, _) = Text.breakOnEnd "_" (Text.pack textureFile)
                    fileExt = Text.pack $ takeExtension textureFile
                    cubeFaceFiles = [Text.unpack $ Text.concat [textureFileName, face, fileExt] | face <- cubeTextureFaces]
                    isCubeTexture = ("" /= textureFileName) && (all id [elem filePath textureFiles | filePath <- cubeFaceFiles])
                    cubeTextureFileName = Text.dropWhileEnd (== '_') textureFileName
                in if isCubeTexture then
                        (getResourceNameFromFilePath textureSourceFilePath $ Text.unpack cubeTextureFileName, cubeFaceFiles)
                    else
                        (textureDataName, [])
            loadImageData :: FilePath -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
            loadImageData textureFile = do
                imageRawData <- Image.readImage textureFile
                case imageRawData of
                    Left err -> throwVKMsg err
                    Right dynamicImage ->
                        pure $ case dynamicImage of
                            Image.ImageRGBA8 image -> (image8ToTuple image, VK_FORMAT_R8G8B8A8_UNORM)
                            Image.ImageRGBF image -> (imageFloatToTuple image, VK_FORMAT_R32G32B32_SFLOAT)
                            Image.ImageRGBA16 image -> (image16ToTuple image, VK_FORMAT_R16G16B16A16_UNORM)
                            otherwise -> (image8ToTuple (Image.convertRGBA8 dynamicImage), VK_FORMAT_R8G8B8A8_UNORM)
                            where
                                image8ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
                                    let imageData8 = imageData::SVector.Vector Word8 in (imageWidth, imageHeight, imageData8)
                                image16ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
                                    let imageData16 = imageData::SVector.Vector Word16 in (imageWidth, imageHeight, SVector.unsafeCast imageData16)
                                imageFloatToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
                                    let imageDataFloat = imageData::SVector.Vector Float in (imageWidth, imageHeight, SVector.unsafeCast imageDataFloat)
            loadImageDatas :: [FilePath] -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
            loadImageDatas textureFiles = do
                imageDatas <- mapM loadImageData textureFiles
                return $ flip foldl1 imageDatas (\((width, height, accImageData), format) ((_, _, imageData), _) ->
                    ((width, height, accImageData SVector.++ imageData), format))

    unloadTextureDatas :: Resources -> RendererData -> IO ()
    unloadTextureDatas resources rendererData =
        clearHashTable (_textureDataMap resources) (\(k, v) -> destroyTexture rendererData v)

    getTextureData :: Resources -> Text.Text -> IO TextureData
    getTextureData resources resourceName =
        getResourceData (_textureDataMap resources) resourceName defaultTextureName

    -- FrameBuffer
    loadFrameBufferDatas :: Resources -> RendererData -> IO ()
    loadFrameBufferDatas resources rendererData = do
        renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
        let frameBufferCreateInfoList = map (\renderPassDataCreateInfo -> (_renderPassCreateInfoName renderPassDataCreateInfo, _renderPassFrameBufferCreateInfo renderPassDataCreateInfo)) renderPassDataCreateInfos
            frameBufferCreateInfoMap = Map.fromList frameBufferCreateInfoList
        HashTable.mapM_ (\(k, v) -> registFrameBufferData frameBufferCreateInfoMap k) (_renderPassDataMap resources)
        where
            registFrameBufferData :: Map.Map Text.Text FrameBufferDataCreateInfo -> Text.Text -> IO ()
            registFrameBufferData frameBufferCreateInfoMap renderPassName = do
                Just renderPassData <- getRenderPassData resources renderPassName
                let frameBufferName = _renderPassFrameBufferName (renderPassData::RenderPassData)
                    Just frameBufferDataCreateInfo = Map.lookup frameBufferName frameBufferCreateInfoMap
                frameBufferData <- createFrameBufferData (getDevice rendererData) (_renderPass renderPassData) frameBufferDataCreateInfo
                HashTable.insert (_frameBufferDataMap resources) frameBufferName frameBufferData

    unloadFrameBufferDatas :: Resources -> RendererData -> IO ()
    unloadFrameBufferDatas resources rendererData =
        clearHashTable (_frameBufferDataMap resources) (\(k, v) -> destroyFrameBufferData (getDevice rendererData) v)

    getFrameBufferData :: Resources -> Text.Text -> IO (Maybe FrameBufferData)
    getFrameBufferData resources resourceName =
       HashTable.lookup (_frameBufferDataMap resources) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: Resources -> RendererData -> IO ()
    loadRenderPassDatas resources rendererData = do
        renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
        mapM_ registRenderPassData renderPassDataCreateInfos
        where
            registRenderPassData renderPassDataCreateInfo = do
                descriptorDatas <- forM (_pipelineDataCreateInfos renderPassDataCreateInfo) $ \pipelineDataCreateInfo -> do
                    getDescriptorData resources rendererData (_renderPassCreateInfoName renderPassDataCreateInfo) pipelineDataCreateInfo
                defaultRenderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo descriptorDatas
                HashTable.insert (_renderPassDataMap resources) (_renderPassDataName defaultRenderPassData) defaultRenderPassData

    unloadRenderPassDatas :: Resources -> RendererData -> IO ()
    unloadRenderPassDatas resources rendererData =
        clearHashTable (_renderPassDataMap resources) (\(k, v) -> destroyRenderPassData (getDevice rendererData) v)

    getRenderPassData :: Resources -> Text.Text -> IO (Maybe RenderPassData)
    getRenderPassData resources resourceName =
        HashTable.lookup (_renderPassDataMap resources) resourceName

    getDefaultRenderPassData :: Resources -> IO (Maybe RenderPassData)
    getDefaultRenderPassData resources =
        getRenderPassData resources defaultRenderPassName

    getRenderPassPipelineData :: Resources -> RenderPassPipelineDataName -> IO (RenderPassData, PipelineData)
    getRenderPassPipelineData resources (renderPassDataName, pipelineDataName) = do
        Just renderPassData <- getRenderPassData resources renderPassDataName
        pipelineData <- getPipelineData renderPassData pipelineDataName
        return (renderPassData, pipelineData)

    -- MaterialDatas
    loadMaterialDatas :: Resources -> RendererData -> IO ()
    loadMaterialDatas resources rendererData = do
        materialFiles <- walkDirectory materialFilePath [".mat"]
        forM_ materialFiles $ \materialFile -> do
            materialName <- getUniqueResourceName (_materialDataMap resources) materialFilePath materialFile
            contents <- ByteString.readFile materialFile
            registMaterialData rendererData (_materialDataMap resources) materialName contents
        where
            registMaterialData rendererData materialDataMap materialName contents = do
                let Just (Aeson.Object materialCreateInfo) = Aeson.decodeStrict contents
                    Just (Aeson.Array pipelineCreateInfoArray) = HashMap.lookup "pipelines" materialCreateInfo
                    Aeson.Object materialParameterMap = HashMap.lookupDefault (Aeson.Object HashMap.empty) "material_parameters" materialCreateInfo
                renderPassPipelineDataList <- forM pipelineCreateInfoArray $ \(Aeson.Object pipelineCreateInfo) -> do
                    let Just (Aeson.String renderPassDataName) = HashMap.lookup "renderPass" pipelineCreateInfo
                        Just (Aeson.String pipelineDataName) = HashMap.lookup "pipleline" pipelineCreateInfo
                    getRenderPassPipelineData resources (renderPassDataName, pipelineDataName)
                material <- Material.createMaterial materialName (Vector.toList renderPassPipelineDataList) materialParameterMap
                HashTable.insert (_materialDataMap resources) materialName material

    unloadMaterialDatas :: Resources -> RendererData -> IO ()
    unloadMaterialDatas resources rendererData =
        clearHashTable (_materialDataMap resources) (\(k, v) -> Material.destroyMaterial v)

    getMaterialData :: Resources -> Text.Text -> IO Material.MaterialData
    getMaterialData resources resourceName =
        getResourceData (_materialDataMap resources) resourceName defaultMaterialName

    -- MaterialInstanceDatas
    loadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    loadMaterialInstanceDatas resources rendererData = do
        materialInstanceFiles <- walkDirectory materialInstanceFilePath [".matinst"]
        forM_ materialInstanceFiles $ \materialInstanceFile -> do
            materialInstanceName <- getUniqueResourceName (_materialInstanceDataMap resources) materialInstanceFilePath materialInstanceFile
            contents <- ByteString.readFile materialInstanceFile
            registMaterialInstanceData rendererData (_materialInstanceDataMap resources) materialInstanceName contents
        where
            registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
                let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.String materialDataName) = HashMap.lookup "material_name" materialInstanceCreateInfoMap
                    Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap

                materialData <- getMaterialData resources materialDataName
                let defaultMaterialParameterMap = Material._materialParameterMap materialData
                pipelineBindingCreateInfoList <- forM (Map.toList $ Material._renderPassPipelineDataMap materialData) $ \(key, (renderPassData, pipelineData)) -> do
                    let descriptorDataCreateInfoList = Descriptor._descriptorDataCreateInfoList $ _descriptorData pipelineData
                    descriptorResourceInfosList <- forM Constants.swapChainImageIndices $ \swapChainIndex -> do
                        descriptorResourceInfos <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
                            let materialParameterName = Descriptor._descriptorName' descriptorDataCreateInfo
                                materialParameterType = Descriptor._descriptorType' descriptorDataCreateInfo
                                materialParameterResourceType = Descriptor._descriptorResourceType' descriptorDataCreateInfo
                                maybeMaterialParameter = lookupWithDefaultMap materialParameterName materialParameterMap defaultMaterialParameterMap
                            case (materialParameterType, materialParameterResourceType) of
                                (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, Descriptor.DescriptorResourceType_UniformBuffer) -> do
                                    uniformBufferData <- getUniformBufferData rendererData (fromText materialParameterName)
                                    return $ Descriptor.DescriptorBufferInfo (atSwapChainIndex swapChainIndex (_descriptorBufferInfos uniformBufferData))
                                (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_Texture) -> do
                                    textureData <- case maybeMaterialParameter of
                                        Just (Aeson.String value) -> getTextureData resources value
                                        otherwise -> getTextureData resources defaultTextureName
                                    return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                                (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
                                    textureData <- getRenderTarget rendererData (fromText materialParameterName)
                                    return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                                otherwise -> return Descriptor.InvalidDescriptorInfo
                        return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
                    return (renderPassData, pipelineData, descriptorResourceInfosList)
                materialInstance <- MaterialInstance.createMaterialInstance (getDevice rendererData) materialInstanceName materialData pipelineBindingCreateInfoList
                HashTable.insert (_materialInstanceDataMap resources) materialInstanceName materialInstance

    unloadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    unloadMaterialInstanceDatas resources rendererData =
        clearHashTable (_materialInstanceDataMap resources) (\(k, v) -> MaterialInstance.destroyMaterialInstance (getDevice rendererData) v)

    updateMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    updateMaterialInstanceDatas resources rendererData =
        flip HashTable.mapM_ (_modelDataMap resources) $ \(k, modelData) -> do
            materialInstances <- Model.getMaterialInstanceDataList modelData
            newMaterialInstances <- forM materialInstances $ \materialInstance ->
                getMaterialInstanceData resources (MaterialInstance._materialInstanceDataName materialInstance)
            Model.setMaterialInstanceDataList modelData newMaterialInstances

    getMaterialInstanceData :: Resources -> Text.Text -> IO MaterialInstance.MaterialInstanceData
    getMaterialInstanceData resources resourceName =
        getResourceData (_materialInstanceDataMap resources) resourceName defaultMaterialInstanceName

    -- DescriptorDatas
    getDescriptorData :: Resources -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
    getDescriptorData resources rendererData renderPassName pipelineDataCreateInfo = do
        let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList (pipelineDataCreateInfo::PipelineDataCreateInfo)
            maxDescriptorPoolCount = Constants.maxDescriptorPoolAllocCount * Constants.descriptorSetCountAtOnce
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resources) descriptorName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- Descriptor.createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList maxDescriptorPoolCount
                HashTable.insert (_descriptorDataMap resources) descriptorName descriptorData
                return descriptorData

    unloadDescriptorDatas :: Resources -> RendererData -> IO ()
    unloadDescriptorDatas resources rendererData = do
        clearHashTable (_descriptorDataMap resources) (\(k, v) -> Descriptor.destroyDescriptorData (getDevice rendererData) v)
