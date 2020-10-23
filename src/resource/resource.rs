use std::path::{ PathBuf };
use std::collections::HashMap;

use crate::application::SceneManagerData;
use crate::renderer::mesh::{ MeshData };
use crate::renderer::model::{ self, ModelData };
use crate::renderer::material::{ self, MaterialData };
use crate::renderer::material_instance::{ self, MaterialInstanceData };
use crate::renderer::renderer::{ RendererData };
use crate::vulkan_context::descriptor::{ self, DescriptorData };
use crate::vulkan_context::render_pass::{
    self,
    PipelineDataCreateInfo,
    RenderPassData,
    RenderPassPipelineDataName,
    RenderPassPipelineData,
};
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::framebuffer::{ self, FramebufferData };
use crate::utilities::system::{ self, RcRefCell, newRcRefCell };


const GATHER_ALL_FILES: bool = false;
const MATERIAL_FILE_PATH: &str = "Resource/Materials";
const MATERIAL_INSTANCE_FILE_PATH: &str = "Resource/MaterialInstances";
const MESH_SOURCE_FILE_PATH: &str = "Resource/Externals/Meshes";
const MESH_FILE_PATH: &str = "Resource/Meshes";
const EXT_OBJ: &str = "obj";
const EXT_COLLADA: &str = "dae";
const MESH_SOURCE_EXTS: [&str; 2] = [EXT_OBJ, EXT_COLLADA];
const EXT_JSON: &str = "json";
const EXT_MESH: &str = "mesh";
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

pub type ResourceDataMap<T> = HashMap<String, RcRefCell<T>>;
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

pub fn create_resources() -> RcRefCell<Resources> {
    newRcRefCell(Resources {
        _meta_data_map: MetaDataMap::new(),
        _mesh_data_map: MeshDataMap::new(),
        _model_data_map: ModelDataMap::new(),
        _texture_data_map: TextureDataMap::new(),
        _framebuffer_data_map: FramebufferDataMap::new(),
        _render_pass_data_map: RenderPassDataMap::new(),
        _material_data_map: MaterialDataMap::new(),
        _material_instance_data_map: MaterialInstanceDataMap::new(),
        _descriptor_data_map: DescriptorDataMap::new()
    })
}

fn get_resource_data<'a, T>(resource_data_map: &'a ResourceDataMap<T>, resource_name: &String, default_resource_name: &str) -> &'a RcRefCell<T> {
    let maybe_data = resource_data_map.get(resource_name);
    match maybe_data {
        None => resource_data_map.get(default_resource_name).unwrap(),
        _ => maybe_data.unwrap(),
    }
}

fn get_resource_name_from_file_path(resource_root_path: &PathBuf, resource_file_path: &PathBuf) -> String {
    String::from(system::get_relative_path(resource_root_path, resource_file_path).to_str().unwrap())
}

fn get_unique_resource_name<T>(resource_map: &ResourceDataMap<T>, resource_root_path: &PathBuf, resource_file_path: &PathBuf) -> String {
    let resource_name = get_resource_name_from_file_path(resource_root_path, resource_file_path);
    system::generate_unique_name(resource_map, &resource_name)
}

pub fn get_resource_file_path(resource_root_path: &PathBuf, resource_name: &String, resource_ext: &str) -> PathBuf {
    let mut resource_file_path: PathBuf = PathBuf::from(resource_root_path);
    resource_file_path.push(resource_name);
    resource_file_path.set_extension(resource_ext);
    resource_file_path
}

impl Resources {
    pub fn initialize_resources(&mut self, renderer_data: &RendererData) {
        log::info!("initialize_resources");
        self.load_texture_datas(renderer_data);
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.load_mesh_datas(renderer_data);
        self.load_model_datas(renderer_data);
    }

    pub fn destroy_resources(&mut self, renderer_data: &RendererData) {
        log::info!("destroy_resources");
        self.unload_model_datas(renderer_data);
        self.unload_mesh_datas(renderer_data);
        self.unload_material_instance_datas(renderer_data);
        self.unload_material_datas(renderer_data);
        self.unload_framebuffer_datas(renderer_data);
        self.unload_render_pass_datas(renderer_data);
        self.unload_texture_datas(renderer_data);
        self.unload_descriptor_datas(renderer_data);
    }

    // GraphicsDatas
    pub fn load_graphics_datas(&mut self, renderer_data: &RendererData) {
        log::info!("load_graphics_datas");
        self.load_render_pass_datas(renderer_data);
        self.load_framebuffer_datas(renderer_data);
        self.load_material_datas(renderer_data);
        self.load_material_instance_datas(renderer_data);
        self.update_material_instance_datas(renderer_data);
    }

    pub fn unload_graphics_datas(&mut self, renderer_data: &RendererData) {
        log::info!("unload_graphics_datas");
        self.unload_material_instance_datas(renderer_data);
        self.unload_material_datas(renderer_data);
        self.unload_framebuffer_datas(renderer_data);
        self.unload_render_pass_datas(renderer_data);
        self.unload_descriptor_datas(renderer_data);
    }

    pub fn create_resource(&mut self) {
        // nothing..
    }

    pub fn regist_resource(&mut self) {
        // nothing..
    }

    pub fn unregist_resource(&mut self) {
        // nothing..
    }

    // SceneManagerData
    pub fn load_scene_manager_datas(&mut self, _renderer_data: &RendererData) {
        // nothing..
    }

    pub fn unload_scene_manager_datas(&mut self, _renderer_data: &RendererData) {
        // nothing..
    }

    // ModelData
    pub fn load_model_datas(&mut self, _renderer_data: &RendererData) {
//         modelFiles <- walkDirectory modelPathBuf ["model"]
//         forM_ modelFiles $ \modelFile -> do
//             modelName <- getUniqueResourceName (_modelDataMap resources) modelPathBuf modelFile
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
    }

    pub fn unload_model_datas(&mut self, _renderer_data: &RendererData) {
        for model_data in self._model_data_map.values() {
            (*model_data).borrow().destroy_model_data();
        }
        self._model_data_map.clear();
    }

    pub fn get_model_data(&self, resource_name: &String) -> &RcRefCell<ModelData> {
        get_resource_data(&self._model_data_map, resource_name, DEFAULT_MODEL_NAME)
    }

    // Mesh Loader
    pub fn load_mesh_datas(&mut self, _renderer_data: &RendererData) {
        // registMeshData (_meshDataMap resources) "quad" GeometryBuffer.quadGeometryCreateInfos
        // registMeshData (_meshDataMap resources) "cube" GeometryBuffer.cubeGeometryCreateInfos
        //
        // let resourceExt = if useJsonForMesh then jsonExt else meshExt
        // meshFiles <- walkDirectory meshPathBuf [resourceExt]
        // let meshFileMap = Map.fromList $ map (\meshFile -> (getResourceNameFromPathBuf meshPathBuf meshFile, meshFile)) meshFiles
        // meshSourceFiles <- walkDirectory meshSourcePathBuf meshSourceExts
        // forM_ meshSourceFiles $ \meshSourceFile -> do
        //     meshName <- getUniqueResourceName (_meshDataMap resources) meshSourcePathBuf meshSourceFile
        //     let resourceName = Text.unpack meshName
        //         file_ext = map Char.toLower (takeExtension meshSourceFile)
        //     geometryCreateInfos <- case Map.lookup meshName meshFileMap of
        //         Just meshFile ->
        //             -- Load mesh
        //             if useJsonForMesh then
        //                 Maybe.fromJust <$> (Aeson.decodeFileStrict meshFile)
        //             else do
        //                 contents <- Binary.decodeFile meshFile::IO [([Word8], [Word8], [Word8])]
        //                 forM contents $ \(vertex_datas, index_datas, boundingBoxData) -> do
        //                     boundingBox <- alloca $ \ptr -> do
        //                         pokeArray ptr boundingBoxData
        //                         peek (castPtr ptr::Ptr BoundingBox)
        //                     return GeometryBuffer.GeometryCreateInfo
        //                         { GeometryBuffer._geometryCreateInfoVertices = (SVector.unsafeCast (SVector.fromList vertex_datas)::SVector.Vector GeometryBuffer.VertexData)
        //                         , GeometryBuffer._geometryCreateInfoIndices = (SVector.unsafeCast (SVector.fromList index_datas)::SVector.Vector Word32)
        //                         , GeometryBuffer._geometryCreateInfoBoundingBox = boundingBox
        //                         }
        //         otherwise -> do
        //             -- Convert to mesh from source
        //             geometryCreateInfos <-
        //                 if ext_obj == file_ext then
        //                     ObjLoader.loadMesh meshSourceFile
        //                 else if ext_collada == file_ext then do
        //                     ColladaLoader.loadCollada meshSourceFile
        //                     error "error"
        //                 else
        //                     return []
        //             -- Save mesh
        //             createDirectoryIfMissing True (takeDirectory $ combine meshPathBuf resourceName)
        //             if useJsonForMesh then
        //                 Aeson.encodeFile (getResourceFileName meshPathBuf resourceName resourceExt) geometryCreateInfos
        //             else do
        //                 contents <- forM geometryCreateInfos $ \geometryCreateInfo -> do
        //                     let vertex_datas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoVertices geometryCreateInfo)::SVector.Vector Word8)
        //                         index_datas = SVector.toList (SVector.unsafeCast (GeometryBuffer._geometryCreateInfoIndices geometryCreateInfo)::SVector.Vector Word8)
        //                         boundingBox = GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo
        //                     boundingBoxData <- alloca $ \ptr -> do
        //                         poke ptr (GeometryBuffer._geometryCreateInfoBoundingBox geometryCreateInfo)
        //                         let count = sizeOf (undefined::BoundingBox) `div` sizeOf (undefined::Word8)
        //                         peekArray count (castPtr ptr::Ptr Word8)
        //                     return ((vertex_datas, index_datas, boundingBoxData)::([Word8], [Word8], [Word8]))
        //                 Binary.encodeFile (getResourceFileName meshPathBuf resourceName resourceExt) contents
        //             return geometryCreateInfos
        //     registMeshData (_meshDataMap resources) meshName geometryCreateInfos
        // where
        //     registMeshData :: MeshDataMap -> String -> [GeometryBuffer.GeometryCreateInfo] -> IO ()
        //     registMeshData meshDataMap meshName geometryCreateInfos = do
        //         geometryBuffer_datas <- forM (zip ([0..]::[Int]) geometryCreateInfos) $ \(index, geometryCreateInfo) -> do
        //             createGeometryBuffer rendererData (Text.append meshName (Text.pack $ show index)) geometryCreateInfo
        //         meshData <- newMeshData meshName geometryBuffer_datas
        //         HashTable.insert (_meshDataMap resources) meshName meshData
    }

    pub fn unload_mesh_datas(&mut self, renderer_data: &RendererData) {
        for mesh_data in self._mesh_data_map.values() {
            for geometry_data in (*mesh_data).borrow().get_geomtry_datas() {
                renderer_data.destroy_geomtry_buffer(geometry_data);
            }
        }
        self._mesh_data_map.clear();
    }

    pub fn get_mesh_data(&self, resource_name: &String) -> &RcRefCell<MeshData> {
        get_resource_data(&self._mesh_data_map, resource_name, DEFAULT_MESH_NAME)
    }

    // TextureLoader
    pub fn load_texture_datas(&mut self, _renderer_data: &RendererData) {
        // texture_datas <- generateTextures rendererData
        // forM_ texture_datas $ \textureData -> do
        //     HashTable.insert (_textureDataMap resources) (_textureDataName textureData) textureData
        //
        // // generate necessary texture datas
        // generateImages rendererData textureSourcePathBuf
        //
        // // load texture from files
        // textureFiles <- walkDirectory textureSourcePathBuf ["jpg", "png", "tga", "bmp"]
        // forM_ textureFiles $ \textureFile -> do
        //     let (textureDataName, cubeTextureFiles) = getTextureDataName textureFiles textureFile
        //         isCubeTexture = not $ null cubeTextureFiles
        //     existingResourceData <- HashTable.lookup (_textureDataMap resources) textureDataName
        //     when (Nothing == existingResourceData) $ do
        //         ((imageWidth, imageHeight, imageData), imageFormat) <-
        //             if isCubeTexture then
        //                 loadImage_datas cubeTextureFiles
        //             else
        //                 loadImageData textureFile
        //         let textureCreateInfo = defaultTextureCreateInfo
        //                 { _textureCreateInfoWidth = fromIntegral imageWidth
        //                 , _textureCreateInfoHeight = fromIntegral imageHeight
        //                 , _textureCreateInfoData = imageData
        //                 , _textureCreateInfoFormat = imageFormat
        //                 , _textureCreateInfoViewType = if isCubeTexture then VK_IMAGE_VIEW_TYPE_CUBE else VK_IMAGE_VIEW_TYPE_2D
        //                 }
        //         textureData <- createTexture rendererData textureDataName textureCreateInfo
        //         HashTable.insert (_textureDataMap resources) textureDataName textureData
        // where
        //     getTextureDataName :: [PathBuf] -> PathBuf -> (String, [String])
        //     getTextureDataName textureFiles textureFile =
        //         let textureDataName = getResourceNameFromPathBuf textureSourcePathBuf textureFile
        //             (textureFileName, _) = Text.breakOnEnd "_" (Text.pack textureFile)
        //             fileExt = Text.pack $ takeExtension textureFile
        //             cubeFaceFiles = [Text.unpack $ Text.concat [textureFileName, face, fileExt] | face <- cubeTextureFaces]
        //             isCubeTexture = ("" /= textureFileName) && (all id [elem filePath textureFiles | filePath <- cubeFaceFiles])
        //             cubeTextureFileName = Text.dropWhileEnd (== '_') textureFileName
        //         in if isCubeTexture then
        //                 (getResourceNameFromPathBuf textureSourcePathBuf $ Text.unpack cubeTextureFileName, cubeFaceFiles)
        //             else
        //                 (textureDataName, [])
        //     loadImageData :: PathBuf -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
        //     loadImageData textureFile = do
        //         imageRawData <- Image.readImage textureFile
        //         case imageRawData of
        //             Left err -> throwVKMsg err
        //             Right dynamicImage ->
        //                 pure $ case dynamicImage of
        //                     Image.ImageRGBA8 image -> (image8ToTuple image, VK_FORMAT_R8G8B8A8_UNORM)
        //                     Image.ImageRGBF image -> (imageFloatToTuple image, VK_FORMAT_R32G32B32_SFLOAT)
        //                     Image.ImageRGBA16 image -> (image16ToTuple image, VK_FORMAT_R16G16B16A16_UNORM)
        //                     otherwise -> (image8ToTuple (Image.convertRGBA8 dynamicImage), VK_FORMAT_R8G8B8A8_UNORM)
        //                     where
        //                         image8ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
        //                             let imageData8 = imageData::SVector.Vector Word8 in (imageWidth, imageHeight, imageData8)
        //                         image16ToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
        //                             let imageData16 = imageData::SVector.Vector Word16 in (imageWidth, imageHeight, SVector.unsafeCast imageData16)
        //                         imageFloatToTuple (Image.Image {imageWidth, imageHeight, imageData}) =
        //                             let imageDataFloat = imageData::SVector.Vector Float in (imageWidth, imageHeight, SVector.unsafeCast imageDataFloat)
        //     loadImage_datas :: [PathBuf] -> IO ((Int, Int, SVector.Vector Word8), VkFormat)
        //     loadImage_datas textureFiles = do
        //         image_datas <- mapM loadImageData textureFiles
        //         return $ flip foldl1 image_datas (\((width, height, accImageData), format) ((_, _, imageData), _) ->
        //             ((width, height, accImageData SVector.++ imageData), format))
    }

    pub fn unload_texture_datas(&mut self, renderer_data: &RendererData) {
        for texture_data in self._texture_data_map.values() {
            renderer_data.destroy_texture(&(*texture_data).borrow());
        }
        self._texture_data_map.clear();
    }

    pub fn get_texture_data(&mut self, resource_name: &String) -> &RcRefCell<TextureData> {
        get_resource_data(&self._texture_data_map, resource_name, DEFAULT_TEXTURE_NAME)
    }

    // Framebuffer
    pub fn load_framebuffer_datas(&mut self, _renderer_data: &RendererData) {
        // renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
        // let frameBufferCreateInfoList = map (\renderPassDataCreateInfo -> (_renderPassCreateInfoName renderPassDataCreateInfo, _renderPassFramebufferCreateInfo renderPassDataCreateInfo)) renderPassDataCreateInfos
        //     frameBufferCreateInfoMap = Map.fromList frameBufferCreateInfoList
        // HashTable.mapM_ (\(k, v) -> registFramebufferData frameBufferCreateInfoMap k) (_renderPassDataMap resources)
        // where
        //     registFramebufferData :: Map.Map String FramebufferDataCreateInfo -> String -> IO ()
        //     registFramebufferData frameBufferCreateInfoMap renderPassName = do
        //         Just renderPassData <- getRenderPassData resources renderPassName
        //         let frameBufferName = _renderPassFramebufferName (renderPassData::RenderPassData)
        //             Just frameBufferDataCreateInfo = Map.lookup frameBufferName frameBufferCreateInfoMap
        //         frameBufferData <- createFramebufferData (getDevice rendererData) (_renderPass renderPassData) frameBufferDataCreateInfo
        //         HashTable.insert (_frameBufferDataMap resources) frameBufferName frameBufferData
    }

    pub fn unload_framebuffer_datas(&mut self, renderer_data: &RendererData) {
        for framebuffer_data in self._framebuffer_data_map.values() {
            framebuffer::destroy_framebuffer_data(renderer_data.get_device(), &(*framebuffer_data).borrow());
        }
        self._framebuffer_data_map.clear();
    }

    pub fn get_framebuffer_data(&self, resource_name: &String) -> &RcRefCell<FramebufferData> {
        get_resource_data(&self._framebuffer_data_map, resource_name, "")
    }

    // RenderPassLoader
    pub fn load_render_pass_datas(&mut self, _renderer_data: &RendererData) {
        // renderPassDataCreateInfos <- RenderPassCreateInfo.getRenderPassDataCreateInfos rendererData
        // mapM_ registRenderPassData renderPassDataCreateInfos
        // where
        //     registRenderPassData renderPassDataCreateInfo = do
        //         descriptor_datas <- forM (_pipelineDataCreateInfos renderPassDataCreateInfo) $ \pipelineDataCreateInfo -> do
        //             getDescriptorData resources rendererData (_renderPassCreateInfoName renderPassDataCreateInfo) pipelineDataCreateInfo
        //         defaultRenderPassData <- create_render_pass_data (getDevice rendererData) renderPassDataCreateInfo descriptor_datas
        //         HashTable.insert (_renderPassDataMap resources) (_renderPassDataName defaultRenderPassData) defaultRenderPassData
    }

    pub fn unload_render_pass_datas(&mut self, renderer_data: &RendererData) {
        for render_pass_data in self._render_pass_data_map.values() {
            render_pass::destroy_render_pass_data(renderer_data.get_device(), &(*render_pass_data).borrow());
        }
        self._render_pass_data_map.clear()
    }

    pub fn get_render_pass_data(&self, resource_name: &String) -> &RcRefCell<RenderPassData> {
        get_resource_data(&self._render_pass_data_map, resource_name, DEFAULT_RENDER_PASS_NAME)
    }

    pub fn get_default_render_pass_data(&self) -> &RcRefCell<RenderPassData> {
        self.get_render_pass_data(&DEFAULT_RENDER_PASS_NAME.to_string())
    }

    pub fn get_render_pass_pipeline_data(&self, render_pass_pipeline_data_name: &RenderPassPipelineDataName) -> RenderPassPipelineData {
        let render_pass_data_refcell = self.get_render_pass_data(&render_pass_pipeline_data_name._render_pass_data_name);
        let render_pass_data = render_pass_data_refcell.borrow();
        let pipeline_data = render_pass_data.get_pipeline_data(&render_pass_pipeline_data_name._pipeline_data_name);
        RenderPassPipelineData {
            _render_pass_data: render_pass_data_refcell.clone(),
             _pipieline_data: pipeline_data.clone(),
        }
    }

    // Material_datas
    pub fn load_material_datas(&mut self, _renderer_data: &RendererData) {
        // materialFiles <- walkDirectory materialPathBuf ["mat"]
        // forM_ materialFiles $ \materialFile -> do
        //     materialName <- getUniqueResourceName (_materialDataMap resources) materialPathBuf materialFile
        //     contents <- ByteString.readFile materialFile
        //     registMaterialData rendererData (_materialDataMap resources) materialName contents
        // where
        //     registMaterialData rendererData materialDataMap materialName contents = do
        //         let Just (Aeson.Object materialCreateInfo) = Aeson.decodeStrict contents
        //             Just (Aeson.Array pipelineCreateInfoArray) = HashMap.lookup "pipelines" materialCreateInfo
        //             Aeson.Object materialParameterMap = HashMap.lookupDefault (Aeson.Object HashMap.empty) "material_parameters" materialCreateInfo
        //         renderPassPipelineDataList <- forM pipelineCreateInfoArray $ \(Aeson.Object pipelineCreateInfo) -> do
        //             let Just (Aeson.String renderPassDataName) = HashMap.lookup "renderPass" pipelineCreateInfo
        //                 Just (Aeson.String pipelineDataName) = HashMap.lookup "pipleline" pipelineCreateInfo
        //             getRenderPassPipelineData resources (renderPassDataName, pipelineDataName)
        //         material <- Material.createMaterial materialName (Vector.toList renderPassPipelineDataList) materialParameterMap
        //         HashTable.insert (_materialDataMap resources) materialName material
    }

    pub fn unload_material_datas(&mut self, _renderer_data: &RendererData) {
        for material_data in self._material_data_map.values() {
            material_data.borrow().destroy_material();
        }
        self._material_data_map.clear();
    }

    pub fn get_material_data(&self, resource_name: &String) -> &RcRefCell<MaterialData> {
        get_resource_data(&self._material_data_map, resource_name, DEFAULT_MATERIAL_NAME)
    }

    // MaterialInstance_datas
    pub fn load_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        // materialInstanceFiles <- walkDirectory materialInstancePathBuf ["matinst"]
        // forM_ materialInstanceFiles $ \materialInstanceFile -> do
        //     materialInstanceName <- getUniqueResourceName (_materialInstanceDataMap resources) materialInstancePathBuf materialInstanceFile
        //     contents <- ByteString.readFile materialInstanceFile
        //     registMaterialInstanceData rendererData (_materialInstanceDataMap resources) materialInstanceName contents
        // where
        //     registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
        //         let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
        //             Just (Aeson.String materialDataName) = HashMap.lookup "material_name" materialInstanceCreateInfoMap
        //             Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap
        //
        //         materialData <- getMaterialData resources materialDataName
        //         let defaultMaterialParameterMap = Material._materialParameterMap materialData
        //         pipelineBindingCreateInfoList <- forM (Map.toList $ Material._renderPassPipelineDataMap materialData) $ \(key, (renderPassData, pipelineData)) -> do
        //             let descriptorDataCreateInfoList = Descriptor._descriptorDataCreateInfoList $ _descriptorData pipelineData
        //             descriptorResourceInfosList <- forM Constants.swapChainImageIndices $ \swapChainIndex -> do
        //                 descriptorResourceInfos <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
        //                     let materialParameterName = Descriptor._descriptorName' descriptorDataCreateInfo
        //                         materialParameterType = Descriptor._descriptorType' descriptorDataCreateInfo
        //                         materialParameterResourceType = Descriptor._descriptorResourceType' descriptorDataCreateInfo
        //                         maybeMaterialParameter = lookupWithDefaultMap materialParameterName materialParameterMap defaultMaterialParameterMap
        //                     case (materialParameterType, materialParameterResourceType) of
        //                         (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, Descriptor.DescriptorResourceType_UniformBuffer) -> do
        //                             uniformBufferData <- getUniformBufferData rendererData (fromText materialParameterName)
        //                             return $ Descriptor.DescriptorBufferInfo (atSwapchainIndex swapChainIndex (_descriptorBufferInfos uniformBufferData))
        //                         (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_Texture) -> do
        //                             textureData <- case maybeMaterialParameter of
        //                                 Just (Aeson.String value) -> getTextureData resources value
        //                                 otherwise -> getTextureData resources defaultTextureName
        //                             return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
        //                         (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
        //                             textureData <- getRenderTarget rendererData (fromText materialParameterName)
        //                             return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
        //                         otherwise -> return Descriptor.InvalidDescriptorInfo
        //                 return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
        //             return (renderPassData, pipelineData, descriptorResourceInfosList)
        //         materialInstance <- MaterialInstance.createMaterialInstance (getDevice rendererData) materialInstanceName materialData pipelineBindingCreateInfoList
        //         HashTable.insert (_materialInstanceDataMap resources) materialInstanceName materialInstance
    }

    pub fn unload_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        for material_instance_data in self._material_instance_data_map.values() {
            (*material_instance_data).borrow().destroy_material_instance();
        }
    }

    pub fn update_material_instance_datas(&mut self, _renderer_data: &RendererData) {
        // flip HashTable.mapM_ (_modelDataMap resources) $ \(k, modelData) -> do
        //     materialInstances <- Model.getMaterialInstanceDataList modelData
        //     newMaterialInstances <- forM materialInstances $ \materialInstance ->
        //         getMaterialInstanceData resources (MaterialInstance._materialInstanceDataName materialInstance)
        //     Model.setMaterialInstanceDataList modelData newMaterialInstances
    }

    pub fn get_material_instance_data(&self, resource_name: &String) ->  &RcRefCell<MaterialInstanceData> {
        get_resource_data(&self._material_instance_data_map, resource_name, DEFAULT_MATERIAL_INSTANCE_NAME)
    }

    // Descriptor_datas
    pub fn get_descriptor_data(
        &self,
        _renderer_data: &RendererData,
        _render_pass_name: &String,
        _pipeline_data_create_info: &PipelineDataCreateInfo
    ) -> RcRefCell<DescriptorData> {
        // let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
        //     descriptorDataCreateInfoList = _descriptorDataCreateInfoList (pipelineDataCreateInfo::PipelineDataCreateInfo)
        //     maxDescriptorPoolCount = Constants.maxDescriptorPoolAllocCount * Constants.descriptorSetCountAtOnce
        // maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resources) descriptorName
        // case maybeDescriptorData of
        //     (Just descriptorData) -> return descriptorData
        //     otherwise -> do
        //         descriptorData <- Descriptor.createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList maxDescriptorPoolCount
        //         HashTable.insert (_descriptorDataMap resources) descriptorName descriptorData
        //         return descriptorData
        system::newRcRefCell(DescriptorData::default())
    }

    pub fn unload_descriptor_datas(&mut self, renderer_data: &RendererData) {
        for descriptor_data in self._descriptor_data_map.values() {
            descriptor::destroy_descriptor_data(renderer_data.get_device(), &(*descriptor_data).borrow());
        }
        self._descriptor_data_map.clear();
    }
}