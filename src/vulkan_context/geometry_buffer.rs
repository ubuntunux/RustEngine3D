use nalgebra::{ Vector2, Vector3, Matrix4 };

use crate::utilities::bounding_box::{ BoundingBox };

#[derive(Debug, Clone)]
pub struct VertexData {
    _position: Vector3<f32>,
    _normal: Vector3<f32>,
    _tangent: Vector3<f32>,
    _color: u32,
    _tex_coord: Vector2<f32>
}

#[derive(Debug, Clone)]
pub struct GeometryCreateInfo {
    _vertices: Vec<VertexData>,
    _indices: Vec<u32>,
    _bounding_box: BoundingBox
}

#[derive(Debug, Clone)]
pub struct GeometryData {
    // _geometryName: Text.Text,
    // _vertexBufferMemory: VkDeviceMemory,
    // _vertexBufferPtr: Ptr VkBuffer,
    // _indexBufferMemory: VkDeviceMemory,
    // _indexBuffer: VkBuffer,
    // _vertexIndexCount: u32,
    // _geometryBoundingBox: BoundingBox
}

impl Default for VertexData {
    fn default() -> VertexData {
        VertexData {
            _position: Vector3::new(0.0, 0.0, 0.0),
            _normal: Vector3::new(0.0, 0.0, 0.0),
            _tangent: Vector3::new(0.0, 0.0, 0.0),
            _color: 0,
            _tex_coord: Vector2::new(0.0, 0.0)
        }
    }
}

// vertexInputBindDescription :: VkVertexInputBindingDescription
// vertexInputBindDescription = createVk @VkVertexInputBindingDescription
//     $  set @"binding" 0
//     &* set @"stride"  (bSizeOf @VertexData undefined)
//     &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX
//
// vertexInputAttributeDescriptions :: DF.Vector VkVertexInputAttributeDescription 5
// vertexInputAttributeDescriptions = ST.runST $ do
//     mv <- ST.newPinnedDataFrame
//     ST.writeDataFrame mv (Idx 0 :* U) . scalar $ createVk
//         $  set @"location" 0
//         &* set @"binding" 0
//         &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
//         &* set @"offset" (bFieldOffsetOf @"_vertex_position" @VertexData undefined)
//     ST.writeDataFrame mv (Idx 1 :* U) . scalar $ createVk
//         $  set @"location" 1
//         &* set @"binding" 0
//         &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
//         &* set @"offset" (bFieldOffsetOf @"_vertex_normal" @VertexData undefined)
//     ST.writeDataFrame mv (Idx 2 :* U) . scalar $ createVk
//         $  set @"location" 2
//         &* set @"binding" 0
//         &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
//         &* set @"offset" (bFieldOffsetOf @"_vertex_tangent" @VertexData undefined)
//     ST.writeDataFrame mv (Idx 3 :* U) . scalar $ createVk
//         $  set @"location" 3
//         &* set @"binding" 0
//         &* set @"format" VK_FORMAT_R8G8B8A8_UNORM
//         &* set @"offset" (bFieldOffsetOf @"_vertex_color" @VertexData undefined)
//     ST.writeDataFrame mv (Idx 4 :* U) . scalar $ createVk
//         $  set @"location" 4
//         &* set @"binding" 0
//         &* set @"format" VK_FORMAT_R32G32_SFLOAT
//         &* set @"offset" (bFieldOffsetOf @"_vertex_tex_coord" @VertexData undefined)
//     ST.unsafeFreezeDataFrame mv
//
//
// createGeometryData :: VkPhysicalDevice
//                    -> VkDevice
//                    -> VkQueue
//                    -> VkCommandPool
//                    -> Text.Text
//                    -> GeometryCreateInfo
//                    -> IO GeometryData
// createGeometryData physicalDevice device graphicsQueue commandPool geometryName geometryCreateInfo = do
//     logInfo $ "createGeometryBuffer : " ++ (Text.unpack geometryName)
//     (vertexBufferMemory, vertexBuffer) <- createVertexBuffer physicalDevice device graphicsQueue commandPool (_geometry_create_info_vertices geometryCreateInfo)
//     (indexBufferMemory, indexBuffer) <- createIndexBuffer physicalDevice device graphicsQueue commandPool (_geometry_create_info_indices geometryCreateInfo)
//     vertexBufferPtr <- new vertexBuffer
//     return GeometryData
//         { _geometryName = geometryName
//         , _vertexBufferMemory = vertexBufferMemory
//         , _vertexBufferPtr = vertexBufferPtr
//         , _indexBufferMemory = indexBufferMemory
//         , _indexBuffer = indexBuffer
//         , _vertexIndexCount = (fromIntegral . SVector.length $ _geometry_create_info_indices geometryCreateInfo)
//         , _geometryBoundingBox = _geometry_create_info_bounding_box geometryCreateInfo
//         }
//
// destroyGeometryData :: VkDevice -> GeometryData -> IO ()
// destroyGeometryData device geometryData@GeometryData {..} = do
//     logInfo "destroyGeometryData"
//     vertexBuffer <- peek _vertexBufferPtr
//     destroyBuffer device vertexBuffer _vertexBufferMemory
//     destroyBuffer device _indexBuffer _indexBufferMemory
//     free _vertexBufferPtr
//
//
// createVertexBuffer :: VkPhysicalDevice
//                    -> VkDevice
//                    -> VkQueue
//                    -> VkCommandPool
//                    -> SVector.Vector VertexData
//                    -> IO (VkDeviceMemory, VkBuffer)
// createVertexBuffer physicalDevice device graphicsQueue commandPool vertices = do
//     let bufferSize = fromIntegral (sizeOf (undefined::VertexData) * (SVector.length vertices))::VkDeviceSize
//         bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
//         memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
//
//     logTrivialInfo $ "createVertexBuffer : bufferSize " ++ show bufferSize
//
//     // create temporary staging buffer
//     let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
//         stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
//     (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags
//
//     // upload data
//     stagingDataPtr <- allocaPeek $ vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
//     SVector.unsafeWith vertices $ \ptr -> do
//         copyBytes (castPtr stagingDataPtr) ptr (fromIntegral bufferSize)
//     vkUnmapMemory device stagingBufferMemory
//
//     // create vertex buffer & copy
//     (vertexBufferMemory, vertexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags
//     copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer bufferSize
//
//     // destroy temporary staging buffer
//     destroyBuffer device stagingBuffer stagingBufferMemory
//
//     return (vertexBufferMemory, vertexBuffer)
//
//
//
// createIndexBuffer :: VkPhysicalDevice
//                   -> VkDevice
//                   -> VkQueue
//                   -> VkCommandPool
//                   -> SVector.Vector u32
//                   -> IO (VkDeviceMemory, VkBuffer)
// createIndexBuffer physicalDevice device graphicsQueue commandPool indices = do
//     let bufferSize = fromIntegral (sizeOf (undefined::u32) * (SVector.length indices))::VkDeviceSize
//         bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
//         memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
//
//     logTrivialInfo $ "createIndexBuffer : bufferSize " ++ show bufferSize
//
//     // create index buffer
//     (indexBufferMemory, indexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags
//
//     // create temporary staging buffer
//     let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
//         stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
//     (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags
//
//     // copy data
//     stagingDataPtr <- allocaPeek $ vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
//     SVector.unsafeWith indices $ \ptr -> do
//         copyBytes (castPtr stagingDataPtr) ptr (fromIntegral bufferSize)
//     vkUnmapMemory device stagingBufferMemory
//     copyBuffer device commandPool graphicsQueue stagingBuffer indexBuffer bufferSize
//
//     // destroy temporary staging buffer
//     destroyBuffer device stagingBuffer stagingBufferMemory
//
//     return (indexBufferMemory, indexBuffer)
//
// {-
//     Note: This point can also be considered as the vector starting from the origin to pi.
//     Writting this equation for the points p1, p2 and p3 give :
//         p1 = u1 * T + v1 * B
//         p2 = u2 * T + v2 * B
//         p3 = u3 * T + v3 * B
//     Texture/World space relation
//
//     With equation manipulation (equation subtraction), we can write :
//         p2 - p1 = (u2 - u1) * T + (v2 - v1) * B
//         p3 - p1 = (u3 - u1) * T + (v3 - v1) * B
//
//     By resolving this system :
//         Equation of Tangent:
//             (v3 - v1) * (p2 - p1) = (v3 - v1) * (u2 - u1) * T + (v3 - v1) * (v2 - v1) * B
//             (v2 - v1) * (p3 - p1) = (v2 - v1) * (u3 - u1) * T + (v2 - v1) * (v3 - v1) * B
//
//         Equation of Binormal:
//             (u3 - u1) * (p2 - p1) = (u3 - u1) * (u2 - u1) * T + (u3 - u1) * (v2 - v1) * B
//             (u2 - u1) * (p3 - p1) = (u2 - u1) * (u3 - u1) * T + (u2 - u1) * (v3 - v1) * B
//
//
//     And we finally have the formula of T and B :
//         T = ((v3 - v1) * (p2 - p1) - (v2 - v1) * (p3 - p1)) / ((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))
//         B = ((u3 - u1) * (p2 - p1) - (u2 - u1) * (p3 - p1)) / -((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))
//
//     Equation of N:
//         N = cross(T, B)
// -}
// computeTangent :: Vector.Vector Vector3<f32> -> Vector.Vector Vector3<f32> -> Vector.Vector Vector2<f32> -> Vector.Vector u32 -> (Vector.Vector Vector3<f32>)
// computeTangent positions normals texcoords indices =
//     let vertexCount = length positions
//         indexCount = length indices
//         tangentList = DList.foldr (\i acc -> DList.append (computeTangent' i positions texcoords normals) acc) DList.empty (DList.fromList [0,3..(indexCount - 1)])
//         tangentMap = Map.fromList $ DList.toList tangentList
//         keys = DList.map (\(index, tangent) -> index) tangentList
//     in
//         Vector.fromList . DList.toList $ DList.map (\key -> fromJust $ Map.lookup key tangentMap) keys
//     where
//         computeTangent' :: Int -> Vector.Vector Vector3<f32> -> Vector.Vector Vector2<f32> -> Vector.Vector Vector3<f32> -> DList.DList (Int, Vector3<f32>)
//         computeTangent' i positions texcoords normals =
//             let i0 = fromIntegral $ indices Vector.! i
//                 i1 = fromIntegral $ indices Vector.! (i + 1)
//                 i2 = fromIntegral $ indices Vector.! (i + 2)
//                 deltaPos_0_1 = (positions Vector.! i1) - (positions Vector.! i0)
//                 deltaPos_0_2 = (positions Vector.! i2) - (positions Vector.! i0)
//                 deltaUV_0_1 = (texcoords Vector.! i1) - (texcoords Vector.! i0)
//                 deltaUV_0_2 = (texcoords Vector.! i2) - (texcoords Vector.! i0)
//                 S r = (deltaUV_0_1 .! Idx 0) * (deltaUV_0_2 .! Idx 1) - (deltaUV_0_1 .! Idx 1) * (deltaUV_0_2 .! Idx 0)
//                 r' = if r /= 0.0 then (1.0 / r) else 0.0
//                 tangent = safeNormalize $ (deltaPos_0_1 * (fromScalar $ deltaUV_0_2 .! Idx 1) - deltaPos_0_2 * (fromScalar $ deltaUV_0_1 .! Idx 1)) * (fromScalar $ scalar r')
//                 avg_normal = safeNormalize $ (normals Vector.! i0 + normals Vector.! i1 + normals Vector.! i2)
//                 resultTangent =
//                     if 0.0 == (dot tangent tangent) then
//                         cross avg_normal world_up
//                     else
//                         tangent
//             in
//                 DList.fromList [(i0, resultTangent), (i1, resultTangent), (i2, resultTangent)]
//
// quadGeometryCreateInfos :: [GeometryCreateInfo]
// quadGeometryCreateInfos =
//     let positionList = [vec3 -1.0 -1.0 0.0, vec3 1.0 -1.0 0.0, vec3 1.0 1.0 0.0, vec3 -1.0 1.0 0.0]
//         positions = Vector.fromList positionList
//         vertexCount = length positions
//         normals = Vector.replicate vertexCount $ vec3 0 1 0
//         vertexColor = getColor32 255 255 255 255
//         texCoords = Vector.fromList [vec2 0 0, vec2 1 0, vec2 1 1, vec2 0 1]
//         indices = Vector.fromList [0, 3, 2, 2, 1, 0] :: Vector.Vector u32
//         tangents = computeTangent positions normals texCoords indices
//         vertices = [VertexData (positions Vector.! i) (normals Vector.! i) (tangents Vector.! i) vertexColor (texCoords Vector.! i) | i <- [0..(vertexCount - 1)]]
//     in
//         [ GeometryCreateInfo
//             { _geometry_create_info_vertices = SVector.fromList vertices
//             , _geometry_create_info_indices = SVector.fromList (Vector.toList indices)
//             , _geometry_create_info_bounding_box = calcBoundingBox positionList
//             }
//         ]
//
// cubeGeometryCreateInfos :: [GeometryCreateInfo]
// cubeGeometryCreateInfos =
//     let vertexColor = getColor32 255 255 255 255
//         positionList = [(vec3 x y z) * 0.5 | (x, y, z) <- [
//             (-1, 1, 1), (-1, -1, 1), (1, -1, 1), (1, 1, 1),
//             (1, 1, 1), (1, -1, 1), (1, -1, -1), (1, 1, -1),
//             (1, 1, -1), (1, -1, -1), (-1, -1, -1), (-1, 1, -1),
//             (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (-1, 1, 1),
//             (-1, 1, -1), (-1, 1, 1), (1, 1, 1), (1, 1, -1),
//             (-1, -1, 1), (-1, -1, -1), (1, -1, -1), (1, -1, 1)]]
//         positions = Vector.fromList positionList
//         normals = Vector.fromList [vec3 x y z | (x, y, z) <- [
//             (0, 0, 1), (0, 0, 1), (0, 0, 1), (0, 0, 1),
//             (1, 0, 0), (1, 0, 0), (1, 0, 0), (1, 0, 0),
//             (0, 0, -1), (0, 0, -1), (0, 0, -1), (0, 0, -1),
//             (-1, 0, 0), (-1, 0, 0), (-1, 0, 0), (-1, 0, 0),
//             (0, 1, 0), (0, 1, 0), (0, 1, 0), (0, 1, 0),
//             (0, -1, 0), (0, -1, 0), (0, -1, 0), (0, -1, 0)]]
//         texCoords = Vector.fromList [vec2 x y | (x, y) <- [
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1),
//             (0, 1), (0, 0), (1, 0), (1, 1)]]
//         indexList = [ 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6, 8, 10, 9, 8, 11, 10, 12, 14, 13, 12, 15, 14, 16, 18, 17, 16, 19, 18, 20, 22, 21, 20, 23, 22 ] :: [u32]
//         indices = Vector.fromList indexList
//         tangents = computeTangent positions normals texCoords indices
//         vertexCount = length positions
//         vertices = [VertexData (positions Vector.! i) (normals Vector.! i) (tangents Vector.! i) vertexColor (texCoords Vector.! i) | i <- [0..(vertexCount - 1)]]
//     in
//         [ GeometryCreateInfo
//             { _geometry_create_info_vertices = SVector.fromList vertices
//             , _geometry_create_info_indices = SVector.fromList indexList
//             , _geometry_create_info_bounding_box = calcBoundingBox positionList
//             }
//         ]