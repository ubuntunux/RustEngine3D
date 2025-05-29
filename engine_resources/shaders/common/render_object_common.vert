void main() {
    vec4 position = vec4(0.0);
    vec4 prev_position = vec4(0.0);
    vec3 vertex_normal = vec3(0.0);
    vec3 vertex_tangent = vec3(0.0);

    const uint transform_offset_index = GET_PUSH_CONSTANT_BASE()._transform_offset_index + gl_InstanceIndex;
#if (RenderMode_CaptureHeightMap == RenderMode)
    const uint transform_matrix_offset = transform_offsets[transform_offset_index].z;
#elif (RenderMode_Shadow == RenderMode)
    const uint transform_matrix_offset = transform_offsets[transform_offset_index].y;
#else
    const uint transform_matrix_offset = transform_offsets[transform_offset_index].x;
#endif

#if (RenderObjectType_Skeletal == RenderObjectType)
    const uint local_matrix_prev_offset = transform_matrix_offset;
    const uint local_matrix_offset = local_matrix_prev_offset + 1;
    const uint prev_bone_matrix_offset = local_matrix_offset + 1;
    const uint bone_matrix_offset = prev_bone_matrix_offset + GET_PUSH_CONSTANT_BASE()._bone_count;

    if (0 < GET_PUSH_CONSTANT_BASE()._bone_count)
    {
        for(int i = 0; i < MAX_BONES_PER_VERTEX; ++i)
        {
            const float boneWeight = inBoneWeights[i];
            if(0.0 < boneWeight)
            {
                prev_position += (transform_matrices[prev_bone_matrix_offset + int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * boneWeight;
                position += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * boneWeight;
                vertex_normal += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inNormal, 0.0)).xyz * boneWeight;
                vertex_tangent += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inTangent, 0.0)).xyz * boneWeight;
            }
        }
        position /= position.w;
        prev_position /= prev_position.w;
        vertex_normal = normalize(vertex_normal);
        vertex_tangent = normalize(vertex_tangent);
    }
    else
    {
        position = vec4(inPosition, 1.0);
        prev_position = vec4(inPosition, 1.0);
        vertex_normal = normalize(inNormal);
        vertex_tangent = normalize(inTangent);
    }
#else
    // RenderObjectType_Static
    const uint local_matrix_prev_offset = transform_matrix_offset; // static mesh can't move
    const uint local_matrix_offset = transform_matrix_offset;
    position = vec4(inPosition, 1.0);
    prev_position = vec4(inPosition, 1.0);
    vertex_normal = normalize(inNormal);
    vertex_tangent = normalize(inTangent);
#endif

    mat4 localMatrix = transform_matrices[local_matrix_offset];
    mat4 localMatrixPrev = transform_matrices[local_matrix_prev_offset];

    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;
    localMatrixPrev[3].xyz -= view_constants.CAMERA_POSITION_PREV;

    vec3 relative_pos = (localMatrix * position).xyz;
    vec3 relative_pos_prev = (localMatrixPrev * prev_position).xyz;

    // Begin: VertexShaderMain
    vec3 world_offset = vec3(0.0, 0.0, 0.0);
    vertex_shader_main(
        relative_pos,
        localMatrix,
        world_offset
    );
    // End: VertexShaderMain

    relative_pos += world_offset;
    relative_pos_prev += world_offset;

#if (RenderMode_DepthPrepass == RenderMode || RenderMode_GBuffer == RenderMode || RenderMode_Forward == RenderMode)
    out_vertex_output._projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV_JITTER * vec4(relative_pos_prev, 1.0);
    out_vertex_output._projection_pos = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(relative_pos, 1.0);
#elif (RenderMode_Shadow == RenderMode)
    out_vertex_output._projection_pos = light_data.SHADOW_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#elif (RenderMode_CaptureHeightMap == RenderMode)
    out_vertex_output._projection_pos = view_constants.CAPTURE_HEIGHT_MAP_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#endif
    gl_Position = out_vertex_output._projection_pos;

    out_vertex_output._relative_position = relative_pos;
    out_vertex_output._color = inColor;
    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vec3 bitangent = cross(vertex_tangent, vertex_normal);
    out_vertex_output._tangent_to_world = mat3(localMatrix) * mat3(vertex_tangent, bitangent, vertex_normal);
    out_vertex_output._texCoord = inTexCoord;
}
