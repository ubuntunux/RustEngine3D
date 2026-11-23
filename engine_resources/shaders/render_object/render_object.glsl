// bindings
layout(binding = USER_BINDING_INDEX0) uniform sampler2D textureBase;
layout(binding = USER_BINDING_INDEX1) uniform sampler2D textureMaterial;
layout(binding = USER_BINDING_INDEX2) uniform sampler2D textureNormal;
layout(binding = USER_BINDING_INDEX3) uniform sampler2D textureFace;

// push constant
BEGIN_PUSH_CONSTANT(PushConstant_RenderObject)
    vec4 _face_uv_rect;
END_PUSH_CONSTANT()

#if SHADER_STAGE_FLAG == VERTEX
VERTEX_SHADER_MAIN()
{

}

#elif SHADER_STAGE_FLAG == FRAGMENT
FRAGMENT_SHADER_MAIN()
{
    vec4 base_color = texture(textureBase, in_vertex_output._texCoord);
    vec4 face_uv_rect = GET_PUSH_CONSTANT()._face_uv_rect;
    vec2 face_uv = in_vertex_output._texCoord1;
    if (face_uv_rect.z > 0.0 && face_uv_rect.w > 0.0
        && face_uv.x >= 0.0 && face_uv.x <= 1.0
        && face_uv.y >= 0.0 && face_uv.y <= 1.0)
    {
        vec2 cell_uv = clamp(face_uv, vec2(0.004), vec2(0.996));
        vec2 atlas_uv = face_uv_rect.xy + cell_uv * face_uv_rect.zw;
        vec4 face_color = texture(textureFace, atlas_uv);
        base_color.xyz = mix(base_color.xyz, face_color.xyz, face_color.w);
    }
    out_base_color = base_color * GET_PUSH_CONSTANT_BASE()._color;
    out_base_color.xyz = pow(out_base_color.xyz, vec3(2.2));
    out_material = texture(textureMaterial, in_vertex_output._texCoord);
    out_tangent_normal = texture(textureNormal, in_vertex_output._texCoord).xyz * 2.0 - 1.0;
}
#endif