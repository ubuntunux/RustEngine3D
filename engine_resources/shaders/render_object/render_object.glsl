// bindings
layout(binding = USER_BINDING_INDEX0) uniform sampler2D textureBase;
layout(binding = USER_BINDING_INDEX1) uniform sampler2D textureMaterial;
layout(binding = USER_BINDING_INDEX2) uniform sampler2D textureNormal;

// push constant
BEGIN_PUSH_CONSTANT(PushConstant_RenderObject)
END_PUSH_CONSTANT()

#if SHADER_STAGE_FLAG == VERTEX
VERTEX_SHADER_MAIN()
{

}

#elif SHADER_STAGE_FLAG == FRAGMENT
FRAGMENT_SHADER_MAIN()
{
    vec4 tex_color = texture(textureBase, in_vertex_output._texCoord);
    vec4 tint_color = GET_PUSH_CONSTANT_BASE()._color;
    out_base_color.xyz = pow(tex_color.xyz, vec3(2.2)) * pow(tint_color.xyz, vec3(2.2));
    out_base_color.w = tex_color.w * tint_color.w;
    out_material = texture(textureMaterial, in_vertex_output._texCoord);
#if (RenderObjectType_Skeletal == RenderObjectType)
    out_material.x = clamp(out_material.x, 0.05, 0.65);
    out_material.w = encode_shading_model(ShadingModel_Character);
#else
    out_material.x = clamp(out_material.x, 0.05, 0.82);
    out_material.w = encode_shading_model(ShadingModel_Default);
#endif
    out_tangent_normal = texture(textureNormal, in_vertex_output._texCoord).xyz * 2.0 - 1.0;
}
#endif