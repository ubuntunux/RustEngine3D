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
    out_base_color = texture(textureBase, in_vertex_output._texCoord);
    out_base_color.xyz = pow(out_base_color.xyz, vec3(2.2));
    out_material = texture(textureMaterial, in_vertex_output._texCoord);
    out_tangent_normal = texture(textureNormal, in_vertex_output._texCoord).xyz * 2.0 - 1.0;
}
#endif