#include "render_object_common.glsl"

layout( push_constant ) uniform PushConstant_RenderObject
{
    PushConstant_RenderObjectBase _push_constant_base;
} pushConstant;

// bindings
layout(binding = USER_BINDING_INDEX0) uniform sampler2D textureBase;
layout(binding = USER_BINDING_INDEX1) uniform sampler2D textureMaterial;
layout(binding = USER_BINDING_INDEX2) uniform sampler2D textureNormal;

// material functions
PushConstant_RenderObjectBase get_push_constant_base()
{
    return pushConstant._push_constant_base;
}

vec4 get_base_color(in const vec2 texcoord)
{
  return texture(textureBase, texcoord);
}

vec4 get_material(in const vec2 texcoord)
{
    return texture(textureMaterial, texcoord);
}

vec3 get_tangent_normal(in const vec2 texcoord)
{
    return (texture(textureNormal, texcoord).xyz * 2.0 - 1.0);
}

vec3 get_world_offset(in const vec3 vertex_position, in const mat4 local_latrix)
{
    return vec3(0.0);
}