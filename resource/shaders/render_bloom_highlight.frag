#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform sampler2D textureSceneColor;

layout( push_constant ) uniform PushConstant_BloomHighlight
{
    float _bloom_threshold_min;
    float _bloom_threshold_max;
    uint _reserved0;
    uint _reserved1;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    const vec2 texCoord = vs_output.texCoord.xy;
    vec3 color = max(vec3(0.0), texture(textureSceneColor, texCoord).xyz);
    float luminance = get_luminance(color);
    if(pushConstant._bloom_threshold_min < luminance)
    {
        outColor = vec4(color, 1.0);
    }
    else
    {
        outColor = vec4(0.0);
    }
}
