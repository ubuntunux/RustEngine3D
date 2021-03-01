#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureSrc;

layout( push_constant ) uniform PushConstant_RenderCopy
{
    uint _taget_mip_level;
    uint _reserved0;
    uint _reserved1;
    uint _reserved2;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    const vec2 texCoord = vs_output.texCoord.xy;
    outColor = textureLod(textureSrc, texCoord, float(pushConstant._taget_mip_level));
}
