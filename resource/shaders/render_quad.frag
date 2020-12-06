#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D texSampler;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    outColor = texture(texSampler, vs_output.texCoord);
    outColor.w = 1.0;
}
