#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "render_quad_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    gl_Position = vec4(inPosition, 1.0);
    vs_output.vertexColor = inColor;
    vs_output.vertexNormal = inNormal;
    vs_output.texCoord = inTexCoord;
}
