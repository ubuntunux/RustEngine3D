#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout(location = 0) out vec4 vertexColor;
layout(location = 1) out vec3 vertexNormal;
layout(location = 2) out vec2 texCoord;

void main() {
    gl_Position = vec4(inPosition, 1.0);
    vertexColor = inColor;
    vertexNormal = inNormal;
    texCoord = inTexCoord;
}
