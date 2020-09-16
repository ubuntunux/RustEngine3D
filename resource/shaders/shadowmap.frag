#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"
#include "utility.glsl"
#include "blending.glsl"

layout(binding = 3) uniform sampler2D textureBase;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out float outColor;

void main() {
    vec4 base_color = texture(textureBase, vs_output.texCoord);
    outColor = gl_FragCoord.z;
}
