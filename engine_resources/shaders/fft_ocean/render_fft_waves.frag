#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/render_quad_common.glsl"
#include "common_fft_ocean.glsl"

layout(binding = 0) uniform sampler2D texture_butterfly;
layout(binding = 1) uniform sampler2DArray texture_fft; // 2 complex inputs (= 4 values) per layer

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 color0;
layout(location = 1) out vec4 color1;
layout(location = 2) out vec4 color2;
layout(location = 3) out vec4 color3;
layout(location = 4) out vec4 color4;

// performs two FFTs on two inputs packed in a single texture
// returns two results packed in a single vec4
vec4 fft2(int layer, vec2 i, vec2 w, vec2 uv)
{
#if defined(RENDER_FFT_X)
    vec4 input1 = textureLod(texture_fft, vec3(i.x, uv.y, layer), 0.0);
    vec4 input2 = textureLod(texture_fft, vec3(i.y, uv.y, layer), 0.0);
#else
    vec4 input1 = textureLod(texture_fft, vec3(uv.x, i.x, layer), 0.0);
    vec4 input2 = textureLod(texture_fft, vec3(uv.x, i.y, layer), 0.0);
#endif
    float res1x = w.x * input2.x - w.y * input2.y;
    float res1y = w.y * input2.x + w.x * input2.y;
    float res2x = w.x * input2.z - w.y * input2.w;
    float res2y = w.y * input2.z + w.x * input2.w;
    return input1 + vec4(res1x, res1y, res2x, res2y);
}

void main()
{
    vec2 uv = vs_output.texCoord;
#if defined(RENDER_FFT_X)
    vec4 data = textureLod(texture_butterfly, vec2(uv.x, pushConstant._pass), 0.0);
#else
    vec4 data = textureLod(texture_butterfly, vec2(uv.y, pushConstant._pass), 0.0);
#endif
    vec2 i = data.xy;
    vec2 w = data.zw;

    color0 = fft2(0, i, w, uv);
    color1 = fft2(1, i, w, uv);
    color2 = fft2(2, i, w, uv);
    color3 = fft2(3, i, w, uv);
    color4 = fft2(4, i, w, uv);
}