#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureSrc;

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
    const vec2 inv_texture_size = 1.0f / vec2(textureSize(textureSrc, 0));
    vec3 color0 = max(vec3(0.0), texture(textureSrc, texCoord).xyz);
    vec3 color1 = max(vec3(0.0), texture(textureSrc, texCoord + vec2(inv_texture_size.x, 0.0)).xyz);
    vec3 color2 = max(vec3(0.0), texture(textureSrc, texCoord + vec2(0.0, inv_texture_size.y)).xyz);
    vec3 color3 = max(vec3(0.0), texture(textureSrc, texCoord + inv_texture_size).xyz);
    vec3 color = (color0 + color1 + color2 + color3) * 0.25;
    float luminance = get_luminance(color);
    // Soft-knee HDR bloom extraction so only genuine bright highlights (sun/sky/emissives) bloom
    const float threshold = max(pushConstant._bloom_threshold_min, 1.6);
    const float knee = 0.5;
    float soft = clamp(luminance - threshold + knee, 0.0, 2.0 * knee);
    soft = (soft * soft) / (4.0 * knee + 0.00001);
    float contribution = max(soft, luminance - threshold) / max(luminance, 0.00001);
    outColor = vec4(color * saturate(contribution), 1.0);
}
