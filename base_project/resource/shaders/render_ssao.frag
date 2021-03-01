#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "PCFKernels.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightConstants
{
    LIGHT_CONSTANTS light_constants;
};
layout(binding = 3) uniform sampler2D textureSceneNormal;
layout(binding = 4) uniform sampler2D textureSceneDepth;
layout(binding = 5) uniform sampler2D ssaoNoise;

// uniform_buffer_data - struct SSAOConstants
layout(binding = 6) uniform SSAOConstants
{
    vec4 _SSAO_KERNEL_SAPLES[SSAO_KERNEL_SIZE];
} uboSSAOKernel;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out float outColor;

void main() {
    const vec2 texCoord = vs_output.texCoord;
    const float device_depth = texture(textureSceneDepth, texCoord).x;
    if(1.0 == device_depth)
    {
        discard;
    }

    const vec4 relative_pos = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, texCoord, device_depth);
    const vec3 normal = normalize(texture(textureSceneNormal, texCoord).xyz * 2.0 - 1.0);
    const vec2 texture_size = textureSize(textureSceneDepth, 0);
    const vec2 noise_size = textureSize(ssaoNoise, 0);
    const vec3 randomVec = normalize(vec3(texture(ssaoNoise, texCoord * texture_size / noise_size).xy, 0.0).xzy);

    vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
    const vec3 bitangent = normalize(cross(normal, tangent));
    const mat3 tnb = mat3(tangent, normal, bitangent);
    const float occlusion_distance_min = 0.1;
    const float occlusion_distance_max = 2.0;

    const int sample_count = 32;//SSAO_KERNEL_SIZE;
    float occlusion = 0.0;
    for (int i = 0; i < sample_count; ++i)
    {
        const float occlusion_distance = mix(occlusion_distance_min, occlusion_distance_max, float(i) / float(sample_count - 1));
        vec3 pos = (tnb * uboSSAOKernel._SSAO_KERNEL_SAPLES[i].xyz) * occlusion_distance + relative_pos.xyz;

        // project sample position:
        vec4 offset = vec4(pos, 1.0);
        offset = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * offset;
        offset.xyz /= offset.w;
        offset.xy = offset.xy * 0.5 + 0.5;

        if(offset.x < 0.0 || offset.x > 1.0 || offset.y < 0.0 || offset.y > 1.0)
        {
            continue;
        }

        const vec3 target_normal = normalize(texture(textureSceneNormal, offset.xy).xyz * 2.0 - 1.0);
        if(0.99 < dot(target_normal, normal))
        {
            continue;
        }

        const float occlusion_depth = texture(textureSceneDepth, offset.xy).x;
        if(offset.z <= occlusion_depth)
        {
            continue;
        }

        const vec4 occlusion_relative_pos = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, offset.xy, occlusion_depth);
        const float distance = length(occlusion_relative_pos - relative_pos);
        occlusion += exp(-distance);
    }

    outColor = saturate(exp(-occlusion * 0.5));
}