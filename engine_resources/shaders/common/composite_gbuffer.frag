#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"
#define ENABLE_DEFERRED_LIGHTING_AND_FOG
#include "shading.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightData
{
    LIGHT_DATA light_data;
};
layout(binding = 3) uniform PointLights
{
    POINT_LIGHTS point_lights;
};
layout(binding = 4) uniform AtmosphereConstants
{
    ATMOSPHERE_CONSTANTS atmosphere_constants;
};
layout(binding = 5) uniform sampler2D textureSceneAlbedo;
layout(binding = 6) uniform sampler2D textureSceneMaterial;
layout(binding = 7) uniform sampler2D textureSceneNormal;
layout(binding = 8) uniform sampler2D textureSceneDepth;
layout(binding = 9) uniform sampler2D textureShadowResolved;
layout(binding = 10) uniform sampler2D textureShadow;
layout(binding = 11) uniform sampler2D textureHeightMap;
layout(binding = 12) uniform samplerCube texture_probe;
layout(binding = 13) uniform sampler2D textureSceneReflect;
layout(binding = 14) uniform sampler2D transmittance_texture;
layout(binding = 15) uniform sampler2D irradiance_texture;
layout(binding = 16) uniform sampler3D scattering_texture;
layout(binding = 17) uniform sampler3D single_mie_scattering_texture;
layout(binding = 18) uniform sampler2D textureSceneGI;
layout(binding = 19) uniform LocalLightingData
{
    LOCAL_LIGHTING_DATA local_lighting_data;
};
layout(binding = 20) uniform sampler3D textureVolumetricFog;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    float depth = texture(textureSceneDepth, vs_output.texCoord).x;
    if(depth == 0.0)
    {
        outColor = vec4(0.0);
        return;
    }

    vec4 base_color = texture(textureSceneAlbedo, vs_output.texCoord);
    float opacity = 1.0;
    vec3 emissive_color = base_color.xyz * decode_emissive_intensity(base_color.w);

    // x: roughness, y: metallic, zw: octahedral vertex_normal
    vec4 material = texture(textureSceneMaterial, vs_output.texCoord);
    // xyz: world normal, w: encoded shading_model
    vec4 normal = texture(textureSceneNormal, vs_output.texCoord);

    vec4 relative_position = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, vs_output.texCoord, depth);
    vec3 world_position = relative_position.xyz + view_constants.CAMERA_POSITION;

    float roughness = material.x;
    float metallic = material.y;
    float reflectance = 0.0;
    vec3 vertexNormal = oct_decode_normal(material.zw);
    vec3 N = normalize(normal.xyz * 2.0 - 1.0);
    vec3 V = normalize(-relative_position.xyz);

    float shadow_resolved = 1.0;
    float ao_resolved = 1.0;
    vec3 bent_normal = N;
    const bool has_shadow_option = (scene_constants.RENDER_OPTION & RenderOption_RenderShadow) != 0u;
    const bool has_ssao_option = (scene_constants.RENDER_OPTION & RenderOption_RenderSSAO) != 0u;
    if (has_shadow_option || has_ssao_option)
    {
        vec4 shadow_ao_center = textureLod(textureShadowResolved, vs_output.texCoord, 0.0);
        // 5-tap depth-aware bilateral filter on ShadowAO.xy removes PCSS/Contact/GTAO dither while preserving sharp edges
        vec2 inv_shadow_ao_size = 1.0 / max(vec2(textureSize(textureShadowResolved, 0)), vec2(1.0));
        float center_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, depth);
        float inv_depth_sigma = 1.0 / max(0.035 * center_linear, 0.04);

        vec2 filtered_xy = shadow_ao_center.xy * 2.0;
        float weight_sum = 2.0;
        const vec2 offsets[4] = vec2[4](
            vec2(-1.0,  0.0),
            vec2( 1.0,  0.0),
            vec2( 0.0, -1.0),
            vec2( 0.0,  1.0)
        );
        for (int i = 0; i < 4; ++i)
        {
            vec2 tap_uv = clamp(vs_output.texCoord + offsets[i] * inv_shadow_ao_size, vec2(0.0), vec2(1.0));
            float tap_depth = textureLod(textureSceneDepth, tap_uv, 0.0).x;
            if (tap_depth > 0.0)
            {
                float tap_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, tap_depth);
                float depth_delta = abs(tap_linear - center_linear) * inv_depth_sigma;
                float w = exp(-depth_delta * depth_delta);
                vec2 tap_xy = textureLod(textureShadowResolved, tap_uv, 0.0).xy;
                filtered_xy += tap_xy * w;
                weight_sum += w;
            }
        }
        filtered_xy /= max(weight_sum, 1e-5);
        shadow_resolved = has_shadow_option ? filtered_xy.x : 1.0;
        ao_resolved = has_ssao_option ? filtered_xy.y : 1.0;
        if (has_ssao_option)
        {
            bent_normal = normalize(oct_decode_normal(shadow_ao_center.zw));
        }
    }
    uint shading_model = decode_shading_model(normal.w);

    // Depth- and normal-aware 5-tap joint bilateral spatial denoise/upsample for half-res SSR and SSGI
    float center_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, depth);
    float inv_bilateral_depth_sigma = 1.0 / max(0.06 * center_linear_depth, 0.08);
    const vec2 bilateral_offsets[4] = vec2[4](
        vec2(-1.0,  0.0),
        vec2( 1.0,  0.0),
        vec2( 0.0, -1.0),
        vec2( 0.0,  1.0)
    );

    vec4 scene_reflect_color = vec4(0.0);
    if ((scene_constants.RENDER_OPTION & RenderOption_RenderSSR) != 0u)
    {
        vec4 ssr_center = textureLod(textureSceneReflect, vs_output.texCoord, 0.0);
        vec2 inv_ssr_size = 1.0 / max(vec2(textureSize(textureSceneReflect, 0)), vec2(1.0));
        vec4 ssr_sum = ssr_center * 2.0;
        float ssr_weight = 2.0;
        for (int i = 0; i < 4; ++i)
        {
            vec2 tap_uv = clamp(vs_output.texCoord + bilateral_offsets[i] * inv_ssr_size, vec2(0.0), vec2(1.0));
            float tap_depth = textureLod(textureSceneDepth, tap_uv, 0.0).x;
            if (tap_depth > 0.0)
            {
                float tap_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, tap_depth);
                float dd = abs(tap_linear - center_linear_depth) * inv_bilateral_depth_sigma;
                vec3 tap_N = normalize(textureLod(textureSceneNormal, tap_uv, 0.0).xyz * 2.0 - 1.0);
                float nw = pow(saturate(dot(N, tap_N)), 12.0);
                float w = exp(-dd * dd) * nw;
                ssr_sum += textureLod(textureSceneReflect, tap_uv, 0.0) * w;
                ssr_weight += w;
            }
        }
        scene_reflect_color = max(ssr_sum / max(ssr_weight, 1e-5), vec4(0.0));
    }

    vec4 scene_gi_color = vec4(0.0);
    if ((scene_constants.RENDER_OPTION & RenderOption_RenderSSGI) != 0u)
    {
        vec4 gi_center = textureLod(textureSceneGI, vs_output.texCoord, 0.0);
        vec2 inv_gi_size = 1.0 / max(vec2(textureSize(textureSceneGI, 0)), vec2(1.0));
        vec4 gi_sum = gi_center * 1.5;
        float gi_weight = 1.5;
        for (int i = 0; i < 4; ++i)
        {
            vec2 tap_uv = clamp(vs_output.texCoord + bilateral_offsets[i] * inv_gi_size * 1.5, vec2(0.0), vec2(1.0));
            float tap_depth = textureLod(textureSceneDepth, tap_uv, 0.0).x;
            if (tap_depth > 0.0)
            {
                float tap_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, tap_depth);
                float dd = abs(tap_linear - center_linear_depth) * inv_bilateral_depth_sigma;
                vec3 tap_N = normalize(textureLod(textureSceneNormal, tap_uv, 0.0).xyz * 2.0 - 1.0);
                float nw = pow(saturate(dot(N, tap_N)), 6.0);
                float w = exp(-dd * dd) * nw;
                gi_sum += textureLod(textureSceneGI, tap_uv, 0.0) * w;
                gi_weight += w;
            }
        }
        scene_gi_color = max(gi_sum / max(gi_weight, 1e-5), vec4(0.0));
    }
    const bool IS_COMBINED_SHADOW = true;

    outColor = surface_shading(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        irradiance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        scene_constants,
        view_constants,
        light_data,
        point_lights,
        base_color.xyz,
        opacity,
        metallic,
        roughness,
        reflectance,
        shadow_resolved,
        ao_resolved,
        shading_model,
        scene_reflect_color,
        scene_gi_color,
        texture_probe,
        textureShadow,
        textureHeightMap,
        vs_output.texCoord,
        world_position,
        vertexNormal,
        N,
        bent_normal,
        V,
        depth,
        IS_COMBINED_SHADOW,
        local_lighting_data,
        textureSceneDepth,
        textureVolumetricFog
    );
    outColor.xyz += emissive_color;
    outColor.w = 1.0;
}
