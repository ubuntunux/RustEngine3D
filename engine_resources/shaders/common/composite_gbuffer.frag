#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"
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
layout(binding = 18) uniform sampler2D textureHiz;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    float depth = texture(textureSceneDepth, vs_output.texCoord).x;
    if(depth == 1.0)
    {
        outColor = vec4(0.0);
        return;
    }

    vec4 base_color = texture(textureSceneAlbedo, vs_output.texCoord);
    float opacity = 1.0;
    vec3 emissive_color = base_color.xyz * decode_emissive_intensity(base_color.w);

    // x: roughness y: metalic, zw: vertex_normal
    vec4 material = texture(textureSceneMaterial, vs_output.texCoord);
    vec4 normal = texture(textureSceneNormal, vs_output.texCoord);

    vec4 relative_position = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, vs_output.texCoord, depth);
    vec3 world_position = relative_position.xyz + view_constants.CAMERA_POSITION;

    float roughness = material.x;
    float metallic = material.y;
    float reflectance = 0.0;
    float shadow_resolved = 1.0;
    {
        vec2 inv_depth_tex_size = 1.0 / textureSize(textureSceneDepth, 0).xy;
        vec2 inv_hiz_tex_size = 1.0 / textureSize(textureHiz, 0).xy;

        const vec2 offsets[9] = {
            vec2(-1.0, -1.0),
            vec2(0.0, -1.0),
            vec2(1.0, -1.0),
            vec2(-1.0, 0.0),
            vec2(0.0, 0.0),
            vec2(1.0, 0.0),
            vec2(-1.0, 1.0),
            vec2(0.0, 1.0),
            vec2(1.0, 1.0)
        };

        float scene_depth = texture(textureSceneDepth, vs_output.texCoord).x;
        int offset_index = 0;
        float closestDepth = 1.0;
        for(int i=0; i<9; ++i)
        {
            float neighborDepth = textureLod(textureHiz, vs_output.texCoord + offsets[i] * inv_hiz_tex_size, 0.0).x;
            float depth_diff = abs(neighborDepth - scene_depth);
            if(depth_diff < closestDepth)
            {
                offset_index = i;
                closestDepth = depth_diff;
            }
        }
        shadow_resolved = texture(textureShadowResolved, vs_output.texCoord - offsets[offset_index] * inv_depth_tex_size).x;
    }
    vec4 scene_reflect_color = texture(textureSceneReflect, vs_output.texCoord);
    vec3 vertexNormal = normalize(vec3(material.z, material.w, normal.w) * 2.0 - 1.0);
    vec3 N = normalize(normal.xyz * 2.0 - 1.0);
    vec3 V = normalize(-relative_position.xyz);

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
        scene_reflect_color,
        texture_probe,
        textureShadow,
        textureHeightMap,
        vs_output.texCoord,
        world_position,
        vertexNormal,
        N,
        V,
        depth,
        true
    );
    outColor.xyz += emissive_color;
    outColor.w = 1.0;
}
