#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"
#include "utility.glsl"
#if (RenderMode_Forward == RenderMode)
#include "shading.glsl"
#endif

#if (RenderMode_Forward == RenderMode)
layout(binding = 5) uniform sampler2D textureShadow;
layout(binding = 6) uniform sampler2D textureHeightMap;
layout(binding = 7) uniform samplerCube texture_probe;
layout(binding = 8) uniform sampler2D transmittance_texture;
layout(binding = 9) uniform sampler2D irradiance_texture;
layout(binding = 10) uniform sampler3D scattering_texture;
layout(binding = 11) uniform sampler3D single_mie_scattering_texture;
#endif
layout(binding = 12) uniform sampler2D textureBase;
#if (RenderMode_GBuffer == RenderMode || RenderMode_Forward == RenderMode)
layout(binding = 13) uniform sampler2D textureMaterial;
layout(binding = 14) uniform sampler2D textureNormal;
#endif

layout(location = 0) in VERTEX_OUTPUT vs_output;

#if (RenderMode_GBuffer == RenderMode)
layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outMaterial;
layout(location = 2) out vec4 outNormal;
layout(location = 3) out vec2 outVelocity;
#elif (RenderMode_Forward == RenderMode)
layout(location = 0) out vec4 outColor;
#elif (RenderMode_Shadow == RenderMode || RenderMode_CaptureHeightMap == RenderMode)
layout(location = 0) out float outDepth;
#endif

void main() {
    vec4 base_color = texture(textureBase, vs_output.texCoord);
    base_color.xyz = pow(base_color.xyz, vec3(2.2));
    base_color *= vs_output.color;
    if(base_color.w < 0.333)
    {
        discard;
    }

#if (RenderMode_GBuffer == RenderMode || RenderMode_Forward == RenderMode)
    vec4 material = texture(textureMaterial, vs_output.texCoord);
    vec3 normal = normalize(vs_output.tangent_to_world * (texture(textureNormal, vs_output.texCoord).xyz * 2.0 - 1.0));
    vec3 vertex_normal = normalize(vs_output.tangent_to_world[2]);
#endif

#if (RenderMode_GBuffer == RenderMode)
    outAlbedo = base_color;
    outMaterial.xy = material.xy; // x : roughness, y: metalicness
    outMaterial.zw = vertex_normal.xy * 0.5 + 0.5;
    outNormal.xyz = normal * 0.5 + 0.5;
    outNormal.w = vertex_normal.z * 0.5 + 0.5;
    outVelocity = ((vs_output.projection_pos.xy / vs_output.projection_pos.w) - (vs_output.projection_pos_prev.xy / vs_output.projection_pos_prev.w)) * 0.5;
    outVelocity -= view_constants.JITTER_DELTA;
#elif (RenderMode_Forward == RenderMode)
    vec2 screen_texcoord = (vs_output.projection_pos.xy / vs_output.projection_pos.w) * 0.5 + 0.5;
    float depth = gl_FragCoord.z;
    vec3 world_position = vs_output.relative_position.xyz + view_constants.CAMERA_POSITION;
    float opacity = base_color.w;
    vec3 emissive_color = vec3(0.0);
    float roughness = material.x;
    float metalicness = material.y;
    float reflectance = 0.0;
    float ssao = 1.0;
    vec4 scene_reflect_color = vec4(0.0);
    vec3 V = normalize(-vs_output.relative_position.xyz);
    vec3 L = normalize(light_constants.LIGHT_DIRECTION);

    float sea_diff = world_position.y - scene_constants.SEA_HEIGHT;
    if(sea_diff < SEA_COASTLINE_THICKNESS)
    {
        float sea_ratio = saturate(1.0 - sea_diff / SEA_COASTLINE_THICKNESS) * (1.0 - metalicness);
        sea_ratio = sea_ratio * sea_ratio * 0.9;
        roughness *= (1.0 - sea_ratio);
        base_color.xyz *= (1.0 - sea_ratio);
    }

    outColor = surface_shading(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        irradiance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        scene_constants,
        view_constants,
        light_constants,
        //point_lights,
        base_color.xyz,
        opacity,
        metalicness,
        roughness,
        reflectance,
        ssao,
        scene_reflect_color,
        texture_probe,
        textureShadow,
        textureHeightMap,
        screen_texcoord,
        world_position.xyz,
        light_constants.LIGHT_COLOR.xyz,
        vertex_normal.xyz,
        normal.xyz,
        V,
        L,
        depth
    );
    outColor.xyz += emissive_color;
    outColor.w = 1.0;
#elif (RenderMode_Shadow == RenderMode || RenderMode_CaptureHeightMap == RenderMode)
    outDepth = gl_FragCoord.z;
#endif
}