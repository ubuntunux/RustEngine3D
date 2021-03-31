#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "render_particle_common.glsl"
#include "../utility.glsl"
#include "../shading.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    vec4 base_color = texture(textureBase, vs_output.texCoord);
    base_color.xyz = pow(base_color.xyz, vec3(2.2));
    base_color *= vs_output.color;

    #if (ParticleBlendMode_Opaque == BlendMode)
    if(base_color.w < 0.333)
    {
        discard;
    }
    #endif

    vec4 material = texture(textureMaterial, vs_output.texCoord);
    vec3 normal = normalize(vs_output.tangent_to_world * (texture(textureNormal, vs_output.texCoord).xyz * 2.0 - 1.0));
    vec3 vertex_normal = normalize(vs_output.tangent_to_world[2]);
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

    #if (ParticleBlendMode_AlphaBlend == BlendMode) || (ParticleBlendMode_Additive == BlendMode)
    outColor.xyz = outColor.xyz * outColor.w + emissive_color;
    #endif
}