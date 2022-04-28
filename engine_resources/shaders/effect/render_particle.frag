#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "render_particle_common.glsl"
#include "../common/utility.glsl"
#include "../common/shading.glsl"

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

    // x: roughness, y: metalicness, z: emissive intensity
    vec4 material = texture(textureMaterial, vs_output.texCoord);
    vec3 normal = normalize(vs_output.tangent_to_world * (texture(textureNormal, vs_output.texCoord).xyz * 2.0 - 1.0));
    vec3 vertex_normal = normalize(vs_output.tangent_to_world[2]);
    vec2 screen_texcoord = (vs_output.projection_pos.xy / vs_output.projection_pos.w) * 0.5 + 0.5;
    float depth = gl_FragCoord.z;
    vec3 world_position = vs_output.relative_position.xyz + view_constants.CAMERA_POSITION;
    float opacity = base_color.w;
    vec3 emissive_color = base_color.xyz * material.z;
    float roughness = material.x;
    float metalicness = material.y;
    float reflectance = 0.0;
    float ssao = 1.0;
    vec4 scene_reflect_color = vec4(0.0);
    vec3 V = normalize(-vs_output.relative_position.xyz);

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
        vertex_normal.xyz,
        normal.xyz,
        V,
        depth
    );

    #if (ParticleBlendMode_AlphaBlend == BlendMode) || (ParticleBlendMode_Additive == BlendMode)
    outColor.xyz = outColor.xyz * outColor.w + emissive_color;
    #endif
}