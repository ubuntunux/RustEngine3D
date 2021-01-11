#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

#if (RenderMode_Forward == RenderMode)
layout(binding = 5) uniform sampler2D texture_shadow;
layout(binding = 6) uniform samplerCube texture_probe;
layout(binding = 7) uniform sampler2D ibl_brdf_lut;
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
#elif (RenderMode_Shadow == RenderMode)
layout(location = 0) out float outDepth;
#endif


void main() {
#if (RenderMode_GBuffer == RenderMode)
    vec4 baseColor = texture(textureBase, vs_output.texCoord);
    baseColor.xyz = pow(baseColor.xyz, vec3(2.2));
    outAlbedo = baseColor * vs_output.color;
    if(outAlbedo.w < 0.333)
    {
        discard;
    }

    vec3 normal = normalize(vs_output.tangent_to_world * (texture(textureNormal, vs_output.texCoord).xyz * 2.0 - 1.0)) * 0.5 + 0.5;
    vec3 vertexNormal = normalize(vs_output.tangent_to_world[2]) * 0.5 + 0.5;

    // x : roughness, y: metalicness
    outMaterial.xy = texture(textureMaterial, vs_output.texCoord).xy;
    outMaterial.zw = vertexNormal.xy;
    outNormal.xyz = normal;
    outNormal.w = vertexNormal.z;
    outVelocity = ((vs_output.projection_pos.xy / vs_output.projection_pos.w) - (vs_output.projection_pos_prev.xy / vs_output.projection_pos_prev.w)) * 0.5;
    outVelocity -= view_constants.JITTER_DELTA;
#elif (RenderMode_Forward == RenderMode)
    vec4 base_color = texture(textureBase, vs_output.texCoord);
    outColor = base_color;
#elif (RenderMode_Shadow == RenderMode)
    vec4 base_color = texture(textureBase, vs_output.texCoord);
    outDepth = gl_FragCoord.z;
#endif
}