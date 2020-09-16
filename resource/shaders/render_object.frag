#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outMaterial;
layout(location = 2) out vec4 outNormal;
layout(location = 3) out vec2 outVelocity;


void main() {
    vec4 baseColor = texture(textureBase, vs_output.texCoord);
    baseColor.xyz = pow(baseColor.xyz, vec3(2.2));
    outAlbedo = baseColor * vs_output.color;
    if(outAlbedo.w < 0.333)
    {
        discard;
    }

    outMaterial = texture(textureMaterial, vs_output.texCoord);
    outNormal = texture(textureNormal, vs_output.texCoord);
    outNormal.xyz = normalize(vs_output.tangent_to_world * (outNormal.xyz * 2.0 - 1.0)) * 0.5 + 0.5;
    outVelocity = ((vs_output.projection_pos.xy / vs_output.projection_pos.w) - (vs_output.projection_pos_prev.xy / vs_output.projection_pos_prev.w)) * 0.5;
}
