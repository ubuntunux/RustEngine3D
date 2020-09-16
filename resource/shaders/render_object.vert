#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    mat4 localMatrix = pushConstant.localMatrix;
    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;

    vec3 relative_pos = (localMatrix * vec4(inPosition, 1.0)).xyz;
#if (RenderMode_Common == RenderMode)
    vec3 relative_pos_prev = relative_pos + view_constants.CAMERA_POSITION - view_constants.CAMERA_POSITION_PREV;
    vs_output.projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV * vec4(relative_pos_prev, 1.0);
    vs_output.projection_pos = view_constants.VIEW_ORIGIN_PROJECTION * vec4(relative_pos, 1.0);
#elif (RenderMode_Shadow == RenderMode)
    vs_output.projection_pos = light_constants.SHADOW_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#endif
    gl_Position = vs_output.projection_pos;

    vs_output.color = inColor;
    vec3 bitangent = cross(inTangent, inNormal);

    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vs_output.tangent_to_world = mat3(pushConstant.localMatrix) * mat3(inTangent, bitangent, inNormal);

    vs_output.texCoord = inTexCoord;
}
