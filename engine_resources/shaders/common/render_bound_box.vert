#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "render_bound_box_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;
layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    // cube vertex 0.5 -> 1.0
    vec4 position = vec4(inPosition * 2.0, 1.0);

    mat4 localMatrix = bound_box_instance_data[gl_InstanceIndex]._transform;
    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;

    vec3 relative_pos = (localMatrix * position).xyz;

    gl_Position = view_constants.VIEW_ORIGIN_PROJECTION * vec4(relative_pos, 1.0);
    vs_output.color = inColor;
}
