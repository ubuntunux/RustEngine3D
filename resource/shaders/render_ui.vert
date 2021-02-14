#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in vec3 vs_in_position;

layout (location = 0) out VERTEX_OUTPUT vs_output;

void main()
{
    vec4 ui_infos = ui_instance_data[gl_InstanceIndex]._ui_instance_infos;
    vec2 ratio = pushConstant._ui_size * pushConstant._inv_canvas_size;

    vs_output.texcoord = vs_in_position.xy + 0.5;

    vec2 position = (pushConstant._ui_pos + vs_in_position.xy * pushConstant._ui_size) * pushConstant._inv_canvas_size;

    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
}