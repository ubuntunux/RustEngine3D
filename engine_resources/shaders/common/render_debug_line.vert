#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"
#include "render_debug_line_common.glsl"

layout(location = 0) in vec4 inPosition;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    DebugLineInstanceData debug_line_data = debug_line_data_list[gl_InstanceIndex];

    // NOTE: invert y cause convert world to ndc
    if (0 == gl_VertexIndex)
    {
        gl_Position = vec4(debug_line_data._positions0.xyz, 1.0);
    }
    else
    {
        gl_Position = vec4(debug_line_data._positions1.xyz, 1.0);
    }

    if(1 == debug_line_data._is_debug_line_3d)
    {
        gl_Position =  view_constants.PROJECTION * view_constants.VIEW * gl_Position;
    }
    else
    {
        // 2d
        gl_Position.x = (gl_Position.x / scene_constants.SCREEN_SIZE.x) * 2.0 - 1.0;
        gl_Position.y = (gl_Position.y / scene_constants.SCREEN_SIZE.y) * 2.0 - 1.0;
    }

    vs_output.color = uint_color_to_float_color(debug_line_data._color);
}
