#include "scene_constants.glsl"

struct VERTEX_OUTPUT
{
    vec4 color;
};

struct DebugLineInstanceData {
    vec3 _positions0;
    uint _color;
    vec3 _positions1;
    uint _is_debug_line_3d;
};

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) buffer DebugLineInstanceDataBuffer
{
    DebugLineInstanceData debug_line_data_list[MAX_DEBUG_LINE_INSTANCE_COUNT];
};