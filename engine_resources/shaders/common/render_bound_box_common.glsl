#include "scene_constants.glsl"

const uint MAX_BOUND_BOX_INSTANCE_COUNT = 1024;

struct VERTEX_OUTPUT
{
    vec4 color;
};

struct BoundBoxInstanceData {
    mat4 _transform;
};

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};

layout(binding = 2) buffer BoundBoxInstanceDataBuffer
{
    BoundBoxInstanceData bound_box_instance_data[MAX_BOUND_BOX_INSTANCE_COUNT];
};