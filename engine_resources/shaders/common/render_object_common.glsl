#if (RenderMode_Forward == RenderMode)
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"
#endif

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightConstants
{
    LIGHT_CONSTANTS light_constants;
};
layout(binding = 3) buffer TransformMetrices
{
    mat4 transform_matrices[MAX_TRANSFORM_COUNT];
};
#if (RenderMode_Forward == RenderMode)
layout(binding = 4) uniform AtmosphereConstants
{
    ATMOSPHERE_CONSTANTS atmosphere_constants;
};
#endif

layout( push_constant ) uniform PushConstant_RenderObject
{
    uint _transform_matrix_offset;
    uint _bone_count;
    uint _reserved0;
    uint _reserved1;
    vec4 _color;
} pushConstant;

struct VERTEX_OUTPUT
{
    mat3 tangent_to_world;
    vec4 color;
    vec2 texCoord;
    vec3 relative_position;
    vec4 projection_pos;
    vec4 projection_pos_prev;
};