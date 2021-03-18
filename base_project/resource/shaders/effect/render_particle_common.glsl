#include "effect_constants.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"

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
layout(binding = 3) uniform AtmosphereConstants
{
    ATMOSPHERE_CONSTANTS atmosphere_constants;
};

layout( push_constant ) uniform PushConstant_RenderParticle
{
    mat4 _localMatrix;
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