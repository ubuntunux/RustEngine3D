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

#if (RenderMode_Common == RenderMode)
layout(binding = 3) uniform sampler2D textureBase;
layout(binding = 4) uniform sampler2D textureMaterial;
layout(binding = 5) uniform sampler2D textureNormal;
#endif

layout( push_constant ) uniform PushConstant
{
    mat4 localMatrix;
} pushConstant;

struct VERTEX_OUTPUT
{
    mat3 tangent_to_world;
    vec4 color;
    vec2 texCoord;
    vec4 projection_pos;
    vec4 projection_pos_prev;
};