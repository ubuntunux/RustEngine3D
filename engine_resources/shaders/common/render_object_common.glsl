#include "scene_constants.glsl"
#include "utility.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"
#include "shading.glsl"

struct VERTEX_OUTPUT
{
    mat3 tangent_to_world;
    vec4 color;
    vec2 texCoord;
    vec3 relative_position;
    vec4 projection_pos;
    vec4 projection_pos_prev;
};

struct PushConstant_RenderObjectBase
{
    uint _transform_offset_index;
    uint _bone_count;
    uint _reserved0;
    uint _reserved1;
    vec4 _color;
};

// material functions
PushConstant_RenderObjectBase get_push_constant_base();
vec4 get_base_color(in const vec2 texcoord);
vec4 get_material(in const vec2 texcoord);
vec3 get_tangent_normal(in const vec2 texcoord);
vec3 get_world_offset(in const vec3 vertex_position, in const mat4 local_latrix);

// bindings
layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightData
{
    LIGHT_DATA light_data;
};
layout(binding = 3) uniform PointLights
{
    POINT_LIGHTS point_lights;
};
layout(binding = 4) buffer TransformMatrices
{
    mat4 transform_matrices[MAX_TRANSFORM_COUNT];
};
layout(binding = 5) buffer TransformOffsets
{
    // x: common transform index, y: transform index for shadow, z: transform index for height map
    ivec4 transform_offsets[MAX_TRANSFORM_COUNT];
};
layout(binding = 6) uniform AtmosphereConstants
{
    ATMOSPHERE_CONSTANTS atmosphere_constants;
};
layout(binding = 7) uniform sampler2D textureShadow;
layout(binding = 8) uniform sampler2D textureHeightMap;
layout(binding = 9) uniform samplerCube texture_probe;
layout(binding = 10) uniform sampler2D transmittance_texture;
layout(binding = 11) uniform sampler2D irradiance_texture;
layout(binding = 12) uniform sampler3D scattering_texture;
layout(binding = 13) uniform sampler3D single_mie_scattering_texture;

#define USER_BINDING_INDEX0 14
#define USER_BINDING_INDEX1 15
#define USER_BINDING_INDEX2 16
#define USER_BINDING_INDEX3 17
#define USER_BINDING_INDEX4 18
#define USER_BINDING_INDEX5 19
#define USER_BINDING_INDEX6 20
#define USER_BINDING_INDEX7 21
#define USER_BINDING_INDEX8 22
#define USER_BINDING_INDEX9 23