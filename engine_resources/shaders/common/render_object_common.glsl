#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"
#include "../common/shading.glsl"

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
#define GET_PUSH_CONSTANT_BASE() get_push_constant_base()
#define IMPL_GET_PUSH_CONSTANT_BASE() PushConstant_RenderObjectBase GET_PUSH_CONSTANT_BASE()

#define INITALIZE_USER_DATA() initialize_user_data()
#define IMPL_INITALIZE_USER_DATA() void initialize_user_data()

#define GET_BASE_COLOR() get_base_color()
#define IMPL_GET_BASE_COLOR() vec4 get_base_color()

#define GET_MATERIAL() get_material()
#define IMPL_GET_MATERIAL() vec4 get_material()

#define GET_TANGENT_NORMAL() get_tangent_normal()
#define IMPL_GET_TANGENT_NORMAL() vec3 get_tangent_normal()

#define GET_WORLD_OFFSET(relative_position, local_latrix) get_world_offset(relative_position, local_latrix)
#define IMPL_GET_WORLD_OFFSET() vec3 get_world_offset(in const vec3 relative_position, in const mat4 local_latrix)


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
#define USER_BINDING_INDEX10 24
#define USER_BINDING_INDEX11 25
#define USER_BINDING_INDEX12 26
#define USER_BINDING_INDEX13 27
#define USER_BINDING_INDEX14 28
#define USER_BINDING_INDEX15 29
#define USER_BINDING_INDEX16 30
#define USER_BINDING_INDEX17 31

// input and output
#if SHADER_STAGE_FLAG == VERTEX
    layout(location = 0) in vec3 inPosition;
    layout(location = 1) in vec3 inNormal;
    layout(location = 2) in vec3 inTangent;
    layout(location = 3) in vec4 inColor;
    layout(location = 4) in vec2 inTexCoord;
    #if (RenderObjectType_Skeletal == RenderObjectType)
        layout(location = 5) in uvec4 inBoneIndices;
        layout(location = 6) in vec4 inBoneWeights;
    #endif
    layout(location = 0) out VERTEX_OUTPUT vs_output;
#elif SHADER_STAGE_FLAG == FRAGMENT
    layout(location = 0) in VERTEX_OUTPUT vs_output;
    #if (RenderMode_GBuffer == RenderMode)
        layout(location = 0) out vec4 outAlbedo;
        layout(location = 1) out vec4 outMaterial;
        layout(location = 2) out vec4 outNormal;
        layout(location = 3) out vec2 outVelocity;
    #elif (RenderMode_Forward == RenderMode)
        layout(location = 0) out vec4 outColor;
    #elif (RenderMode_CaptureHeightMap == RenderMode)
        layout(location = 0) out vec4 outNormalWithDepth;
        layout(location = 1) out float outDepth;
    #elif (RenderMode_DepthPrepass == RenderMode || RenderMode_Shadow == RenderMode)
        layout(location = 0) out float outDepth;
    #endif
#endif
