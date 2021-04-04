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
layout(binding = 4) uniform sampler2D textureShadow;
layout(binding = 5) uniform sampler2D textureHeightMap;
layout(binding = 6) uniform samplerCube texture_probe;
layout(binding = 7) uniform sampler2D transmittance_texture;
layout(binding = 8) uniform sampler2D irradiance_texture;
layout(binding = 9) uniform sampler3D scattering_texture;
layout(binding = 10) uniform sampler3D single_mie_scattering_texture;
layout(binding = 11) buffer readonly GpuParticleStaticConstantsBuffer
{
    GpuParticleStaticConstants gpu_particle_static_constants[];
};
layout(binding = 12) buffer readonly GpuParticleDynamicConstantsBuffer
{
    GpuParticleDynamicConstants gpu_particle_dynamic_constants[];
};
layout(binding = 13) buffer readonly GpuParticleCountBuffer
{
    GpuParticleCountBufferData gpu_particle_count_buffer[];
};
layout(binding = 14) buffer readonly GpuParticleUpdateBuffer
{
    GpuParticleUpdateBufferData gpu_particle_update_buffer[];
};
layout(binding = 15) uniform sampler2D textureBase;
layout(binding = 16) uniform sampler2D textureMaterial;
layout(binding = 17) uniform sampler2D textureNormal;

layout( push_constant ) uniform PushConstant_RenderParticle
{
    int _allocated_emitter_index;
    int _allocated_particle_offset;
    int _reserved0;
    int _reserved1;
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