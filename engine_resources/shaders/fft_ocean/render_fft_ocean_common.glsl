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
layout(binding = 4) uniform sampler2DArray fftWavesSampler;
layout(binding = 5) uniform sampler2D texture_scene;
layout(binding = 6) uniform sampler2D texture_depth;
layout(binding = 7) uniform sampler2D texture_shadow;
layout(binding = 8) uniform samplerCube texture_probe;
layout(binding = 9) uniform sampler2D texture_noise;
layout(binding = 10) uniform sampler3D texture_caustic;
layout(binding = 11) uniform sampler2D texture_foam;
layout(binding = 12) uniform sampler2D transmittance_texture;
layout(binding = 13) uniform sampler2D irradiance_texture;
layout(binding = 14) uniform sampler3D scattering_texture;
layout(binding = 15) uniform sampler3D single_mie_scattering_texture;

layout( push_constant ) uniform PushConstant_FFT_Ocean
{
    vec4 _simulation_size;
    vec2 _cell_size;
    float _simulation_wind;
    float _simulation_amplitude;
    float _t;
    int _reserved0;
    int _reserved1;
    int _reserved2;
} pushConstant;

struct VERTEX_OUTPUT
{
    vec4 uvs;
    vec3 wave_offset;
    float shadow_factor;
    float vertex_noise;
    float screen_fade;
    vec3 vertex_normal;
    vec3 relative_pos;
    vec4 proj_pos;
    vec3 sun_irradiance;
    vec3 sky_irradiance;
    vec3 in_scatter;
};
