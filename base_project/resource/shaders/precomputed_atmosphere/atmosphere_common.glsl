#include "atmosphere_predefined.glsl"

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

layout(binding = 4) uniform sampler2D texture_shadow;
layout(binding = 5) uniform sampler2D texture_depth;
layout(binding = 6) uniform sampler3D texture_cloud;
layout(binding = 7) uniform sampler3D texture_noise;
layout(binding = 8) uniform sampler2D transmittance_texture;
layout(binding = 9) uniform sampler2D irradiance_texture;
layout(binding = 10) uniform sampler3D scattering_texture;
layout(binding = 11) uniform sampler3D single_mie_scattering_texture;
layout(binding = 12) uniform sampler3D single_rayleigh_scattering_texture;
layout(binding = 13) uniform sampler3D scattering_density_texture;
layout(binding = 14) uniform sampler3D multiple_scattering_texture;
layout(binding = 15) uniform sampler2D texture_atmosphere;
layout(binding = 16) uniform sampler2D texture_inscatter;