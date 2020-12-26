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
layout(binding = 3) uniform sampler2DArray fftWavesSampler;
layout(binding = 4) uniform sampler3D slopeVarianceSampler;
layout(binding = 5) uniform sampler2D texture_scene;
layout(binding = 6) uniform sampler2D texture_linear_depth;
layout(binding = 7) uniform sampler2D texture_shadow;
layout(binding = 8) uniform samplerCube texture_probe;
layout(binding = 9) uniform sampler2D texture_noise;
layout(binding = 10) uniform sampler2D texture_caustic;
layout(binding = 11) uniform sampler2D texture_foam;

layout( push_constant ) uniform PushConstant_FFTOceanRender
{
    vec4 simulation_size;
    vec2 cell_size;
    float height;
    float simulation_wind;
    float simulation_amplitude;
    float t;
    uint reserved0;
    uint reserved1;
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
};
