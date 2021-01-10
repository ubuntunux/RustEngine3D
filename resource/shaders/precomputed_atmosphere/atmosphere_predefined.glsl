struct ATMOSPHERE_CONSTANTS
{
    vec3 SKY_RADIANCE_TO_LUMINANCE;
    float cloud_exposure;

    vec3 SUN_RADIANCE_TO_LUMINANCE;
    float cloud_altitude;

    float cloud_height;
    float cloud_speed;
    float cloud_absorption;
    float cloud_tiling;

    float cloud_contrast;
    float cloud_coverage;
    float noise_tiling;
    float noise_contrast;

    vec3 earth_center;
    float noise_coverage;

    vec2 sun_size;
    float atmosphere_exposure;
    float inscatter_power;
};

// Definitions
struct DensityProfileLayer
{
    float width;
    float exp_term;
    float exp_scale;
    float linear_term;
    float constant_term;
};

struct DensityProfile
{
    DensityProfileLayer layers[2];
};

struct AtmosphereParameters
{
    vec3 solar_irradiance;
    float sun_angular_radius;
    float bottom_radius;
    float top_radius;
    DensityProfile rayleigh_density;
    vec3 rayleigh_scattering;
    DensityProfile mie_density;
    vec3 mie_scattering;
    vec3 mie_extinction;
    float mie_phase_function_g;
    DensityProfile absorption_density;
    vec3 absorption_extinction;
    vec3 ground_albedo;
    float mu_s_min;
};

// Constants
const int TRANSMITTANCE_TEXTURE_WIDTH = 256;
const int TRANSMITTANCE_TEXTURE_HEIGHT = 64;
const int SCATTERING_TEXTURE_R_SIZE = 32;
const int SCATTERING_TEXTURE_MU_SIZE = 128;
const int SCATTERING_TEXTURE_MU_S_SIZE = 32;
const int SCATTERING_TEXTURE_NU_SIZE = 8;
const int IRRADIANCE_TEXTURE_WIDTH = 64;
const int IRRADIANCE_TEXTURE_HEIGHT = 16;
const vec2 IRRADIANCE_TEXTURE_SIZE = vec2(64, 16);
const float ATMOSPHERE_RATIO = 0.1;

const AtmosphereParameters ATMOSPHERE = AtmosphereParameters(
vec3(1.474, 1.8504, 1.91198),
0.01175,
6361,
6420,
DensityProfile(DensityProfileLayer[2](DensityProfileLayer(0, 0, 0, 0, 0),DensityProfileLayer(0, 1, -0.125, 0, 0))),
vec3(0.005802339, 0.013557761, 0.033099998),
DensityProfile(DensityProfileLayer[2](DensityProfileLayer(0, 0, 0, 0, 0),DensityProfileLayer(0, 1, -0.8333334, 0, 0))),
vec3(0.0039959997, 0.0039959997, 0.0039959997),
vec3(0.00444, 0.00444, 0.00444),
0.8,
DensityProfile(DensityProfileLayer[2](DensityProfileLayer(25, 0, 0, 0.06666667, -0.6666667),DensityProfileLayer(0, 0, 0, -0.06666667, 2.6666667))),
vec3(0.0006497166, 0.0018809001, 0.00008501668),
vec3(0.1, 0.1, 0.1),
-0.50000006);