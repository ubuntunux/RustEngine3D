#ifndef _SCENE_CONSTANTS_
#define _SCENE_CONSTANTS_

// constants.rs
#define SSAO_KERNEL_SIZE 64
#define SHADOW_BIAS -0.000025

// enum RenderMode
#define RenderMode_DepthPrepass 0
#define RenderMode_GBuffer 1
#define RenderMode_Forward 2
#define RenderMode_Shadow 3
#define RenderMode_CaptureHeightMap 4

// enum RenderObjectType
#define RenderObjectType_Static 0
#define RenderObjectType_Skeletal 1

#define BlendMode_AlphaBlend 0
#define BlendMode_Additive 1
#define BlendMode_Opaque 2

// Global immutable sampler indices (must match image_sampler.rs)
#define SAMPLER_LINEAR_REPEAT 0
#define SAMPLER_LINEAR_CLAMP 1
#define SAMPLER_POINT_REPEAT 2
#define SAMPLER_POINT_CLAMP 3
#define SAMPLER_ANISO_REPEAT 4
#define SAMPLER_ANISO_CLAMP 5
#define GLOBAL_SAMPLER_COUNT 6

// must match with constants.rs
#define WORK_GROUP_SIZE 64
#define PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE 64

#define MAX_BONES_PER_VERTEX 4
#define MAX_TRANSFORM_COUNT 65536 // MAX_TRANSFORM_COUNT must match with constants.rs
#define CSM_CASCADE_COUNT 4
#define MAX_SPOT_LIGHTS 16
#define MAX_LOCAL_LIGHT_PROBES 16
#define VOLUMETRIC_FOG_GRID_WIDTH 160
#define VOLUMETRIC_FOG_GRID_HEIGHT 90
#define VOLUMETRIC_FOG_GRID_DEPTH 64
#define VOLUMETRIC_FOG_NEAR 0.5
#define VOLUMETRIC_FOG_FAR 160.0

// GBuffer Shading Model IDs (stored in SceneNormal.w via encode_shading_model)
const uint ShadingModel_Default = 0u;
const uint ShadingModel_Foliage = 1u;
const uint ShadingModel_Character = 2u;

const float PI = 3.14159265358979323846;
const float HALF_PI = PI * 0.5;
const float TWO_PI = PI * 2.0;
const float deg = PI / 180.0;

const vec3 kSphereCenter = vec3(1.0, 1.0, -2.0);
const float kSphereRadius = 1.0;
const vec3 kSphereAlbedo = vec3(0.8);
const vec3 kGroundAlbedo = vec3(0.0, 0.0, 0.04);

const int MAX_POINT_LIGHTS = 32; // MAX_POINT_LIGHTS must match with constants.rs
const int MAX_LIGHT_COUNT_PER_CELL = 4; // MAX_LIGHT_COUNT_PER_CELL must match with constants.rs

const uint MAX_BOUND_BOX_INSTANCE_COUNT = 65536; // NOTE: Ensure enum values match in constants.rs
const uint MAX_DEBUG_LINE_INSTANCE_COUNT = 65536; // NOTE: Ensure enum values match in constants.rs
const uint MAX_FONT_INSTANCE_COUNT = 65536; // NOTE: Ensure enum values match in constants.rs
const uint MAX_UI_INSTANCE_COUNT = 65536; // NOTE: Ensure enum values match in constants.rs

const float SEA_COASTLINE_THICKNESS = 1.0;

// enum RenderOption
const uint RenderOption_RenderSSR = 1u << 0u;
const uint RenderOption_RenderOcean = 1u << 1u;
const uint RenderOption_RenderAtmosphere = 1u << 2u;
const uint RenderOption_RenderSky = 1u << 3u;
const uint RenderOption_RenderShadow = 1u << 4u;
const uint RenderOption_RenderSSAO = 1u << 5u;
const uint RenderOption_RenderSSGI = 1u << 6u;
const uint RenderOption_RenderHalfLambert = 1u << 7u;
const uint RenderOption_RenderShadowTint = 1u << 8u;
const uint RenderOption_RenderRimLight = 1u << 9u;
const uint RenderOption_RenderFoliageSSS = 1u << 10u;
const uint RenderOption_RenderPCSS = 1u << 11u;
const uint RenderOption_RenderContactShadow = 1u << 12u;
const uint RenderOption_RenderBloom = 1u << 13u;
const uint RenderOption_RenderHeightFog = 1u << 14u;
const uint RenderOption_RenderAutoExposure = 1u << 15u;
const uint RenderOption_RenderGodRays = 1u << 16u;
const uint RenderOption_RenderDepthOfField = 1u << 17u;
const uint RenderOption_RenderStylizedOutline = 1u << 18u;
const uint RenderOption_RenderVolumetricFog = 1u << 19u;
const uint RenderOption_RenderLocalContactShadow = 1u << 20u;

// uniform_buffer_data_list.rs - struct SceneConstants
struct SCENE_CONSTANTS
{
    vec2 SCREEN_SIZE;
    vec2 BACK_BUFFER_SIZE;
    vec3 MOON_LIGHT_COLOR;
    float TIME;
    float DELTA_TIME;
    float SEA_HEIGHT;
    int MAX_PARTICLE_COUNT;
    int MAX_EMITTER_COUNT;
    int GPU_PARTICLE_COUNT_BUFFER_OFFSET;
    int GPU_PARTICLE_UPDATE_BUFFER_OFFSET;
    int PREV_GPU_PARTICLE_COUNT_BUFFER_OFFSET;
    int PREV_GPU_PARTICLE_UPDATE_BUFFER_OFFSET;
    int RENDER_POINT_LIGHT_COUNT;
    uint ELAPSED_FRAME;
    uint RENDER_OPTION;
    float DOF_INTENSITY;
    float BLOOM_INTENSITY;
    float GOD_RAYS_INTENSITY;
    float HEIGHT_FOG_INTENSITY;
    float MOTION_BLUR_INTENSITY;
    vec2 WIND_DIRECTION;
    float WIND_STRENGTH;
    float WIND_TIME;
    float WETNESS;
    float RAIN_INTENSITY;
    float SCENE_RESERVED0;
    float SCENE_RESERVED1;
};

// uniform_buffer_data_list.rs - struct ViewConstants
struct VIEW_CONSTANTS
{
    mat4 VIEW;
    mat4 INV_VIEW;
    mat4 VIEW_ORIGIN;
    mat4 INV_VIEW_ORIGIN;
    mat4 PROJECTION;
    mat4 INV_PROJECTION;
    mat4 VIEW_PROJECTION;
    mat4 INV_VIEW_PROJECTION;
    mat4 VIEW_ORIGIN_PROJECTION;
    mat4 INV_VIEW_ORIGIN_PROJECTION;
    mat4 VIEW_ORIGIN_PROJECTION_PREV;
    mat4 PROJECTION_JITTER;
    mat4 INV_PROJECTION_JITTER;
    mat4 VIEW_PROJECTION_JITTER;
    mat4 INV_VIEW_PROJECTION_JITTER;
    mat4 VIEW_ORIGIN_PROJECTION_JITTER;
    mat4 INV_VIEW_ORIGIN_PROJECTION_JITTER;
    mat4 VIEW_ORIGIN_PROJECTION_PREV_JITTER;
    mat4 CAPTURE_HEIGHT_MAP_VIEW_PROJECTION;
    mat4 INV_CAPTURE_HEIGHT_MAP_VIEW_PROJECTION;
    vec3 CAMERA_POSITION;
    int JITTER_FRAME;
    vec3 CAMERA_POSITION_PREV;
    uint FRAME_COUNT;
    vec2 NEAR_FAR;
    vec2 JITTER_DELTA;
    vec2 JITTER_OFFSET;
    uint ELAPSED_FRAME;
    float VIEW_CONSTANTS_DUMMY1;
};

// light.rs - struct LightData
struct LIGHT_DATA
{
    mat4 SHADOW_VIEW_PROJECTION;
    mat4 INV_SHADOW_VIEW_PROJECTION;
    vec3 LIGHT_POSITION;
    int SHADOW_SAMPLES;
    vec3 LIGHT_DIRECTION;
    int ACTIVE_SHADOW_CASCADE;
    vec3 LIGHT_COLOR;
    int CSM_CASCADE_COUNT_ACTIVE;
    mat4 CSM_VIEW_PROJECTIONS[CSM_CASCADE_COUNT];
    mat4 INV_CSM_VIEW_PROJECTIONS[CSM_CASCADE_COUNT];
    vec4 CSM_SPLIT_DISTANCES;
    vec4 CSM_ATLAS_RECTS[CSM_CASCADE_COUNT];
};

// light.rs - struct PointLightData
struct POINT_LIGHT_DATA
{
    vec3 LIGHT_POSITION;
    float RADIUS;
    vec3 LIGHT_COLOR;
    int reserved0;
};

// shader_buffer_data.rs - struct PointLights
struct POINT_LIGHTS
{
    POINT_LIGHT_DATA point_light_data[MAX_POINT_LIGHTS];
};

// light.rs - struct SpotLightData
struct SPOT_LIGHT_DATA
{
    vec3 LIGHT_POSITION;
    float RADIUS;
    vec3 LIGHT_DIRECTION;
    float OUTER_COS_ANGLE;
    vec3 LIGHT_COLOR;
    float INNER_COS_ANGLE;
};

// light.rs - struct LocalLightProbeData
struct LOCAL_LIGHT_PROBE_DATA
{
    vec3 POSITION;
    float BLEND_DISTANCE;
    vec3 HALF_EXTENTS;
    float INDOOR_FACTOR;
    vec3 SKY_IRRADIANCE;
    float INTENSITY;
    vec3 GROUND_IRRADIANCE;
    float PRIORITY;
    vec3 SIDE_IRRADIANCE;
    float SPECULAR_TINT;
};

// light.rs - struct LocalLightingData
struct LOCAL_LIGHTING_DATA
{
    SPOT_LIGHT_DATA spot_lights[MAX_SPOT_LIGHTS];
    LOCAL_LIGHT_PROBE_DATA local_light_probes[MAX_LOCAL_LIGHT_PROBES];
    int SPOT_LIGHT_COUNT;
    int LOCAL_LIGHT_PROBE_COUNT;
    int RESERVED0;
    int RESERVED1;
};

// shader_buffer_data.rs - struct LightIndicesCell
struct LIGHT_INDICES_CELL {
    int light_indices[MAX_LIGHT_COUNT_PER_CELL];
};

#endif // _SCENE_CONSTANTS_