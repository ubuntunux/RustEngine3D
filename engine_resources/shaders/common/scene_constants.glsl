#ifndef _SCENE_CONSTANTS_
#define _SCENE_CONSTANTS_

// constants.rs
#define SSAO_KERNEL_SIZE 64

// enum RenderMode
#define RenderMode_DepthPrepass 0
#define RenderMode_GBuffer 1
#define RenderMode_Forward 2
#define RenderMode_Shadow 3
#define RenderMode_CaptureHeightMap 4

// enum RenderObjectType
#define RenderObjectType_Static 0
#define RenderObjectType_Skeletal 1

#define BLEND 0
#define ADDITIVE 1
#define MULTIPLY 2
#define SUBTRACT 3

// must match with constants.rs
#define WORK_GROUP_SIZE 64
#define PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE 64

#define MAX_BONES_PER_VERTEX 4
#define MAX_TRANSFORM_COUNT 65536 // MAX_TRANSFORM_COUNT must match with constants.rs

const float PI = 3.14159265358979323846;
const float HALF_PI = PI * 0.5;
const float TWO_PI = PI * 2.0;
const float deg = PI / 180.0;

const vec3 kSphereCenter = vec3(1.0, 1.0, -2.0);
const float kSphereRadius = 1.0;
const vec3 kSphereAlbedo = vec3(0.8);
const vec3 kGroundAlbedo = vec3(0.0, 0.0, 0.04);

const int MAX_POINT_LIGHTS = 32; // MAX_POINT_LIGHTS must match with constants.rs

const float SEA_COASTLINE_THICKNESS = 1.0;

// uniform_buffer_data_list.rs - struct SceneConstants
struct SCENE_CONSTANTS
{
    vec2 SCREEN_SIZE;
    vec2 BACK_BUFFER_SIZE;
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
    int reserved0;
    int reserved1;
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
    vec3 CAMERA_POSITION;
    int JITTER_FRAME;
    vec3 CAMERA_POSITION_PREV;
    float VIEW_CONSTANTS_DUMMY0;
    vec2 NEAR_FAR;
    vec2 JITTER_DELTA;
    vec2 JITTER_OFFSET;
    float VIEW_CONSTANTS_DUMMY1;
    float VIEW_CONSTANTS_DUMMY2;
};

// light.rs - struct LightData
struct LIGHT_DATA
{
    mat4 SHADOW_VIEW_PROJECTION;
    vec3 LIGHT_POSITION;
    int SHADOW_SAMPLES;
    vec3 LIGHT_DIRECTION;
    int LIGHT_DATA_TEMP0;
    vec3 LIGHT_COLOR;
    int LIGHT_DATA_TEMP1;
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

#endif // _SCENE_CONSTANTS_