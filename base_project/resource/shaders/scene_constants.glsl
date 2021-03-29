#ifndef _SCENE_CONSTANTS_
#define _SCENE_CONSTANTS_

// constants.rs
#define SSAO_KERNEL_SIZE 64

// enum RenderMode
#define RenderMode_GBuffer 0
#define RenderMode_Forward 1
#define RenderMode_Shadow 2
#define RenderMode_CaptureHeightMap 3

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
// MAX_BONES must match with constants.rs
#define MAX_BONES 128 * 128

const float PI = 3.14159265358979323846;
const float HALF_PI = PI * 0.5;
const float TWO_PI = PI * 2.0;
const float deg = PI / 180.0;

const vec3 kSphereCenter = vec3(1.0, 1.0, -2.0);
const float kSphereRadius = 1.0;
const vec3 kSphereAlbedo = vec3(0.8);
const vec3 kGroundAlbedo = vec3(0.0, 0.0, 0.04);

const int MAX_POINT_LIGHTS = 10;

const float SEA_COASTLINE_THICKNESS = 1.0;

// uniform_buffer_datas.rs - struct SceneConstants
struct SCENE_CONSTANTS
{
    vec2 SCREEN_SIZE;
    vec2 BACKBUFFER_SIZE;
    float TIME;
    float DELTA_TIME;
    float SEA_HEIGHT;
    int MAX_PARTICLE_COUNT;
    int MAX_EMITTER_COUNT;
    int GPU_PARTICLE_COUNT_BUFFER_OFFSET;
    int GPU_PARTICLE_UPDATE_BUFFER_OFFSET;
    int reserved0;
};

// uniform_buffer_datas.rs - struct ViewConstants
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
    float VIEWCONSTANTS_DUMMY0;
    vec2 NEAR_FAR;
    vec2 JITTER_DELTA;
    vec2 JITTER_OFFSET;
    float VIEWCONSTANTS_DUMMY1;
    float VIEWCONSTANTS_DUMMY2;
};

// uniform_buffer_datas.rs - struct LightConstants
struct LIGHT_CONSTANTS
{
    mat4 SHADOW_VIEW_PROJECTION;
    vec3 LIGHT_POSITION;
    float SHADOW_EXP;
    vec3 LIGHT_DIRECTION;
    float SHADOW_BIAS;
    vec3 LIGHT_COLOR;
    int SHADOW_SAMPLES;
};

struct POINT_LIGHT
{
    vec3 color;
    float radius;
    vec3 pos;
    float render;
};

struct POINT_LIGHTS
{
    POINT_LIGHT data[MAX_POINT_LIGHTS];
};

#endif // _SCENE_CONSTANTS_