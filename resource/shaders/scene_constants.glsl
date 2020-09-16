#ifndef _SCENE_CONSTANTS_
#define _SCENE_CONSTANTS_

// Constants.hs, data RenderMode
#define RenderMode_Common   0
#define RenderMode_Shadow   1

// Constants.hs, data RenderObjectType
#define RenderObject_Static     0
#define RenderObject_Skeletal   1

const int BLEND = 0;
const int ADDITIVE = 1;
const int MULTIPLY = 2;
const int SUBTRACT = 3;

#define WORK_GROUP_SIZE 64

const int MAX_BONES_PER_VERTEX = 4;
const int MAX_BONES = 100;

const float PI = 3.14159265358979323846;
const float HALF_PI = PI * 0.5;
const float TWO_PI = PI * 2.0;
const float deg = PI / 180.0;

const vec3 kSphereCenter = vec3(1.0, 1.0, -2.0);
const float kSphereRadius = 1.0;
const vec3 kSphereAlbedo = vec3(0.8);
const vec3 kGroundAlbedo = vec3(0.0, 0.0, 0.04);

const int MAX_POINT_LIGHTS = 10;

// UniformBufferDatas.hs - data SceneConstants
struct SCENE_CONSTANTS
{
    vec2 SCREEN_SIZE;
    vec2 BACKBUFFER_SIZE;
    float TIME;
    float DELTA_TIME;
    float JITTER_FRAME;
    int SCENE_CONSTANTS_DUMMY0;
};

// UniformBufferDatas.hs - data ViewConstants
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
    vec3 CAMERA_POSITION;
    float VIEWCONSTANTS_DUMMY0;
    vec3 CAMERA_POSITION_PREV;
    float VIEWCONSTANTS_DUMMY1;
    vec2 NEAR_FAR;
    vec2 JITTER_DELTA;
    vec2 JITTER_OFFSET;
    float VIEWCONSTANTS_DUMMY2;
    float VIEWCONSTANTS_DUMMY3;
};

// UniformBufferDatas.hs - data LightConstants
struct LIGHT_CONSTANTS
{
    mat4 SHADOW_VIEW_PROJECTION;
    vec3 LIGHT_POSITION;
    float SHADOW_EXP;
    vec3 LIGHT_DIRECTION;
    float SHADOW_BIAS;
    vec3 LIGHT_COLOR;
    int SHADOW_SAMPLES;
    vec4 SHADOW_DIMENSIONS;
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