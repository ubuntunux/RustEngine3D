#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "shading.glsl"

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
layout(binding = 3) uniform sampler2D textureSceneAlbedo;
layout(binding = 4) uniform sampler2D textureSceneMaterial;
layout(binding = 5) uniform sampler2D textureSceneNormal;
layout(binding = 6) uniform sampler2D textureSceneDepth;
layout(binding = 7) uniform sampler2D textureSSAO;
layout(binding = 8) uniform sampler2D textureShadow;
layout(binding = 9) uniform samplerCube textureProbe;
//layout(binding = 10) uniform sampler2D ibl_brdf_lut;
//layout(binding = 10) uniform sampler2D textureSceneReflect;


layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

void main() {

    float depth = texture(textureSceneDepth, texCoord).x;

    if(depth == 1.0)
    {
        outColor = vec4(0.0);
        return;
    }

    vec4 base_color = texture(textureSceneAlbedo, texCoord);
    float opacity = base_color.w;
    vec3 emissive_color = vec3(0.0);

    vec4 material = texture(textureSceneMaterial, texCoord);
    vec3 N = normalize(texture(textureSceneNormal, texCoord).xyz * 2.0 - 1.0);

    vec4 relative_position = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION, texCoord, depth);
    vec3 world_position = relative_position.xyz + view_constants.CAMERA_POSITION;

    float roughness = material.x;
    float metalicness = material.y;
    float reflectance = material.z;
    float ssao = texture(textureSSAO, texCoord).x;
    vec4 scene_reflect_color = vec4(0.0);//texture(textureSceneReflect, texCoord);

    vec3 V = normalize(-relative_position.xyz);
    vec3 L = normalize(light_constants.LIGHT_DIRECTION);

    outColor = surface_shading(
        //const in AtmosphereParameters ATMOSPHERE,
        scene_constants,
        view_constants,
        light_constants,
        //point_lights,
        base_color.xyz,
        opacity,
        emissive_color,
        metalicness,
        roughness,
        reflectance,
        ssao,
        scene_reflect_color,
        textureProbe,
        //ibl_brdf_lut,
        textureShadow,
        texCoord,
        world_position,
        light_constants.LIGHT_COLOR.xyz,
        N,
        V,
        L,
        depth
    );
    outColor.w = 1.0;
}
