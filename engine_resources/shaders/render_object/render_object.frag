#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

// shader predefined
#include "../common/render_object_common.glsl"

// user defined shader
#include "render_object.glsl"

// shader entry point
#include "../common/render_object_common.frag"