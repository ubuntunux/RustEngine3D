#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor0;
#if 1 < ColorAttachmentCount
layout(location = 1) out vec4 outColor1;
#endif
#if 2 < ColorAttachmentCount
layout(location = 2) out vec4 outColor2;
#endif
#if 3 < ColorAttachmentCount
layout(location = 3) out vec4 outColor3;
#endif
#if 4 < ColorAttachmentCount
layout(location = 4) out vec4 outColor4;
#endif
#if 5 < ColorAttachmentCount
layout(location = 5) out vec4 outColor5;
#endif
#if 6 < ColorAttachmentCount
layout(location = 6) out vec4 outColor6;
#endif
#if 7 < ColorAttachmentCount
layout(location = 7) out vec4 outColor7;
#endif

void main() {
}
