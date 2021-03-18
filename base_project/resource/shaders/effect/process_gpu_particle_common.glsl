#include "effect_constants.glsl"

layout( push_constant ) uniform PushConstant_ProcessGpuParticle
{
    mat4 _localMatrix;
} pushConstant;