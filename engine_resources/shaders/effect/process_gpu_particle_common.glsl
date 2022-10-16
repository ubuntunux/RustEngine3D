#include "effect_constants.glsl"

layout( push_constant ) uniform PushConstant_UpdateGpuParticle
{
    int _process_emitter_count;
    int _process_particle_count;
    int _dispatch_count;
    int _reserved0;
} pushConstant;