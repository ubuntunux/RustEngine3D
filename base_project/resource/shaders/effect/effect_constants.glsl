const uint GPU_PARTICLE_CONSTANT_FLAG_NONE = 0;
const uint GPU_PARTICLE_CONSTANT_FLAG_FIRST_UPDATE = 1 << 0;

const uint PARTICLE_STATE_NONE = 0;
const uint PARTICLE_STATE_DELAY = 1 << 0;
const uint PARTICLE_STATE_ALIVE = 1 << 1;
const uint PARTICLE_STATE_DEAD = 1 << 2;

const uint ParticleBlendMode_AlphaBlend = 0;
const uint ParticleBlendMode_Additive = 1;
const uint ParticleBlendMode_Opaque = 2;

const uint ParticleSpawnVolumeType_Box = 0;
const uint ParticleSpawnVolumeType_Sphere = 1;
const uint ParticleSpawnVolumeType_Cone = 2;
const uint ParticleSpawnVolumeType_Cylinder = 3;

const uint ParticleGeometryType_Quad = 0;
const uint ParticleGeometryType_Decal = 1;
const uint ParticleGeometryType_Mesh = 2;
const uint ParticleGeometryType_Ribbon = 3;
const uint ParticleGeometryType_Beam = 4;
const uint ParticleGeometryType_Capsule = 5;

const uint ParticleAlignMode_None = 0;
const uint ParticleAlignMode_Billboard = 1;
const uint ParticleAlignMode_VelocityAlign = 2;

const uint ParticleVelocityType_Local = 0;
const uint ParticleVelocityType_WorldY_LocalXZ = 1;
const uint ParticleVelocityType_NormalDirection = 2;

//
struct GpuParticleStaticConstants
{
    mat4 _spawn_volume_transform;
    vec4 _spawn_volume_info;
    vec3 _rotation_min;
    float _particle_lifetime_min;
    vec3 _rotation_max;
    float _particle_lifetime_max;
    vec3 _scale_min;
    int _spawn_volume_type;
    vec3 _scale_max;
    int _max_particle_count;
    vec3 _velocity_min;
    int _align_mode;
    vec3 _velocity_max;
    int _geometry_type;
    vec3 _force_min;
    int _reserved0;
    vec3 _force_max;
    int _reserved1;
};

struct GpuParticleDynamicConstants
{
    mat4 _emitter_transform;
    int _prev_allocated_emitter_index;
    int _prev_allocated_particle_offset;
    int _allocated_emitter_index;
    int _allocated_particle_offset;
    int _spawn_count;
    uint _gpu_particle_constant_flags;
    uint _reserved0;
    uint _reserved1;
};

struct GpuParticleCountBufferData
{
    int _particle_alive_count;
    int _prev_particle_alive_count;
    int _particle_dead_count;
    int _reserved0;
};

struct GpuParticleUpdateBufferData
{
    mat4 _particle_emitter_transform;
    vec3 _particle_relative_position;
    float _particle_elapsed_time;
    vec3 _particle_local_position;
    float _particle_initial_life_time;
    vec3 _particle_initial_rotation;
    uint _particle_state;
    vec3 _particle_initial_scale;
    int _reserved0;
    vec3 _particle_velocity;
    int _reserved1;
    vec3 _particle_initial_force;
    int _reserved2;
};