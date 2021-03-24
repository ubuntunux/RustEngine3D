const int PARTICLE_STATE_NONE = 0;
const int PARTICLE_STATE_DELAY = 1 << 0;
const int PARTICLE_STATE_ALIVE = 1 << 1;
const int PARTICLE_STATE_DEAD = 1 << 2;

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
    int _align_mode;
    int _geometry_type;
    int _reserved0;
    int _reserved1;
};

struct GpuParticleDynamicConstants
{
    mat4 _emitter_transform;
    int _spawn_count;
    int _allocated_emitter_index;
    int _allocated_particle_offset;
    int _reserved0;
};

struct GpuParticleCountBufferData
{
    int _particle_alive_count;
    int _prev_particle_alive_count;
    int _particle_dead_count;
    int _reserved0;
};

struct GpuParticleEmitterIndexBufferData
{
    int _emitter_index;
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
    float _reserved0;
};

//
struct ParticleData
{
    mat4 parent_matrix;
    mat4 local_matrix;
    vec3 force;
    float delay;
    vec3 transform_position;
    float lifetime;
    vec3 transform_rotation;
    float opacity;
    vec3 transform_scale;
    float elapsed_time;
    vec3 velocity_position;
    float sequence_ratio;
    vec3 velocity_rotation;
    int sequence_index;
    vec3 velocity_scale;
    int next_sequence_index;
    vec2 sequence_uv;
    vec2 next_sequence_uv;
    vec3 relative_position;
    int state;
};


struct ParticleIndexRange
{
    uint begin_index;
    uint instance_count;
    uint destroy_count;
    uint dummy;
};


struct DispatchIndirectCommand
{
    uint num_groups_x;
    uint num_groups_y;
    uint num_groups_z;
};

struct DrawElementsIndirectCommand
{
    uint vertex_count;
    uint instance_count;
    uint first_index;
    uint base_vertex;
    uint base_instance;
};