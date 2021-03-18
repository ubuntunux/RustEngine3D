const int PARTICLE_STATE_NONE = 0;
const int PARTICLE_STATE_DELAY = 1;
const int PARTICLE_STATE_ALIVE = 2;
const int PARTICLE_STATE_DEAD = 3;

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