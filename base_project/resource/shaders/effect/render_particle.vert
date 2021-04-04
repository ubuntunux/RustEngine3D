#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "../utility.glsl"
#include "render_particle_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;
layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    const int instance_id = int(gl_InstanceIndex);
    const int count_buffer_index = scene_constants.GPU_PARTICLE_COUNT_BUFFER_OFFSET + pushConstant._allocated_emitter_index;
    const int particle_dead_count = gpu_particle_count_buffer[count_buffer_index]._particle_dead_count;
    const int particle_alive_count = max(0, gpu_particle_count_buffer[count_buffer_index]._particle_alive_count - particle_dead_count);
    const int update_buffer_index = scene_constants.GPU_PARTICLE_UPDATE_BUFFER_OFFSET + pushConstant._allocated_particle_offset + particle_dead_count + instance_id;
    const uint particle_state = gpu_particle_update_buffer[update_buffer_index]._particle_state;
    if(particle_alive_count <= instance_id || false == check_flags_any(PARTICLE_STATE_ALIVE, particle_state))
    {
        gl_Position = vec4(0.0 / 0.0);
        return;
    }

    vec4 position = vec4(inPosition, 1.0);
    vec4 prev_position = vec4(inPosition, 1.0);
    vec3 vertex_normal = normalize(inNormal);
    vec3 vertex_tangent = normalize(inTangent);

    mat4 localMatrix = mat4(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);
    vec3 particle_scale = gpu_particle_update_buffer[update_buffer_index]._particle_initial_scale;
    localMatrix[0].xyz *= particle_scale.x;
    localMatrix[1].xyz *= particle_scale.y;
    localMatrix[2].xyz *= particle_scale.z;
    localMatrix[3].xyz = gpu_particle_update_buffer[update_buffer_index]._particle_relative_position;
    vec3 relative_pos = (localMatrix * position).xyz;
    vec3 relative_pos_prev = (localMatrix * prev_position).xyz + (view_constants.CAMERA_POSITION - view_constants.CAMERA_POSITION_PREV);

    vs_output.projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV_JITTER * vec4(relative_pos_prev, 1.0);
    vs_output.projection_pos = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(relative_pos, 1.0);
    vs_output.relative_position = relative_pos;
    gl_Position = vs_output.projection_pos;

    const float play_time_ratio = saturate(gpu_particle_update_buffer[update_buffer_index]._particle_elapsed_time / gpu_particle_update_buffer[update_buffer_index]._particle_initial_life_time);
    vs_output.color = inColor * vec4(1.0, 1.0, 1.0, 1.0);
    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vec3 bitangent = cross(vertex_tangent, vertex_normal);
    vs_output.tangent_to_world = mat3(localMatrix) * mat3(vertex_tangent, bitangent, vertex_normal);
    vs_output.texCoord = inTexCoord;
}
