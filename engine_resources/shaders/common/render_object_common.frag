void main() {
    vec4 base_color = vec4(1.0, 1.0, 1.0, 1.0);
    // x: roughness, y: metallic, z: encoded emissive intensity, w: encoded shading model
    vec4 material = vec4(1.0, 0.0, 0.0, 0.0);
    vec3 tangent_normal = vec3(0.0, 0.0, 1.0);

    fragment_shader_main(
        base_color,
        material,
        tangent_normal
    );

#if (RenderMode_Forward == RenderMode)
    if(base_color.w < 0.05)
    {
        discard;
    }
#else
    if(base_color.w < 0.333)
    {
        discard;
    }
#endif

    vec3 normal = normalize(in_vertex_output._tangent_to_world * tangent_normal);
    vec3 vertex_normal = normalize(in_vertex_output._tangent_to_world[2]);

#if (RenderMode_GBuffer == RenderMode)
    // xyz: albedo, w: encoded emissive_intensity
    outAlbedo.xyz = base_color.xyz;
    outAlbedo.w = material.z;
    // x: roughness, y: metallic, zw: octahedral vertex_normal
    outMaterial.xy = material.xy;
    outMaterial.zw = oct_encode_normal(vertex_normal);
    // xyz: world normal, w: encoded shading_model
    outNormal.xyz = normal * 0.5 + 0.5;
    outNormal.w = material.w;
    outVelocity = ((in_vertex_output._projection_pos.xy / in_vertex_output._projection_pos.w) - (in_vertex_output._projection_pos_prev.xy / in_vertex_output._projection_pos_prev.w)) * 0.5;
    outVelocity -= view_constants.JITTER_DELTA;
#elif (RenderMode_Forward == RenderMode)
    vec2 screen_texcoord = (in_vertex_output._projection_pos.xy / in_vertex_output._projection_pos.w) * 0.5 + 0.5;
    float depth = gl_FragCoord.z;
    vec3 world_position = in_vertex_output._relative_position.xyz + view_constants.CAMERA_POSITION;
    float opacity = base_color.w;
    vec3 emissive_color = base_color.xyz * decode_emissive_intensity(material.z);
    float roughness = material.x;
    float metallic = material.y;
    float reflectance = 0.0;
    float shadow_factor = 1.0;
    float ao_factor = 1.0;
    uint shading_model = decode_shading_model(material.w);
    vec4 scene_reflect_color = vec4(0.0);
    vec3 V = normalize(-in_vertex_output._relative_position.xyz);
    const bool IS_COMBINED_SHADOW = false;

    outColor = surface_shading(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        irradiance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        scene_constants,
        view_constants,
        light_data,
        point_lights,
        base_color.xyz,
        opacity,
        metallic,
        roughness,
        reflectance,
        shadow_factor,
        ao_factor,
        shading_model,
        scene_reflect_color,
        vec4(0.0),
        texture_probe,
        textureShadow,
        textureHeightMap,
        screen_texcoord,
        world_position.xyz,
        vertex_normal.xyz,
        normal.xyz,
        normal.xyz,
        V,
        depth,
        IS_COMBINED_SHADOW
    );
    outColor.xyz += emissive_color;
    outColor.w = opacity;
#elif (RenderMode_CaptureHeightMap == RenderMode)
    outNormalWithDepth = vec4(normalize(in_vertex_output._tangent_to_world[2]) * 0.5 + 0.5, gl_FragCoord.z);
    outDepth = gl_FragCoord.z;
#elif (RenderMode_DepthPrepass == RenderMode || RenderMode_Shadow == RenderMode)
    outDepth = gl_FragCoord.z;
#endif
}