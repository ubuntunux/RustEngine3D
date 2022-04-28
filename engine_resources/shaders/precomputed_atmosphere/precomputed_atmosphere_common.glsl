layout( push_constant ) uniform PushConstant_PrecomputedAtmosphere
{
    mat3 _luminance_from_radiance;
    int _scattering_order;
    int _layer;
    int _reserved0;
} pushConstant;