layout( push_constant ) uniform PushConstant_FFT_Ocean
{
    vec4 _grid_sizes;
    vec4 _inverse_grid_sizes;
    vec4 _simulation_size;
    vec2 _cell_size;
    float _simulation_wind;
    float _simulation_amplitude;
    float _t;
    int _fft_size;
    float _pass;
    float _n_slope_variance;
    float _slope_variance_delta;
    float _c;
    int _reserved0;
    int _reserved1;
} pushConstant;
