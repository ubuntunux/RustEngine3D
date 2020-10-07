use nalgebra::{
    Matrix4
};

use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

trait PushConstantInterface {
}

#[derive(Debug, Clone)]
struct PushConstants_StaticRenderObject {
    _model_matrixt: Matrix4<f32>,
}

impl PushConstantInterface for PushConstants_StaticRenderObject {

}

// getPushConstantRange :: PushConstantData -> VkShaderStageFlags -> VkPushConstantRange
// getPushConstantRange pushConstantData shaderStage = createVk @VkPushConstantRange
//     $ set @"stageFlags" shaderStage
//     &* set @"size" (bSizeOf @PushConstantData pushConstantData)
//     &* set @"offset" 0