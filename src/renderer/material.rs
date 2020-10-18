use std::collections::HashMap;

use crate::vulkan_context::render_pass;

type RenderPassPipelineDataMap = HashMap<String, (render_pass::RenderPassData, render_pass::PipelineData)>;

#[derive(Clone, Debug)]
pub struct MaterialData {
    _material_data_name: String,
    _render_pass_pipeline_data_map: RenderPassPipelineDataMap,
    _material_parameter_map: String,
}

//
// createMaterial: String
//                -> [(RenderPass.RenderPassData, RenderPass.PipelineData)]
//                -> Aeson.Object
//                -> IO MaterialData
// createMaterial materialDataName renderPassPipelineDataList materialParameterMap = do
//     log::info!("createMaterial : " ++ Text.unpack materialDataName
//     renderPassPipelineDataMap <- forM renderPassPipelineDataList $ \(renderPassData, pipelineData) -> do
//         let renderPassPipelineDataName = (RenderPass._renderPassDataName renderPassData, RenderPass._pipelineDataName pipelineData)
//         logTrivialInfo $ "    renderPass, pipeline : " ++ show renderPassPipelineDataName
//         return (renderPassPipelineDataName, (renderPassData, pipelineData))
//     return MaterialData
//         { _materialDataName = materialDataName
//         , _renderPassPipelineDataMap = Map.fromList renderPassPipelineDataMap
//         , _materialParameterMap = materialParameterMap
//         }
//
// destroyMaterial: MaterialData -> IO ()
// destroyMaterial materialData = return ()
//
// getRenderPassPipelineData: MaterialData -> RenderPass.RenderPassPipelineDataName -> (RenderPass.RenderPassData, RenderPass.PipelineData)
// getRenderPassPipelineData materialData renderPassPipelineDataName = Maybe.fromJust $ Map.lookup renderPassPipelineDataName (_renderPassPipelineDataMap materialData)