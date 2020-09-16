{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.Material where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import HulkanEngine3D.Utilities.Logger

data MaterialData = MaterialData
    { _materialDataName :: Text.Text
    , _renderPassPipelineDataMap :: Map.Map RenderPass.RenderPassPipelineDataName (RenderPass.RenderPassData, RenderPass.PipelineData)
    , _materialParameterMap :: Aeson.Object
    } deriving Show


createMaterial :: Text.Text
               -> [(RenderPass.RenderPassData, RenderPass.PipelineData)]
               -> Aeson.Object
               -> IO MaterialData
createMaterial materialDataName renderPassPipelineDataList materialParameterMap = do
    logInfo $ "createMaterial : " ++ Text.unpack materialDataName
    renderPassPipelineDataMap <- forM renderPassPipelineDataList $ \(renderPassData, pipelineData) -> do
        let renderPassPipelineDataName = (RenderPass._renderPassDataName renderPassData, RenderPass._pipelineDataName pipelineData)
        logTrivialInfo $ "    renderPass, pipeline : " ++ show renderPassPipelineDataName
        return (renderPassPipelineDataName, (renderPassData, pipelineData))
    return MaterialData
        { _materialDataName = materialDataName
        , _renderPassPipelineDataMap = Map.fromList renderPassPipelineDataMap
        , _materialParameterMap = materialParameterMap
        }

destroyMaterial :: MaterialData -> IO ()
destroyMaterial materialData = return ()

getRenderPassPipelineData :: MaterialData -> RenderPass.RenderPassPipelineDataName -> (RenderPass.RenderPassData, RenderPass.PipelineData)
getRenderPassPipelineData materialData renderPassPipelineDataName = Maybe.fromJust $ Map.lookup renderPassPipelineDataName (_renderPassPipelineDataMap materialData)