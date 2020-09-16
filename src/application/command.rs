{-# LANGUAGE DeriveGeneric #-}

module HulkanEngine3D.Application.Command where

data Command = Command_None
             | Command_Resize_Window
             | Command_Close_App
             deriving (Enum, Eq, Ord, Show)