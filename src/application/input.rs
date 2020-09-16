{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HulkanEngine3D.Application.Input
    ( KeyboardInputData (..)
    , KeyboardInputInterface (..)
    , MouseInputData (..)
    , MouseMoveData (..)
    , newKeyboardInputData
    , newMouseInputData
    , newMouseMoveData
    ) where

import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.HashTable.IO as HashTable
import qualified Graphics.UI.GLFW as GLFW
import Numeric.DataFrame

instance Hashable GLFW.Key

type KeyMap = HashTable.BasicHashTable GLFW.Key Bool

data KeyboardInputData = KeyboardInputData
    { _keyboardDown :: Bool
    , _keyboardPressed :: Bool
    , _keyboardUp :: Bool
    , _keyPressedMap :: KeyMap
    , _keyReleasedMap :: KeyMap
    , _modifierKeys :: GLFW.ModifierKeys
    } deriving (Show)

data MouseMoveData = MouseMoveData
    { _mousePos :: Vec2i
    , _mousePosPrev :: Vec2i
    , _mousePosDelta :: Vec2i
    , _scroll_xoffset :: Float
    , _scroll_yoffset :: Float
    } deriving (Show)

data MouseInputData = MouseInputData
    { _btn_l_down :: Bool
    , _btn_m_down :: Bool
    , _btn_r_down :: Bool
    , _btn_l_up :: Bool
    , _btn_m_up :: Bool
    , _btn_r_up :: Bool
    } deriving (Show)


class KeyboardInputInterface a where
    getKeyPressed :: a -> GLFW.Key -> IO Bool
    getKeyReleased :: a -> GLFW.Key -> IO Bool

instance KeyboardInputInterface KeyboardInputData where
    getKeyPressed keyboardInputData key = fromMaybe False <$> HashTable.lookup (_keyPressedMap keyboardInputData) key
    getKeyReleased keyboardInputData key = fromMaybe False <$> HashTable.lookup (_keyReleasedMap keyboardInputData) key

newKeyboardInputData :: IO KeyboardInputData
newKeyboardInputData = do
    keyPressed <- HashTable.new
    keyReleased <- HashTable.new
    return KeyboardInputData
            { _keyboardDown = False
            , _keyboardPressed = False
            , _keyboardUp = False
            , _keyPressedMap = keyPressed
            , _keyReleasedMap = keyReleased
            , _modifierKeys = GLFW.ModifierKeys
                { GLFW.modifierKeysShift = False
                , GLFW.modifierKeysControl = False
                , GLFW.modifierKeysAlt = False
                , GLFW.modifierKeysSuper = False
                }
            }

newMouseInputData :: MouseInputData
newMouseInputData =
    MouseInputData
        { _btn_l_down = False
        , _btn_m_down = False
        , _btn_r_down = False
        , _btn_l_up = False
        , _btn_m_up = False
        , _btn_r_up = False
        }

newMouseMoveData :: Vec2i -> MouseMoveData
newMouseMoveData mousepos =
    MouseMoveData
        { _mousePos = mousepos
        , _mousePosPrev = mousepos
        , _mousePosDelta = (vec2 0 0 :: Vec2i)
        , _scroll_xoffset = 0.0
        , _scroll_yoffset = 0.0
        }