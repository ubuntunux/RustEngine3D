{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module HulkanEngine3D.Application.Editor
    ( runEditor
    ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import HulkanEngine3D.Application.Command

runEditor :: IORef Command -> IORef Command -> IO ()
runEditor commandToEditor commandToApp = startGUI defaultConfig (setup commandToEditor commandToApp)

setup :: IORef Command -> IORef Command -> Window -> UI ()
setup commandToEditor commandToApp w = do
    -- active elements
    return w # set title "HulkanEngine3D Editor"

    elResizeWindow <- UI.button # set UI.text "Resize Window"
    elCloseApp <- UI.button # set UI.text "Close App"
    elAdd <- UI.button # set UI.text "Add"
    elRemove <- UI.button # set UI.text "Remove"
    elResult <- UI.span

    inputs   <- liftIO $ newIORef []

    -- functionality
    let
        displayTotal = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs)
            element elResult # set UI.text (showNumber . sum $ map readNumber xs)

        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout]
            displayTotal

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [ column $
                [ element elResizeWindow
                , element elCloseApp
                , element elAdd
                , element elRemove
                ]
            , UI.hr
            ] ++
            map element xs ++
            [ UI.hr
            , row
                [ UI.span # set text "Sum: "
                , element elResult
                ]
            ]

        addInput :: UI ()
        addInput = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal
            liftIO $ modifyIORef inputs (elInput:)

        removeInput :: UI ()
        removeInput = liftIO $ modifyIORef inputs (drop 1)

    on UI.click elResizeWindow $ \_ -> liftIO $ atomicWriteIORef commandToApp Command_Resize_Window
    on UI.click elCloseApp $ \_ -> liftIO $ atomicWriteIORef commandToApp Command_Close_App
    on UI.click elAdd $ \_ -> addInput >> redoLayout
    on UI.click elRemove $ \_ -> removeInput >> redoLayout
    addInput >> redoLayout


{-----------------------------------------------------------------------------
    Functionality
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber :: String -> Number
readNumber s = listToMaybe [x | (x,"") <- reads s]

showNumber :: Number -> String
showNumber maybeNumber = maybe "--" show maybeNumber