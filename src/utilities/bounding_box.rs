{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}


module HulkanEngine3D.Utilities.BoundingBox where

import GHC.Generics (Generic)
import Data.Text as Text ()
import Foreign.Storable

import Data.Aeson
import Numeric.DataFrame
import Numeric.PrimBytes

import HulkanEngine3D.Utilities.DataFrame ()

data BoundingBox = BoundingBox
    { _boundingBoxMin :: {-# UNPACK #-} !Vec3f
    , _boundingBoxMax :: {-# UNPACK #-} !Vec3f
    , _boundingBoxCenter :: {-# UNPACK #-} !Vec3f
    , _boundingBoxRadius :: {-# UNPACK #-} !Float
    } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance PrimBytes BoundingBox

instance Storable BoundingBox where
    sizeOf _ = bSizeOf (undefined :: BoundingBox)
    alignment _ = bAlignOf (undefined :: BoundingBox)
    peek ptr = bPeek ptr
    poke ptr vertexData = bPoke ptr vertexData

defaultBoundingBox ::  BoundingBox
defaultBoundingBox =
    let minValue = vec3 -1 -1 -1
        maxValue = vec3 1 1 1
        S radius = normL2 $ (maxValue - minValue)
    in BoundingBox
        { _boundingBoxMin = minValue
        , _boundingBoxMax = maxValue
        , _boundingBoxCenter = (minValue + maxValue) * 0.5
        , _boundingBoxRadius = radius
        }

calcBoundingBox :: [Vec3f] -> BoundingBox
calcBoundingBox [] = defaultBoundingBox
calcBoundingBox (position:positions) =
    let (minValue, maxValue) = calcBoundingBox' position position positions
        S radius = normL2 $ (maxValue - minValue)
    in BoundingBox
           { _boundingBoxMin = minValue
           , _boundingBoxMax = maxValue
           , _boundingBoxCenter = (minValue + maxValue) * 0.5
           , _boundingBoxRadius = radius
           }
    where
        calcBoundingBox' :: Vec3f -> Vec3f -> [Vec3f] -> (Vec3f, Vec3f)
        calcBoundingBox' boundMin boundMax [] = (boundMin, boundMax)
        calcBoundingBox' boundMin boundMax (position:positions) =
            let (# minX, minY, minZ #) = unpackV3# boundMin
                (# maxX, maxY, maxZ #) = unpackV3# boundMax
                minValue = vec3 (min minX maxX) (min minY maxY) (min minZ maxZ)
                maxValue = vec3 (max minX maxX) (max minY maxY) (max minZ maxZ)
            in
                calcBoundingBox' minValue maxValue positions
