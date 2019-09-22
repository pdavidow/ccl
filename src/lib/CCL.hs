{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Connected Component Labeling, with second pass for finding highest component value
-- Implementation of "One component at a time" from https://en.wikipedia.org/wiki/Connected-component_labeling, but no need for stack for depth-first search

module CCL
    ( asssignLabels
    , asssignLabels_HighestComponentValue
    )
    where

import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.Unsafe as U
import MassivExtensions (iFoldlMutM)

import Data.Function ( (&) ) 
import Data.List as L
import Control.Monad.ST

import CCL_def (Connectivity(..), Label, PixelVal, Pixel, PixelL, Image, ImageL, ImageSize)
import CCL_unexposed (asssignLabel, defaultLabel, incrementLabel, isLabelable, neighborOffsets, withDefaultLabels)
import CCL_Util (highestComponentValue)


asssignLabels_HighestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels_HighestComponentValue con arr = (highestComponentValue imageL, imageL)
    where imageL = asssignLabels con arr


asssignLabels :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> ImageL
asssignLabels con arr = runST $ asssignLabelsM con arr


asssignLabelsM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Image -> m ImageL
asssignLabelsM con arr = do
    marr <- thawS $ computeAs U $ withDefaultLabels arr
    let sz = msize marr

    let
        f :: Ix2 -> Label -> m Label
        f ix acc = tryToLabelNext con sz marr ix acc
      
    _ <- iFoldlMutM f (incrementLabel defaultLabel) marr
    unsafeFreeze (getComp arr) marr


tryToLabelNext :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNext = tryToLabel False


tryToLabelNeighbor :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNeighbor = tryToLabel True


tryToLabel :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Bool -> Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabel isNeighbor con sz marr ix l = do 
    e <- unsafeRead marr ix

    if isLabelable e then do
        _ <- asssignLabel marr ix l
        _ <- handleNeighbors con sz marr ix l

        if isNeighbor then pure l
        else pure $ incrementLabel l
    else 
        pure l


handleNeighbors :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m ()
handleNeighbors con sz marr ix l = do   
    let
        f :: m () -> Ix2 -> m ()
        f acc offset = do
            let ix' = ix + offset

            if isSafeIndex sz ix' then do
                _ <- tryToLabelNeighbor con sz marr ix' l
                acc
            else 
                acc

    L.foldl' f (pure ()) $ neighborOffsets con