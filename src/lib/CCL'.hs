{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Connected Component Labeling, single pass also finds highest component value
-- Implementation of "One component at a time" from https://en.wikipedia.org/wiki/Connected-component_labeling, but no need for stack for depth-first search

module CCL'
    ( highestComponentValue
    , toLabeled
    )
    where

import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.Unsafe as U
import MassivExtensions (iFoldlMutM)

import Data.List as L
import Control.Monad.ST

import CCL_def (Connectivity(..), Label, PixelVal, Pixel, PixelL, Image, ImageL, ImageSize)
import CCL_unexposed (asssignLabel, defaultLabel, incrementLabel, isLabelable, neighborOffsets, withDefaultLabels)

type Acc = (Label, PixelVal)


highestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> PixelVal
highestComponentValue con arr = x 
    where (x, _) = toLabeled_highestComponentValue con arr


toLabeled :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> ImageL
toLabeled con arr = x
    where (_, x) = toLabeled_highestComponentValue con arr


toLabeled_highestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
toLabeled_highestComponentValue con arr = runST $ toLabeledM con arr


toLabeledM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Image -> m (PixelVal, ImageL)
toLabeledM con arr = do
    marr <- thawS $ computeAs U $ withDefaultLabels arr
    let sz = msize marr

    let
        f :: Ix2 -> Acc -> m Acc
        f ix acc = tryToLabelNext con sz marr ix acc
      
    (_, val) <- iFoldlMutM f (incrementLabel defaultLabel, 0) marr
    imageL <- unsafeFreeze (getComp arr) marr
    pure (val, imageL)


tryToLabelNext :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabelNext = tryToLabel False


tryToLabelNeighbor :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabelNeighbor = tryToLabel True


tryToLabel :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Bool -> Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabel isNeighbor con sz marr ix lv@(l, v_current) = do       
    e <- unsafeRead marr ix

    if isLabelable e then do
        (v, _) <- asssignLabel marr ix l
        let v_contender = if isNeighbor then v_current else 0   
        acc <- handleNeighbors con sz marr ix (l, v_contender + v)
        let (_, v') = acc

        if isNeighbor then pure acc
        else pure $ (incrementLabel l, max v_current v')
    else 
        pure lv


handleNeighbors :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> ImageSize -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
handleNeighbors con sz marr ix acc = do   
    let
        f :: m Acc -> Ix2 -> m Acc
        f mAcc offset = do
            let ix' = ix + offset

            if isSafeIndex sz ix' then do
                acc' <- mAcc
                tryToLabelNeighbor con sz marr ix' acc'
            else 
                mAcc

    L.foldl' f (pure acc) $ neighborOffsets con    