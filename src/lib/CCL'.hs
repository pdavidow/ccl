{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Connected Component Labeling, specialized in single pass for finding highest component value

module CCL'
    ( asssignLabels_HighestComponentValue
    )
    where

-- Implementation of "One component at a time" from https://en.wikipedia.org/wiki/Connected-component_labeling, but no need for stack for depth-first search

import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.Unsafe as U
import MassivExtensions (iFoldlMutM)

import Data.List as L
import Control.Monad.ST

import CCL_Shared (Connectivity(..), Label, PixelVal, Pixel, PixelL, Image, ImageL, ImageSize)

type Acc = (Label, PixelVal)


asssignLabels_HighestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels_HighestComponentValue = asssignLabels


asssignLabels :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels con arr = runST $ asssignLabelsM con arr


asssignLabelsM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Image -> m (PixelVal, ImageL)
asssignLabelsM con arr = do
    let
        withDefaultLabels :: Array U Ix2 Pixel -> Array D Ix2 PixelL
        withDefaultLabels arr = A.map (\x -> (x, defaultLabel)) arr

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
    let
        v_contender = if isNeighbor then v_current else 0        

        isLabelable :: PixelL -> Bool
        isLabelable x = (isForeground x) && not (isLabeled x)
            where
                isForeground (y, _) = y > 0
                isLabeled (_, y) = y > defaultLabel

        asssignLabel :: Label -> m (PixelL)   
        asssignLabel y = unsafeModify marr (\(x, _) -> pure (x, y)) ix

    e <- unsafeRead marr ix
    if isLabelable e then do
        (v, _) <- asssignLabel l
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


neighborOffsets :: Connectivity -> [Ix2]  
neighborOffsets con = 
    -- row-major order (as usual)
    let
        offsets4 :: [Ix2]
        offsets4 = 
            [              (-1 :.  0 )            
            , ( 0 :. -1 ),              ( 0 :. 1 ) 
            ,              ( 1 :.  0 )            
            ]        
        {-# INLINE offsets4 #-}

        
        offsets8 :: [Ix2]
        offsets8 = 
            [ (-1 :. -1 ), (-1 :.  0 ), (-1 :. 1 ) 
            , ( 0 :. -1 ),              ( 0 :. 1 ) 
            , ( 1 :. -1 ), ( 1 :.  0 ), ( 1 :. 1 ) 
            ]       
        {-# INLINE offsets8 #-}                   
    in
        case con of
            Connect_4 -> offsets4
            Connect_8 -> offsets8       


defaultLabel :: Label
defaultLabel = 0


incrementLabel :: Label -> Label
incrementLabel x = x + 1