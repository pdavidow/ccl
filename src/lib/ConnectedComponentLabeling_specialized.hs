{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConnectedComponentLabeling_specialized
    ( Connectivity(..)
    , Label
    , Pixel
    , PixelL
    , Image
    , ImageL
    , asssignLabels_HighestComponentValue
    )
    where

-- Implementation of "One component at a time" from https://en.wikipedia.org/wiki/Connected-component_labeling, but no need for stack for depth-first search

import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.Unsafe as U
import MassivExtensions (ifoldlMutM)

import Data.Function ( (&) ) 
import Data.List as L
import Control.Monad.ST
import qualified Data.Map.Strict as Map
import Safe (lastMay)

type Label = Int
type PixelVal = Int
type Pixel = PixelVal
type PixelL = (Pixel, Label)
type PixelLIx = (PixelL, Ix2)
type Image = Array U Ix2 Pixel
type ImageL = Array U Ix2 PixelL     
type Acc = (Label, PixelVal)

data Connectivity = Connect_4 | Connect_8 deriving (Eq, Show)


asssignLabels_HighestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels_HighestComponentValue con arr = asssignLabels con arr


asssignLabels :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels con arr = runST $ asssignLabelsM con arr


asssignLabelsM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> Image -> m (PixelVal, ImageL)
asssignLabelsM con arr = do
    let
        withDefaultLabels :: Array U Ix2 Pixel -> Array D Ix2 PixelL
        withDefaultLabels arr = A.map (\x -> (x, defaultLabel)) arr

    marr <- thawS $ computeAs U $ withDefaultLabels arr

    let
        f :: Ix2 -> Acc -> PixelL -> m Acc
        f ix acc e = tryToLabelNext con marr ix acc
      
    (_, val) <- ifoldlMutM f (incrementLabel defaultLabel, 0) marr
    imageL <- unsafeFreeze (getComp arr) marr
    pure (val, imageL)


tryToLabelNext :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabelNext a b c d = tryToLabel False a b c d


tryToLabelNeighbor :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabelNeighbor a b c d = tryToLabel True a b c d


tryToLabel :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Bool -> Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
tryToLabel isNeighbor con marr ix lv@(l, v_current) = do 
    let
        v_contender = if isNeighbor then v_current else 0        

        isLabelable :: PixelL -> Bool
        isLabelable x = (isForeground x) && not (isLabeled x)
            where
                isForeground (y, _) = y > 0
                isLabeled (_, y) = y > defaultLabel

        asssignLabel :: Label -> m (PixelL)   
        asssignLabel y = modifyM marr (\(x, _) -> pure (x, y)) ix

    e <- readM marr ix
    if isLabelable e then do
        (v, _) <- asssignLabel l
        acc <- handleNeighbors con marr ix (l, v_contender + v)
        let (_, v') = acc

        if isNeighbor then pure acc
        else pure $ (incrementLabel l, max v_current v')
    else 
        pure lv


handleNeighbors :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Acc -> m Acc
handleNeighbors con marr ix acc = do   
    let
        f :: m Acc -> Ix2 -> m Acc
        f mAcc offset = do
            let ix' = ix + offset
            mbE <- M.read marr ix'
            case mbE of
                Just e -> do
                    acc' <- mAcc
                    tryToLabelNeighbor con marr ix' acc'
                Nothing -> 
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