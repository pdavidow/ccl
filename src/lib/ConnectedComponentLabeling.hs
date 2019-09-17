{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConnectedComponentLabeling
    ( Connectivity(..)
    , Label
    , Pixel
    , PixelL
    , Image
    , ImageL
    , asssignLabels
    , asssignLabels_HighestComponentValue
    )
    where

-- Implementation of "One component at a time" from https://en.wikipedia.org/wiki/Connected-component_labeling, but no need for stack for depth-first search

import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.Unsafe as U
import MassivExtensions (iFoldlMutM)

import Data.Function ( (&) ) 
import Data.List as L
import Control.Monad.ST
import qualified Data.Map.Strict as Map
import Safe (lastMay)

type Label = Int
type PixelVal = Int
type Pixel = PixelVal
type PixelL = (Pixel, Label)
type Image = Array U Ix2 Pixel
type ImageL = Array U Ix2 PixelL     
type CMap = Map.Map Label PixelVal

data Connectivity = Connect_4 | Connect_8 deriving (Eq, Show)


asssignLabels_HighestComponentValue :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> (PixelVal, ImageL)
asssignLabels_HighestComponentValue con arr = 
    let 
        imageL = asssignLabels con arr

        components :: CMap
        components = A.foldlS f Map.empty imageL
            where
                f :: CMap -> PixelL -> CMap
                f acc (n, l) = 
                    if l == defaultLabel then acc
                    else Map.insertWith (+) l n acc   


        highest :: PixelVal
        highest = 
            components
                & Map.elems
                & L.sort
                & lastMay
                & maybe 0 id
    in
        (highest, imageL)


asssignLabels :: (Source U Ix2 Pixel, Mutable U Ix2 PixelL) => Connectivity -> Image -> ImageL
asssignLabels con arr = runST $ asssignLabelsM con arr


asssignLabelsM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Image -> m ImageL
asssignLabelsM con arr = do
    let
        withDefaultLabels :: Array U Ix2 Pixel -> Array D Ix2 PixelL
        withDefaultLabels arr = A.map (\x -> (x, defaultLabel)) arr

    marr <- thawS $ computeAs U $ withDefaultLabels arr
    let sz = msize marr

    let
        f :: Ix2 -> Label -> m Label
        f ix acc = tryToLabelNext con sz marr ix acc
      
    _ <- iFoldlMutM f (incrementLabel defaultLabel) marr
    unsafeFreeze (getComp arr) marr


tryToLabelNext :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Sz Ix2 -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNext = tryToLabel False


tryToLabelNeighbor :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Sz Ix2 -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNeighbor = tryToLabel True


tryToLabel :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Bool -> Connectivity -> Sz Ix2 -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabel isNeighbor con sz marr ix l = do 
    let
        isLabelable :: PixelL -> Bool
        isLabelable x = (isForeground x) && not (isLabeled x)
            where
                isForeground (y, _) = y > 0
                isLabeled (_, y) = y > defaultLabel

        asssignLabel :: Label -> m (PixelL)   
        asssignLabel y = unsafeModify marr (\(x, _) -> pure (x, y)) ix

    e <- unsafeRead marr ix
    if isLabelable e then do
        _ <- asssignLabel l
        _ <- handleNeighbors con sz marr ix l

        if isNeighbor then pure l
        else pure $ incrementLabel l
    else 
        pure l


handleNeighbors :: forall m . (Mutable U Ix2 PixelL, PrimMonad m) => Connectivity -> Sz Ix2 -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m ()
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