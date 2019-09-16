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


asssignLabelsM :: forall m . (Source U Ix2 Pixel, Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> Image -> m ImageL
asssignLabelsM con arr = do
    let
        withDefaultLabels :: Array U Ix2 Pixel -> Array D Ix2 PixelL
        withDefaultLabels arr = A.map (\x -> (x, defaultLabel)) arr

    marr <- thawS $ computeAs U $ withDefaultLabels arr

    let
        f :: Ix2 -> Label -> PixelL -> m Label
        f ix acc e = tryToLabelNext con marr ix acc
      
    _ <- ifoldlMutM f (incrementLabel defaultLabel) marr
    unsafeFreeze (getComp arr) marr


tryToLabelNext :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNext a b c d = tryToLabel False a b c d


tryToLabelNeighbor :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabelNeighbor a b c d = tryToLabel True a b c d


tryToLabel :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Bool -> Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m Label
tryToLabel isNeighbor con marr ix l = do 
    let
        isLabelable :: PixelL -> Bool
        isLabelable x = (isForeground x) && not (isLabeled x)
            where
                isForeground (y, _) = y > 0
                isLabeled (_, y) = y > defaultLabel

        asssignLabel :: Label -> m (PixelL)   
        asssignLabel y = modifyM marr (\(x, _) -> pure (x, y)) ix

    e <- readM marr ix
    if isLabelable e then do
        _ <- asssignLabel l
        _ <- handleNeighbors con marr ix l

        if isNeighbor then pure l
        else pure $ incrementLabel l
    else 
        pure l


handleNeighbors :: forall m . (Mutable U Ix2 PixelL, PrimMonad m, MonadThrow m) => Connectivity -> MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m ()
handleNeighbors con marr ix l = do   
    let
        f :: m () -> Ix2 -> m ()
        f acc offset = do
            let ix' = ix + offset
            mbE <- M.read marr ix'
            case mbE of
                Just _ -> do
                    _ <- tryToLabelNeighbor con marr ix' l
                    acc
                Nothing -> acc

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