{-# LANGUAGE FlexibleContexts #-}

module CCL_unexposed
    ( asssignLabel
    , defaultLabel
    , incrementLabel
    , isLabelable
    , neighborOffsets
    , withDefaultLabels
    )
    where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as U
import CCL_def (Connectivity(..), Label, Pixel, PixelL)

defaultLabel :: Label
defaultLabel = 0


incrementLabel :: Label -> Label
incrementLabel x = x + 1        


withDefaultLabels :: Array U Ix2 Pixel -> Array D Ix2 PixelL
withDefaultLabels arr = A.map (\x -> (x, defaultLabel)) arr


isLabelable :: PixelL -> Bool
isLabelable x = (isForeground x) && not (isLabeled x)
    where
        isForeground (y, _) = y > 0
        isLabeled (_, y) = y > defaultLabel


asssignLabel :: (Mutable U Ix2 PixelL, PrimMonad m) => MArray (PrimState m) U Ix2 PixelL -> Ix2 -> Label -> m (PixelL)   
asssignLabel marr ix l = unsafeModify marr (\(x, _) -> pure (x, l)) ix


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