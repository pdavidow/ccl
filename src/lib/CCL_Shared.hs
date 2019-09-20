module CCL_Shared
    ( Connectivity(..)
    , Label
    , PixelVal
    , Pixel
    , PixelL
    , Image
    , ImageL
    , ImageSize
    )
    where

import Data.Massiv.Array as A

type Label = Int
type PixelVal = Int
type Pixel = PixelVal
type PixelL = (Pixel, Label)
type Image = Array U Ix2 Pixel
type ImageL = Array U Ix2 PixelL    
type ImageSize = Sz Ix2 


data Connectivity = Connect_4 | Connect_8 deriving (Eq, Show)