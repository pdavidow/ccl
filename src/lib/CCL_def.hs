{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module CCL_def
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

import Test.Tasty.QuickCheck as QC
import qualified Data.Vector as V

import Data.Massiv.Array as A

type Label = Int
type PixelVal = Int
type Pixel = PixelVal
type PixelL = (Pixel, Label)
type Image = Array U Ix2 Pixel
type ImageL = Array U Ix2 PixelL    
type ImageSize = Sz Ix2 


data Connectivity = Connect_4 | Connect_8 deriving (Eq, Show)


-- stack repl
-- > :load CCL_def.hs
-- > generate arbitrary :: IO Image

instance Arbitrary Image where
    arbitrary :: Gen Image
    arbitrary = do
        numCols <- choose (1, 1000)
        numRows <- choose (1, 1000)

        e1 <- choose (1, 1000)
        e2 <- choose (1, 1000)
        e3 <- choose (1, 1000)

        let g1 = choose (0, 1)
        let g2 = choose (0, 2)
        let g3 = choose (0, 9)
        let g4 = elements [0, e1]
        let g5 = elements [0, 0, e2]
        let g6 = elements [0, e1, e2, e3]
        let g7 = elements [0, 0, 0, e1, e2, e3]
        let g8 = frequency [(99, pure 0), (1, pure 1)]
        let g9 = frequency [(1, pure 0), (99, pure 1)]        
        let g10 = frequency [(99, pure 0), (1, choose (1, 1000000000))]
        let g11 = frequency [(1, pure 0), (99, choose (1, 1000000000))]
        let gs = [g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11]

        xs <- V.replicateM numRows $ vectorOf numCols $ oneof gs
        let xss = V.map (\i -> xs V.! (i - 1)) $ V.fromList [1 .. numRows]
        pure $ A.fromLists' Seq $ V.toList xss