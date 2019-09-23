module CCL__util
    ( componentCount
    , componentSizes
    , componentValues
    , highestComponentValue
    , sumComponentSizes
    , sumComponentValues
    )
    where

import Data.Function ( (&) ) 
import Data.Massiv.Array as A
import Data.List as L
import Safe (lastMay)
import qualified Data.Map.Strict as Map

import CCL_def (Image, ImageL, Label, PixelL, PixelVal)
import CCL_unexposed (defaultLabel)

type CMap = Map.Map Label PixelVal


cFold :: ImageL -> (Int -> Int) -> CMap
cFold arr f = 
    let
        g :: CMap -> PixelL -> CMap
        g acc (x, l) =
            if l == defaultLabel then 
                acc
            else 
                Map.insertWith (+) l (f x) acc 
    in
        A.foldlS g Map.empty arr


componentSizes :: ImageL -> CMap
componentSizes arr = cFold arr $ const 1


componentValues :: ImageL -> CMap
componentValues arr = cFold arr id


sumComponentSizes :: ImageL -> Int
sumComponentSizes arr =
    componentSizes arr
        & Map.elems
        & L.sum


sumComponentValues :: ImageL -> Int
sumComponentValues arr =
    componentValues arr
        & Map.elems
        & L.sum


highestComponentValue :: ImageL -> PixelVal
highestComponentValue arr = 
    componentValues arr
        & Map.elems
        & L.sort
        & lastMay
        & maybe 0 id


componentCount :: ImageL -> Int
componentCount arr =       
    toList arr
        & L.sortOn f
        & lastMay
        & maybe 0 f
            where f = \ (_, l) -> l                    