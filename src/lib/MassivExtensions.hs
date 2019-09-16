module MassivExtensions
    ( ifoldlMutM
    )
    where  

import Data.Massiv.Array
import Data.Massiv.Array.Unsafe 


-- provided by Alexey Kuleshevich
ifoldlMutM :: (Mutable r ix a, PrimMonad m) =>  (ix -> t -> a -> m t) -> t -> MArray (PrimState m) r ix a -> m t
ifoldlMutM f acc0 marr = 
    foldlM 
        (\acc i -> unsafeLinearRead marr i >>= f (fromLinearIndex (msize marr) i) acc) 
        acc0 
        (0 ..: totalElem (msize marr))                       