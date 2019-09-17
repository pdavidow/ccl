module MassivExtensions
    ( iFoldlMutM
    )
    where  

import Data.Massiv.Array                  


iFoldlMutM :: (Mutable r ix a, PrimMonad m) =>  (ix -> t -> m t) -> t -> MArray (PrimState m) r ix a -> m t
iFoldlMutM f acc0 marr = 
    foldlM 
        (\acc i -> f (fromLinearIndex (msize marr) i) acc) 
        acc0 
        (0 ..: totalElem (msize marr))                        