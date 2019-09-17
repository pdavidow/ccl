module MassivExtensions
    ( iFoldlMutM
    )
    where  

import Data.Massiv.Array                  


iFoldlMutM :: (Mutable r ix a, PrimMonad m) =>  (ix -> t -> m t) -> t -> Sz ix -> MArray (PrimState m) r ix a -> m t
iFoldlMutM f acc0 sz marr = 
    foldlM 
        (\acc i -> f (fromLinearIndex sz i) acc) 
        acc0 
        (0 ..: totalElem sz)          