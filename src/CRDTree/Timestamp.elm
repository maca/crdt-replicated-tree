module CRDTree.Timestamp exposing (replicaId)
{-|
@docs replicaId
-}


{-| Extract the replica id from timestamp

     (replicaId 2) == 0
     (replicaId 4294967297) == 1

-}
replicaId : Int -> Int
replicaId timestamp =
  timestamp // 2 ^ 32
