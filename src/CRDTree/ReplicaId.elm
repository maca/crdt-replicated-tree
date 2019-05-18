module CRDTree.ReplicaId exposing (ReplicaId, fromInt, toInt)
{-|
@docs ReplicaId
@docs toInt
@docs fromInt
-}


{-| The id for a replica
-}
type ReplicaId = ReplicaId Int


{-| Convert an Int to ReplicaId

    toInt (fromInt 1) == 1

-}
fromInt : Int -> ReplicaId
fromInt id =
  ReplicaId id


{-| Convert a ReplicaId to Int

    toInt (fromInt 1) == 1
-}
toInt : ReplicaId -> Int
toInt (ReplicaId id) =
  id
