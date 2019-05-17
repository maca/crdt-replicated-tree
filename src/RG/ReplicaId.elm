module RG.ReplicaId exposing (ReplicaId, fromInt, toInt)


{-| The id for a replica
-}
type ReplicaId = ReplicaId Int


fromInt : Int -> ReplicaId
fromInt id =
  ReplicaId id


toInt : ReplicaId -> Int
toInt (ReplicaId id) =
  id
