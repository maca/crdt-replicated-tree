module RG.Operation exposing
  ( Operation(..)
  , ReplicaId(..)
  , sinceTimestamp
  )

import RG.Node as Node exposing (Path)

{-| The id for a replica
-}
type ReplicaId = ReplicaId Int


{-| Represents an RG operation
-}
type Operation a
  = Add ReplicaId Int Path (Maybe a)
  | Delete ReplicaId Path
  | Batch (List (Operation a))


{-| Return operations since a timestamp
-}
sinceTimestamp : Int -> List (Operation a) -> List (Operation a)
sinceTimestamp ts operations =
  sinceTimestampFold ts operations []


sinceTimestampFold : Int -> List (Operation a)
                         -> List (Operation a)
                         -> List (Operation a)
sinceTimestampFold timestamp operations acc =
  case operations of
    [] -> []

    o :: os ->
      case o of
        Batch _ ->
          sinceTimestampFold timestamp os acc

        Delete _ _ ->
          sinceTimestampFold timestamp os (o :: acc)

        Add _ operationTimestamp _ _ ->
          if timestamp == operationTimestamp then
            o :: acc

          else
            sinceTimestampFold timestamp os (o :: acc)


