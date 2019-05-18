module RG.Operation exposing
  ( Operation(..)
  , since
  , toList
  , merge
  )

import RG.Node as Node exposing (Path)
import RG.ReplicaId as ReplicaId exposing (ReplicaId)


{-| Represents an RG operation
-}
type Operation a
  = Add ReplicaId Int Path a
  | Delete ReplicaId Path
  | Batch (List (Operation a))


{-| Return operations since a timestamp
-}
since : Int -> List (Operation a) -> List (Operation a)
since ts operations =
  sinceFold ts operations []


sinceFold : Int -> List (Operation a)
                         -> List (Operation a)
                         -> List (Operation a)
sinceFold timestamp operations acc =
  case operations of
    [] -> []

    o :: os ->
      case o of
        Batch _ ->
          sinceFold timestamp os acc

        Delete _ _ ->
          sinceFold timestamp os (o :: acc)

        Add _ operationTimestamp _ _ ->
          if timestamp == operationTimestamp then
            o :: acc

          else
            sinceFold timestamp os (o :: acc)


toList : Operation a -> List (Operation a)
toList operation =
  case operation of
    Add _ _ _ _ -> [ operation ]
    Delete _ _ -> [ operation ]
    Batch list -> list


merge : Operation a -> Operation a -> Operation a
merge a b =
  Batch ((toList a) ++ (toList b))


