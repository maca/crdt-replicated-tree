module CRDTree.Operation exposing
  ( Operation(..)
  , since
  , toList
  , merge
  )

import CRDTree.Node as Node
import CRDTree.ReplicaId as ReplicaId exposing (ReplicaId)


{-| Represents an CRDTree operation
-}
type Operation a
  = Add ReplicaId Int (List Int) a
  | Delete ReplicaId (List Int)
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


