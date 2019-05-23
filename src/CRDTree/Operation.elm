module CRDTree.Operation exposing
  ( Operation(..)
  , replicaId
  , timestamp
  , path
  , since
  , toList
  , merge
  )

{-| Represents an CRDTree operation

@docs Operation

# Properties
@docs timestamp
@docs path
@docs replicaId

# List
@docs since
@docs toList

# Manipulation
@docs merge


-}

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
sinceFold ts operations acc =
  case operations of
    [] -> []

    o :: os ->
      case o of
        Batch _ ->
          sinceFold ts os acc

        Delete _ _ ->
          sinceFold ts os (o :: acc)

        Add _ operationTimestamp _ _ ->
          if ts == operationTimestamp then
            o :: acc

          else
            sinceFold ts os (o :: acc)


{-|
List of operations

-}
toList : Operation a -> List (Operation a)
toList operation =
  case operation of
    Add _ _ _ _ -> [ operation ]
    Delete _ _ -> [ operation ]
    Batch list -> list


{-| Merge two operations
-}
merge : Operation a -> Operation a -> Operation a
merge a b =
  Batch ((toList a) ++ (toList b))


{-| Id of the replica originating the operation
-}
replicaId : Operation a -> Maybe Int
replicaId operation =
  case operation of
    Add id _ _ _ -> Just (ReplicaId.toInt id)
    Delete id _ -> Just (ReplicaId.toInt id)
    Batch list -> Nothing


{-| Timestamp of the operation
-}
timestamp : Operation a -> Maybe Int
timestamp operation =
  case operation of
    Add _ ts _ _ ->
      Just ts

    Delete _ p ->
      List.reverse p |> List.head

    Batch _ ->
      Nothing


{-| Operation path
-}
path : Operation a -> Maybe (List Int)
path operation =
  case operation of
    Add _ _ p _ -> Just p
    Delete _ p -> Just p
    Batch _ -> Nothing
