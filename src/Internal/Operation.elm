module Internal.Operation exposing
    ( Operation(..)
    , fromList
    , merge
    , path
    , replicaId
    , since
    , timestamp
    , toList
    )

import CRDTree.Timestamp as Timestamp


{-| Represents an CRDTree operation
-}
type Operation a
    = Add Int (List Int) a
    | Delete (List Int)
    | Batch (List (Operation a))


{-| Return operations since a timestamp
-}
since : Int -> List (Operation a) -> List (Operation a)
since ts operations =
    sinceFold ts operations []


sinceFold :
    Int
    -> List (Operation a)
    -> List (Operation a)
    -> List (Operation a)
sinceFold ts operations acc =
    case operations of
        [] ->
            []

        o :: os ->
            case o of
                Batch _ ->
                    sinceFold ts os acc

                Delete _ ->
                    sinceFold ts os (o :: acc)

                Add operationTimestamp _ _ ->
                    if ts == operationTimestamp then
                        o :: acc

                    else
                        sinceFold ts os (o :: acc)


{-| Operation to List of Operations
-}
toList : Operation a -> List (Operation a)
toList operation =
    case operation of
        Add _ _ _ ->
            [ operation ]

        Delete _ ->
            [ operation ]

        Batch list ->
            list


{-| List of Operations to Batch
-}
fromList : List (Operation a) -> Operation a
fromList operations =
    Batch operations


{-| Merge two operations
-}
merge : Operation a -> Operation a -> Operation a
merge a b =
    Batch (toList a ++ toList b)


{-| Id of the replica originating the operation
-}
replicaId : Operation a -> Maybe Int
replicaId operation =
    timestamp operation |> Maybe.map Timestamp.replicaId


{-| Timestamp of the operation
-}
timestamp : Operation a -> Maybe Int
timestamp operation =
    case operation of
        Add ts _ _ ->
            Just ts

        Delete p ->
            List.reverse p |> List.head

        Batch _ ->
            Nothing


{-| Operation path
-}
path : Operation a -> Maybe (List Int)
path operation =
    case operation of
        Add _ p _ ->
            Just p

        Delete p ->
            Just p

        Batch _ ->
            Nothing
