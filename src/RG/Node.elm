module RG.Node exposing
  ( Node(..)
  , init
  , root
  , tombstone
  , children
  , value
  , timestamp
  , path
  , descendant
  , hasTimestamp
  , updateChildren
  )

{-| This module implements types and functions to build, find, get
values from, and compare nodes

@docs Node
@docs init
@docs root
@docs tombstone
@docs children
@docs value
@docs timestamp
@docs path
@docs descendant
@docs hasTimestamp

-}

import RG.List as List
import Dict exposing (Dict)


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
 -}
type Node a
  = Node { path: List Int
         , children: List (Node a)
         , value: a
         }
  | Tombstone { path: List Int }
  | Root { children: List (Node a) }


{-| Build a node

    timestamp (node (Just 'a') [0, 1]) == 1
    path (node (Just 'a') [0, 1]) == [0, 1]
    value (node (Just 'a') [0, 1]) == Just 'a'
 -}
init : a -> List Int -> Node a
init val p =
  Node
    { value = val
    , path = p
    , children = []
    }


{-| Build a root node

    timestamp root == 0
    path root == []
    value root == Nothing
 -}
root : Node a
root =
  Root { children = [] }


{-| Build a tombstone providing timestamp and path

    timestamp (tombstone [0, 1]) == 1
    path (tombstone [0, 1]) == [0, 1]
    value (tombstone [0, 1]) == Nothing
-}
tombstone : List Int -> Node a
tombstone p =
  Tombstone { path = p }


{-| Return a list of a nodes' children
-}
children : Node a -> List (Node a)
children n =
  case n of
    Node record -> record.children
    Tombstone _ -> []
    Root record -> record.children


{-| Return `Just` a nodes' children if found by timestamp or `Nothing`
-}
child : Int -> Node a -> Maybe (Node a)
child ts n =
  children n |> List.find (timestamp >> ((==) ts))


{-| Return `Just` a nodes' descendant if found by path or `Nothing`
-}
descendant : List Int -> Node a -> Maybe (Node a)
descendant nodePath n =
  case nodePath of
    [] ->
      Nothing

    [ts] ->
      child ts n

    ts :: tss ->
      child ts n |> Maybe.andThen (descendant tss)


{-| Return the value of a node

    value (node (Just 'a') [0, 1]) == Just 'a'
    value (tombstone [0, 1]) == Nothing
-}
value : Node a -> Maybe a
value n =
  case n of
    Node rec -> Just rec.value
    Tombstone _ -> Nothing
    Root _ -> Nothing


{-| Return the timestamp of a node

    timestamp (node (Just 'a') [0, 1]) == 1
-}
timestamp : Node a -> Int
timestamp n =
  path n |> List.reverse |> List.head |> Maybe.withDefault 0


{-| Return the path of a node

    path (node (Just 'a') [0, 1]) == [0, 1]
-}
path : Node a -> List Int
path n =
  case n of
    Node rec -> rec.path
    Tombstone rec -> rec.path
    Root _ -> [-1]


{-| Determine wether a node has a timestamp

    hasTimestamp 1 (node (Just 'a') [0, 1]) == True
    hasTimestamp 1 (node (Just 'a') [0, 2]) == False
-}
hasTimestamp : Int -> Node a -> Bool
hasTimestamp ts n =
  (timestamp n) == ts


updateChildren : List (Node a) -> Node a -> Node a
updateChildren ch node =
  case node of
    Root record ->
      Root { record | children = ch }

    Node record ->
      Node { record | children = ch }

    Tombstone _ -> node


