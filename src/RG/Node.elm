module RG.Node exposing
  ( Node(..)
  , Path
  , init
  , root
  , tombstone
  , children
  , data
  , timestamp
  , path
  , descendant
  , hasTimestamp
  )

{-| This module implements types and functions to build, find, get
values from, and compare nodes

@docs Node
@docs Path
@docs init
@docs root
@docs tombstone
@docs children
@docs data
@docs timestamp
@docs path
@docs descendant
@docs hasTimestamp

-}

import RG.List as List
import Dict exposing (Dict)

{-| The path of the node is represented as a list of integers
-}
type alias Path =
  List Int


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
 -}
type Node a
  = Node { path: Path
         , children: List (Node a)
         , data: Maybe a
         }
  | Tombstone { path: Path }


{-| Build a node

    timestamp (node (Just 'a') [0, 1]) == 1
    path (node (Just 'a') [0, 1]) == [0, 1]
    data (node (Just 'a') [0, 1]) == Just 'a'
 -}
init : Maybe a -> Path -> Node a
init maybeA p =
  Node
    { data = maybeA
    , path = p
    , children = []
    }


{-| Build a root node

    timestamp root == 0
    path root == []
    data root == Nothing
 -}
root : Node a
root =
  init Nothing []


{-| Build a tombstone providing timestamp and path

    timestamp (tombstone [0, 1]) == 1
    path (tombstone [0, 1]) == [0, 1]
    data (tombstone [0, 1]) == Nothing
-}
tombstone : Path -> Node a
tombstone p =
  Tombstone { path = p }


{-| Return a list of a nodes' children
-}
children : Node a -> List (Node a)
children n =
  case n of
    Node record -> record.children
    Tombstone _ -> []


{-| Return `Just` a nodes' children if found by timestamp or `Nothing`
-}
child : Int -> Node a -> Maybe (Node a)
child ts n =
  children n |> List.find (timestamp >> ((==) ts))


{-| Return `Just` a nodes' descendant if found by path or `Nothing`
-}
descendant : Path -> Node a -> Maybe (Node a)
descendant nodePath n =
  case nodePath of
    [] ->
      Nothing

    [ts] ->
      child ts n

    ts :: tss ->
      child ts n |> Maybe.andThen (descendant tss)


{-| Return the data of a node

    data (node (Just 'a') [0, 1]) == Just 'a'
    data (tombstone [0, 1]) == Nothing
-}
data : Node a -> Maybe a
data n =
  case n of
    Node rec -> rec.data
    Tombstone _ -> Nothing


{-| Return the timestamp of a node

    timestamp (node (Just 'a') [0, 1]) == 1
-}
timestamp : Node a -> Int
timestamp n =
  path n |> List.reverse |> List.head |> Maybe.withDefault 0


{-| Return the path of a node

    path (node (Just 'a') [0, 1]) == [0, 1]
-}
path : Node a -> Path
path n =
  case n of
    Node rec -> rec.path
    Tombstone rec -> rec.path


{-| Determine wether a node has a timestamp

    hasTimestamp 1 (node (Just 'a') [0, 1]) == True
    hasTimestamp 1 (node (Just 'a') [0, 2]) == False
-}
hasTimestamp : Int -> Node a -> Bool
hasTimestamp ts n =
  (timestamp n) == ts


