module CRDTree.Node exposing
  ( Node
  , value
  , timestamp
  , path
  , isDeleted
  , children
  , descendant
  , init
  , root
  , tombstone
  , updateChildren
  )

{-| This module implements types and functions to build, find, get
values from, and compare nodes

@docs Node

# Properties
@docs value
@docs timestamp
@docs path
@docs isDeleted

# Traverse
@docs children
@docs descendant

# Build
@docs init
@docs root
@docs tombstone

# Update
@docs updateChildren

-}

import CRDTree.List as List
import Dict exposing (Dict)


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
 -}
type Node a
  = Root { children: List (Node a) }
  | Node { path: List Int
         , children: List (Node a)
         , value: a
         }
  | Tombstone { path: List Int }


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
    Root record -> record.children
    Node record -> record.children
    Tombstone _ -> []


{-| Find a node child matching a timestamp
-}
child : Int -> Node a -> Maybe (Node a)
child ts n =
  children n |> List.find (timestamp >> ((==) ts))


{-| Return a Node at a path
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
    Root _ -> Nothing
    Node rec -> Just rec.value
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
path : Node a -> List Int
path n =
  case n of
    Root _ -> [-1]
    Node rec -> rec.path
    Tombstone rec -> rec.path


{-| Return `True` if the node has been deleted
-}
isDeleted : Node a -> Bool
isDeleted n =
  case n of
    Root _ -> False
    Node _ -> False
    Tombstone _ -> True


{-| Update a Node's children
-}
updateChildren : List (Node a) -> Node a -> Node a
updateChildren ch node =
  case node of
    Root record ->
      Root { record | children = ch }

    Node record ->
      Node { record | children = ch }

    Tombstone _ -> node


