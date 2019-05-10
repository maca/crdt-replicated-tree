module RGA.Node exposing
  ( Node(..)
  , Path
  , root
  , node
  , tombstone
  , children
  , data
  , timestamp
  , path
  , descendant
  , match
  )

{-| Functions and types for modeling a graph

@docs Node
@docs Path
@docs root
@docs node
@docs tombstone
@docs children
@docs data
@docs timestamp
@docs path
@docs descendant
@docs match

-}

import RGA.List as List
import Dict exposing (Dict)

{-| Node's path is represented as a list of integers
-}
type alias Path =
  List Int


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` wich represents a removed Node
 -}
type Node a
  = Node (Payload a)
  | Tombstone { timestamp: Int, path: Path }


type alias Payload a =
  { data: Maybe a
  , children: List (Node a)
  , timestamp: Int
  , path: Path
  }


{-| Build a graph root node

    path root == []
    data root == Nothing
    timestamp root == 0
 -}
root : Node a
root =
  node Nothing 0 []


{-| Given `Maybe` data, the timestamp and path build a
graph node

    timestamp (node (Just 'a') 1 [0, 1]) == 1
    path (node (Just 'a') 1 [0, 1]) == [0, 1]
    data (node (Just 'a') 1 [0, 1]) == Just 'a'
 -}
node : Maybe a -> Int -> Path -> Node a
node maybeA ts p =
  Node
    { data = maybeA
    , timestamp = ts
    , path = p
    , children = []
    }


{-| Build a tombstone provided a timestamp and path
-}
tombstone : Int -> Path -> Node a
tombstone ts p =
  Tombstone { timestamp = ts, path = p }


{-| Return a list of children for a given node
-}
children : Node a -> List (Node a)
children n =
  case n of
    Node record -> record.children
    Tombstone _ -> []


{-| Return `Just` a children or `Nothing` if the timestamp does
not match any children's timestamp
-}
child : Node a -> Int -> Maybe (Node a)
child n ts =
  children n |> List.find (timestamp >> ((==) ts))


{-| Return `Just` a descendant or `Nothing` if there is no
descendant with the given path
-}
descendant : Path -> Node a -> Maybe (Node a)
descendant nodePath n =
  case nodePath of
    [] ->
      Nothing

    [ts] ->
      child n ts

    ts :: tss ->
      child n ts |> Maybe.andThen (descendant tss)


{-| Return the payload of a node
-}
data : Node a -> Maybe a
data n =
  case n of
    Node rec -> rec.data
    Tombstone _ -> Nothing


{-| Return the timestamp of a node
-}
timestamp : Node a -> Int
timestamp n =
  case n of
    Node rec -> rec.timestamp
    Tombstone rec -> rec.timestamp


{-| Return the path of a node
-}
path : Node a -> Path
path n =
  case n of
    Node rec -> rec.path
    Tombstone rec -> rec.path


{-| Determine wether two nodes have the same timestamp
-}
match : Int -> Node a -> Bool
match ts n =
  (timestamp n) == ts


