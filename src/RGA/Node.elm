module RGA.Node exposing
  ( Node(..)
  , NodeData
  , Path
  , root
  , node
  , tombstone
  , children
  , data
  , timestamp
  , path
  , get
  , match
  )

import RGA.List as List
import Dict exposing (Dict)


type alias Path =
  List Int


type alias NodeData a =
  { data: Maybe a
  , children: List (Node a)
  , timestamp: Int
  , path: Path
  }


type Node a
  = Node (NodeData a)
  | Tombstone { timestamp: Int, path: Path }


root : Node a
root =
  node Nothing 0 []


node : Maybe a -> Int -> Path -> Node a
node maybeA ts p =
  Node
    { data = maybeA
    , timestamp = ts
    , path = p
    , children = []
    }


tombstone : Int -> Path -> Node a
tombstone ts p =
  Tombstone { timestamp = ts, path = p }


children : Node a -> List (Node a)
children n =
  case n of
    Node record -> record.children
    Tombstone _ -> []


child : Node a -> Int -> Maybe (Node a)
child n ts =
  children n |> List.find (timestamp >> ((==) ts))


get : Path -> Node a -> Maybe (Node a)
get nodePath n =
  case nodePath of
    [] ->
      Nothing

    [ts] ->
      child n ts

    ts :: tss ->
      child n ts |> Maybe.andThen (get tss)


data : Node a -> Maybe a
data n =
  case n of
    Node rec -> rec.data
    Tombstone _ -> Nothing


timestamp : Node a -> Int
timestamp n =
  case n of
    Node rec -> rec.timestamp
    Tombstone rec -> rec.timestamp


path : Node a -> Path
path n =
  case n of
    Node rec -> rec.path
    Tombstone rec -> rec.path


match : Int -> Node a -> Bool
match ts n =
  (timestamp n) == ts


