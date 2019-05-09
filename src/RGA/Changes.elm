module RGA.Changes exposing
  ( Changes
  , empty
  , union
  )

import Dict exposing (Dict)

import RGA.Node as Node exposing(Node)


type alias Changes a =
  { additions: Dict Int (Node a)
  , deletions: Dict Int (Node a)
  , updates: Dict Int (Node a)
  , pointer: List Int
  , branch: Node a
  }


empty : Changes a
empty =
  { additions = Dict.empty
  , deletions = Dict.empty
  , updates = Dict.empty
  , pointer = []
  , branch = Node.root
  }


union : Changes a -> Changes a -> Changes a
union first second =
  let
      additions =
        Dict.intersect first.updates second.additions
        |> Dict.union first.additions
        |> (flip Dict.union) second.additions

      deletions =
        Dict.intersect first.updates second.deletions
        |> Dict.union first.deletions
        |> (flip Dict.union) second.deletions

      updates = Dict.union first.updates second.updates
  in
      { additions = additions
      , deletions = deletions
      , updates = updates
      , pointer = first.pointer
      , branch = first.branch
      }


flip : (a -> b -> c) -> b -> a -> c
flip function argB argA =
    function argA argB


