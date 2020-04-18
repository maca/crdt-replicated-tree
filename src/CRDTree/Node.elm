module CRDTree.Node exposing
  ( Node
  , Error(..)
  , value
  , timestamp
  , path
  , isDeleted
  , children
  , descendant
  , init
  , root
  , tombstone
  , addAfter
  , delete
  , updateParent
  )

{-| This module implements types and functions to build, find, get
values from, and compare nodes

@docs Node
@docs Error

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

# Operations
@docs addAfter
@docs delete
@docs updateParent

-}

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


{-| Represents an Error updating a node -}
type Error
  = NotFound
  | AlreadyApplied
  | BadTimestamp


{-| Build a node

    timestamp (init 'a' [0, 1]) == 1
    path (init 'a' [0, 1]) == [0, 1]
    value (init 'a' [0, 1]) == Just 'a'
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
child ts node =
  find (\n -> (timestamp n) == ts) (children node)


find : (Node a -> Bool) -> List (Node a) -> Maybe (Node a)
find pred nodes =
  case nodes of
    [] ->
      Nothing

    n :: ns ->
      if pred n then
        Just n

      else
        find pred ns


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

    value (node 'a' [0, 1]) == Just 'a'
    value (tombstone [0, 1]) == Nothing
-}
value : Node a -> Maybe a
value n =
  case n of
    Root _ -> Nothing
    Node rec -> Just rec.value
    Tombstone _ -> Nothing


{-| Return the timestamp of a node

    timestamp (node 'a' [0, 1]) == 1
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
    Root _ -> []
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


{-| Adds  after node with timestamp
-}
addAfter : Int -> (Int, a) -> Node a -> Result Error (Node a)
addAfter prev (ts, val) parent =
  let
      node =
        init val <| (path parent) ++ [ts]

      result =
        case compare prev 0 of
          LT ->
            Err BadTimestamp

          EQ ->
            insertAfter prev node [] (children parent)

          GT ->
            insertAfter prev node [] (children parent)
  in
      applyResult parent result


insertAfter : Int -> Node a
                  -> List (Node a)
                  -> List (Node a)
                  -> Result Error (List (Node a))
insertAfter prev node acc nodes =
  case nodes of
    [] ->
      if prev == 0 then
        (insert node [] acc)
          |> Result.map (\acc2 -> acc2 ++ nodes)
          -- optimize

      else
        Err NotFound

    n :: rest ->
      if (timestamp n) == prev then
        (insert node [] acc)
          |> Result.map (\acc2 -> acc2 ++ nodes)

      else
        insertAfter prev node (n :: acc) rest


insert : Node a -> List (Node a)
                -> List (Node a)
                -> Result Error (List (Node a))
insert node acc nodes =
  case nodes of
    [] ->
      Ok <| node :: acc

    n :: rest ->
      case compare (timestamp n) (timestamp node) of
        LT ->
          Ok <| (List.reverse nodes) ++ node :: acc

        EQ ->
          Err AlreadyApplied

        GT ->
          insert node (n :: acc) rest


{-| Delete a node -}
delete : Int -> Node a -> Result Error (Node a)
delete ts parent =
  update ts (path >> tombstone >> Ok) [] (children parent)
    |> applyResult parent


{-| Update a node with a successful result -}
updateParent : Int -> Node a
                   -> (Node a -> Result Error (Node a))
                   -> Result Error (Node a)
updateParent ts parent updateFn =
  update ts updateFn [] (children parent)
    |> applyResult parent


update : Int -> (Node a -> Result Error (Node a))
             -> List (Node a)
             -> List (Node a)
             -> Result Error (List (Node a))
update ts fn acc nodes =
  case nodes of
    [] ->
      Err NotFound

    n :: rest ->
      if ts < 1 then
        Err BadTimestamp

      else if (timestamp n) == ts then
        case fn n of
          Ok node ->
            if node == n then
              Err AlreadyApplied

            else
              Ok <| (List.reverse acc) ++ (node :: rest)

          Err err ->
            Err err

      else
        update ts fn (n :: acc) rest


applyResult : Node a -> Result Error (List (Node a))
                     -> Result Error (Node a)
applyResult parent result =
  Result.map (\nodes -> replaceChildren nodes parent) result


{-| Update a Node's children
-}
replaceChildren : List (Node a) -> Node a -> Node a
replaceChildren ch node =
  case node of
    Root record ->
      Root { record | children = ch }

    Node record ->
      Node { record | children = ch }

    Tombstone _ -> node
