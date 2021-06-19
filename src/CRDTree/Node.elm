module CRDTree.Node exposing
    ( Node
    , root
    , value
    , timestamp
    , path
    , children
    , descendant
    , find
    , map
    , filterMap
    , foldl
    , foldr
    )

{-| This module implements types and functions to build, traverse and
transform nodes

@docs Node


# Build

@docs root


# Properties

@docs value
@docs timestamp
@docs path


# Children

@docs children
@docs descendant
@docs find
@docs map
@docs filterMap
@docs foldl
@docs foldr

-}

import Internal.Node as Node


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
-}
type alias Node a =
    Node.Node a


{-| A path is a list of **Int**s
-}
type alias Path =
    List Int


{-| Create a root node
-}
root : Node a
root =
    Node.root


{-| List of nodes' children
-}
children : Node a -> List (Node a)
children node =
    map identity node


{-| Find node matching function
-}
find : (Node a -> Bool) -> Node a -> Maybe (Node a)
find =
    Node.find


{-| Apply a function to all children nodes
-}
map : (Node a -> b) -> Node a -> List b
map =
    Node.map


{-| Filter and apply a function to children nodes
-}
filterMap : (Node a -> Maybe b) -> Node a -> List b
filterMap =
    Node.filterMap


{-| Fold over all children nodes from the left
-}
foldl : (Node a -> b -> b) -> b -> Node a -> b
foldl =
    Node.foldl


{-| Fold over all children nodes from the left
-}
foldr : (Node a -> b -> b) -> b -> Node a -> b
foldr =
    Node.foldr


{-| Return a Node at a path
-}
descendant : List Int -> Node a -> Maybe (Node a)
descendant =
    Node.descendant


{-| Timestamp of a node
-}
timestamp : Node a -> Int
timestamp =
    Node.timestamp


{-| Path of a node
-}
path : Node a -> List Int
path =
    Node.path


{-| Value of a node unless deleted or root node
-}
value : Node a -> Maybe a
value =
    Node.value
