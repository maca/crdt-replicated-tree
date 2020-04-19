module CRDTree.Node exposing
    ( Node
    , Error(..)
    , root
    , addAfter
    , delete
    , value
    , children
    , descendant
    , map
    , filterMap
    , foldl
    , update
    )

{-| This module implements types and functions to build, get
values from, and compare nodes

@docs Node
@docs Error


# Build

@docs root


# Operations

@docs addAfter
@docs delete


# Properties

@docs value
@docs timestamp


# Children

@docs children
@docs descendant
@docs map
@docs filterMap
@docs foldl

-}

import Dict exposing (Dict)


type alias Children a =
    Dict Int (Node a)


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
-}
type Node a
    = Root (Children a)
    | Node a (Children a) (Maybe Int)
    | Tombstone (Maybe Int)


{-| Represents an Error updating a node
-}
type Error
    = NotFound
    | AlreadyApplied
    | Invalid


{-| Create a root node
-}
root : Node a
root =
    Root emptyChildren


emptyChildren : Children a
emptyChildren =
    Dict.singleton 0 (Tombstone Nothing)


{-| Create node after node at timestamp
-}
addAfter : Int -> ( Int, a ) -> Node a -> Result Error (Node a)
addAfter tsPrev ( ts, val ) parent =
    case child ts parent of
        Nothing ->
            addAfterHelp ( ts, val ) tsPrev parent

        Just _ ->
            Err AlreadyApplied


addAfterHelp : ( Int, a ) -> Int -> Node a -> Result Error (Node a)
addAfterHelp ( ts, val ) tsPrev parent =
    case child tsPrev parent of
        Nothing ->
            Err NotFound

        Just found ->
            let
                node =
                    Node val emptyChildren (next found)

                ( leftTs, left ) =
                    findInsertion ts
                        ( tsPrev, found )
                        (childrenDict parent)
            in
            parent
                |> insert leftTs (updateNext ts left)
                |> insert ts node
                |> Ok


findInsertion : Int -> ( Int, Node a ) -> Children a -> ( Int, Node a )
findInsertion ts ( tsLeft, left ) c =
    case next left of
        Nothing ->
            ( tsLeft, left )

        Just n ->
            if ts > n then
                ( tsLeft, left )

            else
                case Dict.get n c of
                    Nothing ->
                        ( tsLeft, left )

                    Just node ->
                        findInsertion ts ( n, node ) c


{-| Delete a node
-}
delete : Int -> Node a -> Result Error (Node a)
delete ts parent =
    case child ts parent of
        Nothing ->
            Err NotFound

        Just (Node _ _ n) ->
            Ok <| insert ts (Tombstone n) parent

        Just _ ->
            Err AlreadyApplied


insert : Int -> Node a -> Node a -> Node a
insert ts node parent =
    case parent of
        Node val dict n ->
            Node val (Dict.insert ts node dict) n

        Tombstone _ ->
            parent

        Root dict ->
            Root (Dict.insert ts node dict)


update :
    (Int -> Node a -> Result Error (Node a))
    -> List Int
    -> Node a
    -> Result Error (Node a)
update func path parent =
    case parent of
        Tombstone _ ->
            Err AlreadyApplied

        _ ->
            case path of
                [] ->
                    Err Invalid

                ts :: [] ->
                    func ts parent

                ts :: tss ->
                    updateHelp (update func tss) ts parent


updateHelp :
    (Node a -> Result Error (Node a))
    -> Int
    -> Node a
    -> Result Error (Node a)
updateHelp func ts parent =
    case child ts parent of
        Nothing ->
            Err Invalid

        Just found ->
            case func found of
                Err err ->
                    Err err

                Ok node ->
                    if node == found then
                        Err AlreadyApplied

                    else
                        Ok <| insert ts node parent


{-| List of nodes' children
-}
children : Node a -> List ( Int, Node a )
children node =
    map Tuple.pair node


{-| Apply a function to all children nodes
-}
map : (Int -> Node a -> b) -> Node a -> List b
map func node =
    foldl (\t n acc -> func t n :: acc) [] node |> List.reverse


{-| Filter and apply a function to children nodes
-}
filterMap : (Int -> Node a -> Maybe b) -> Node a -> List b
filterMap func node =
    foldl (maybeConst func) [] node |> List.reverse


maybeConst :
    (Int -> Node a -> Maybe b)
    -> Int
    -> Node a
    -> List b
    -> List b
maybeConst func ts node acc =
    case func ts node of
        Nothing ->
            acc

        Just val ->
            val :: acc


{-| Fold over all children nodes
-}
foldl : (Int -> Node a -> b -> b) -> b -> Node a -> b
foldl func acc node =
    case Dict.get 0 (childrenDict node) of
        Nothing ->
            acc

        Just left ->
            childrenFold func acc left (childrenDict node)


childrenFold :
    (Int -> Node a -> b -> b)
    -> b
    -> Node a
    -> Children a
    -> b
childrenFold func acc left c =
    let
        n =
            next left
    in
    case Maybe.andThen (\t -> Dict.get t c) n of
        Nothing ->
            acc

        Just ((Tombstone _) as node) ->
            childrenFold func acc node c

        Just node ->
            let
                timestamp =
                    Maybe.withDefault 0 n
            in
            childrenFold func (func timestamp node acc) node c


childrenDict : Node a -> Children a
childrenDict node =
    case node of
        Node _ c _ ->
            c

        Tombstone _ ->
            Dict.empty

        Root c ->
            c


next : Node a -> Maybe Int
next node =
    case node of
        Node _ _ n ->
            n

        Tombstone n ->
            n

        Root _ ->
            Nothing


updateNext : Int -> Node a -> Node a
updateNext n node =
    case node of
        Node val c _ ->
            Node val c (Just n)

        Tombstone _ ->
            Tombstone (Just n)

        Root _ ->
            node


{-| Find a node child matching a timestamp
-}
child : Int -> Node a -> Maybe (Node a)
child ts node =
    childrenDict node |> Dict.get ts


{-| Return a Node at a path
-}
descendant : List Int -> Node a -> Maybe (Node a)
descendant nodePath n =
    case nodePath of
        [] ->
            Nothing

        [ ts ] ->
            child ts n

        ts :: tss ->
            child ts n |> Maybe.andThen (descendant tss)


{-| Return the value of a node unless deleted or root node
-}
value : Node a -> Maybe a
value node =
    case node of
        Node v _ _ ->
            Just v

        Tombstone _ ->
            Nothing

        Root _ ->
            Nothing
