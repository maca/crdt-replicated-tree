module CRDTree.Node exposing
    ( Node
    , Error(..)
    , root
    , addAfter
    , localAddAfter
    , delete
    , value
    , timestamp
    , children
    , descendant
    , find
    , findNodes
    , map
    , filterMap
    , foldl
    , atIndex
    , childIndex
    , path
    )

{-| This module implements types and functions to build, traverse and
transform nodes

@docs Node
@docs Error


# Build

@docs root


# Operations

@docs addAfter
@docs localAddAfter
@docs delete


# Properties

@docs value
@docs timestamp


# Children

@docs children
@docs descendant
@docs find
@docs findNodes
@docs map
@docs filterMap
@docs foldl
@docs atIndex
@docs childIndex
@docs path

-}

import Array exposing (Array)
import Dict exposing (Dict)


type alias Children a =
    Dict Int (Node a)


{-| Node can be either a branch or leaf `Node` with optional
data or a `Tombstone` representing a removed Node
-}
type Node a
    = Root (Children a)
    | Node a (Children a) (Array Int) (Maybe Int)
    | Tombstone (Array Int) (Maybe Int)


{-| Represents an Error updating a node
-}
type Error
    = NotFound
    | AlreadyApplied
    | Invalid


{-| Create a root node

    timestamp root -- 0

    value root -- Nothing

-}
root : Node a
root =
    Root emptyChildren


emptyChildren : Children a
emptyChildren =
    Dict.singleton 0 (Tombstone Array.empty Nothing)


{-| Create node after node at timestamp

    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.map (atIndex 0 >> Maybe.andThen value)

    -- Ok (Just 'a')

    addAfter [ 1 ] ( 1, 'a' ) root -- Err NotFound

-}
addAfter : List Int -> ( Int, a ) -> Node a -> Result Error (Node a)
addAfter p insertion parent =
    update (addAfterHelp (findInsertion parent) p insertion) p parent


addAfterHelp :
    (Int -> ( Int, Node a ) -> ( Int, Node a ))
    -> List Int
    -> ( Int, a )
    -> Int
    -> Node a
    -> Result Error (Node a)
addAfterHelp findFunc p ( ts, val ) prevTs parent =
    case child ts parent of
        Just _ ->
            Err AlreadyApplied

        Nothing ->
            case child prevTs parent of
                Nothing ->
                    Err NotFound

                Just found ->
                    let
                        ( leftTs, left ) =
                            findFunc ts ( prevTs, found )

                        nd =
                            Array.fromList p
                                |> Array.slice 0 -1
                                |> Array.push ts
                                |> Node val emptyChildren

                        node =
                            nextNodeTimestamp left |> nd
                    in
                    parent
                        |> insert leftTs (updateNext ts left)
                        |> insert ts node
                        |> Ok


{-| Create node after node at timestamp
-}
localAddAfter :
    List Int
    -> ( Int, a )
    -> Node a
    -> Result Error (Node a)
localAddAfter p insertion parent =
    update (addAfterHelp (always identity) p insertion) p parent


findInsertion : Node a -> Int -> ( Int, Node a ) -> ( Int, Node a )
findInsertion parent ts ( n, node ) =
    case nextNodeTuple node parent of
        Nothing ->
            ( n, node )

        Just found ->
            if ts > Tuple.first found then
                ( n, node )

            else
                findInsertion parent ts found


{-| Delete a node
-}
delete : List Int -> Node a -> Result Error (Node a)
delete p node =
    update deleteHelp p node


deleteHelp : Int -> Node a -> Result Error (Node a)
deleteHelp ts parent =
    case child ts parent of
        Nothing ->
            Err NotFound

        Just (Node _ _ p n) ->
            Ok <| insert ts (Tombstone p n) parent

        Just _ ->
            Err AlreadyApplied


insert : Int -> Node a -> Node a -> Node a
insert ts node parent =
    case parent of
        Node val dict p n ->
            Node val (Dict.insert ts node dict) p n

        Tombstone _ _ ->
            parent

        Root dict ->
            Root (Dict.insert ts node dict)


update :
    (Int -> Node a -> Result Error (Node a))
    -> List Int
    -> Node a
    -> Result Error (Node a)
update func p parent =
    case parent of
        Tombstone _ _ ->
            Err AlreadyApplied

        _ ->
            case p of
                [] ->
                    Err Invalid

                ts :: [] ->
                    func ts parent

                ts :: tss ->
                    case child ts parent of
                        Nothing ->
                            Err Invalid

                        Just found ->
                            update func tss found
                                |> Result.map (\n -> insert ts n parent)


{-| List of nodes' children
-}
children : Node a -> List (Node a)
children parent =
    map identity parent


{-| Find node matching function
-}
find : (Node a -> Bool) -> Node a -> Maybe (Node a)
find pred parent =
    first parent |> Maybe.andThen (findHelp pred parent)


findHelp : (Node a -> Bool) -> Node a -> Node a -> Maybe (Node a)
findHelp pred parent left =
    case next left parent of
        Nothing ->
            Nothing

        Just node ->
            if pred node then
                Just node

            else
                findHelp pred parent node


{-| Find nodes matching function
-}
findNodes : (Node a -> Bool) -> Int -> Node a -> List (Node a)
findNodes pred count parent =
    first parent
        |> Maybe.map (findNHelp pred [] count parent)
        |> Maybe.map List.reverse
        |> Maybe.withDefault []


findNHelp :
    (Node a -> Bool)
    -> List (Node a)
    -> Int
    -> Node a
    -> Node a
    -> List (Node a)
findNHelp pred acc count parent left =
    case next left parent of
        Nothing ->
            acc

        Just node ->
            let
                acc_ =
                    if pred node then
                        node :: acc

                    else
                        acc
            in
            if List.length acc_ == count then
                acc_

            else
                findNHelp pred acc_ count parent node


{-| Apply a function to all children nodes
-}
map : (Node a -> b) -> Node a -> List b
map func node =
    foldr (func >> (::)) [] node


{-| Filter and apply a function to children nodes
-}
filterMap : (Node a -> Maybe b) -> Node a -> List b
filterMap func node =
    foldl (maybeConst func) [] node |> List.reverse


maybeConst : (Node a -> Maybe b) -> Node a -> List b -> List b
maybeConst func node acc =
    case func node of
        Nothing ->
            acc

        Just val ->
            val :: acc


{-| Fold over all children nodes from the left
-}
foldr : (Node a -> b -> b) -> b -> Node a -> b
foldr func acc node =
    foldl (::) [] node |> List.foldl func acc


{-| Fold over all children nodes from the left
-}
foldl : (Node a -> b -> b) -> b -> Node a -> b
foldl func acc parent =
    case first parent of
        Nothing ->
            acc

        Just left ->
            foldlFrom func left acc parent


{-| Get a child at index
-}
atIndex : Int -> Node a -> Maybe (Node a)
atIndex idx parent =
    first parent |> Maybe.andThen (atIndexHelp idx 0 parent)


atIndexHelp : Int -> Int -> Node a -> Node a -> Maybe (Node a)
atIndexHelp idx current parent left =
    case next left parent of
        Nothing ->
            Nothing

        Just ((Tombstone _ _) as node) ->
            atIndexHelp idx current parent node

        Just node ->
            if idx == current then
                Just node

            else
                atIndexHelp idx (current + 1) parent node


{-| Get index of a child
-}
childIndex : Node a -> Node a -> Maybe Int
childIndex expected parent =
    first parent |> Maybe.andThen (childIndexHelp 0 expected parent)


childIndexHelp : Int -> Node a -> Node a -> Node a -> Maybe Int
childIndexHelp idx expected parent left =
    case next left parent of
        Nothing ->
            Nothing

        Just ((Tombstone _ _) as node) ->
            childIndexHelp idx expected parent node

        Just node ->
            if node == expected then
                Just idx

            else
                childIndexHelp (idx + 1) expected parent node


foldlFrom : (Node a -> b -> b) -> Node a -> b -> Node a -> b
foldlFrom func left acc parent =
    case next left parent of
        Nothing ->
            acc

        Just ((Tombstone _ _) as node) ->
            foldlFrom func node acc parent

        Just node ->
            foldlFrom func node (func node acc) parent


childrenDict : Node a -> Children a
childrenDict node =
    case node of
        Node _ c _ _ ->
            c

        Tombstone _ _ ->
            Dict.empty

        Root c ->
            c


nextNodeTimestamp : Node a -> Maybe Int
nextNodeTimestamp node =
    case node of
        Node _ _ _ n ->
            n

        Tombstone _ n ->
            n

        Root _ ->
            Nothing


first : Node a -> Maybe (Node a)
first node =
    Dict.get 0 (childrenDict node)


next : Node a -> Node a -> Maybe (Node a)
next node parent =
    case nextNodeTimestamp node of
        Nothing ->
            Nothing

        Just ts ->
            Dict.get ts (childrenDict parent)


nextNodeTuple : Node a -> Node a -> Maybe ( Int, Node a )
nextNodeTuple node parent =
    Maybe.map2 Tuple.pair (nextNodeTimestamp node) (next node parent)


updateNext : Int -> Node a -> Node a
updateNext n node =
    case node of
        Node val c p _ ->
            Node val c p (Just n)

        Tombstone p _ ->
            Tombstone p (Just n)

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


{-| Path of a node
-}
timestamp : Node a -> Int
timestamp node =
    let
        p =
            arrayPath node
    in
    Array.get (Array.length p - 1) p |> Maybe.withDefault 0


{-| Path of a node
-}
path : Node a -> List Int
path node =
    arrayPath node |> Array.toList


arrayPath : Node a -> Array Int
arrayPath node =
    case node of
        Node _ _ p _ ->
            p

        Tombstone p _ ->
            p

        Root _ ->
            Array.empty


{-| Value of a node unless deleted or root node
-}
value : Node a -> Maybe a
value node =
    case node of
        Node v _ _ _ ->
            Just v

        Tombstone _ _ ->
            Nothing

        Root _ ->
            Nothing
