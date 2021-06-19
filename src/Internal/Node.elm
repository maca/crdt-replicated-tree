module Internal.Node exposing
    ( Error(..)
    , Node
    , addAfter
    , children
    , delete
    , descendant
    , filterMap
    , find
    , foldl
    , foldr
    , map
    , path
    , root
    , timestamp
    , value
    )

import Array exposing (Array)
import Dict exposing (Dict)


type alias Children a =
    Dict Int (Node a)


type Node a
    = Root (Children a)
    | Node a (Children a) (Array Int) (Maybe Int)
    | Tombstone (Array Int) (Maybe Int)


type Error
    = NotFound
    | AlreadyApplied
    | InvalidPath


root : Node a
root =
    Root emptyChildren


emptyChildren : Children a
emptyChildren =
    Dict.singleton 0 (Tombstone Array.empty Nothing)


addAfter : List Int -> ( Int, a ) -> Node a -> Result Error (Node a)
addAfter p insertion parent =
    update (addAfterHelp p insertion) p parent


addAfterHelp :
    List Int
    -> ( Int, a )
    -> Int
    -> Node a
    -> Result Error (Node a)
addAfterHelp p ( ts, val ) prevTs parent =
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
                            findInsertion ts
                                ( prevTs, found )
                                (childrenDict parent)

                        nodePath =
                            Array.fromList p
                                |> Array.slice 0 -1
                                |> Array.push ts

                        node =
                            Node val emptyChildren nodePath (next left)
                    in
                    parent
                        |> insert leftTs (updateNext ts left)
                        |> insert ts node
                        |> Ok


findInsertion : Int -> ( Int, Node a ) -> Children a -> ( Int, Node a )
findInsertion ts ( n, node ) c =
    case nextNodeTuple node c of
        Nothing ->
            ( n, node )

        Just found ->
            if ts < Tuple.first found then
                ( n, node )

            else
                findInsertion ts found c


delete : List Int -> Node a -> Result Error (Node a)
delete nodePath node =
    update deleteHelp nodePath node


deleteHelp : Int -> Node a -> Result Error (Node a)
deleteHelp nodeTimestamp parent =
    case child nodeTimestamp parent of
        Nothing ->
            Err NotFound

        Just (Node _ _ p n) ->
            Ok <| insert nodeTimestamp (Tombstone p n) parent

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
                    Err InvalidPath

                ts :: [] ->
                    func ts parent

                ts :: tss ->
                    case child ts parent of
                        Nothing ->
                            Err InvalidPath

                        Just found ->
                            update func tss found
                                |> Result.map (\n -> insert ts n parent)


children : Node a -> List (Node a)
children node =
    map identity node


find : (Node a -> Bool) -> Node a -> Maybe (Node a)
find pred node =
    Dict.get 0 (childrenDict node)
        |> Maybe.andThen (findHelp pred (childrenDict node))


findHelp : (Node a -> Bool) -> Children a -> Node a -> Maybe (Node a)
findHelp pred c left =
    case Maybe.andThen (\t -> Dict.get t c) (next left) of
        Nothing ->
            Nothing

        Just node ->
            if pred node then
                Just node

            else
                findHelp pred c node


map : (Node a -> b) -> Node a -> List b
map func node =
    foldr (func >> (::)) [] node


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


foldl : (Node a -> b -> b) -> b -> Node a -> b
foldl func acc node =
    case Dict.get 0 (childrenDict node) of
        Nothing ->
            acc

        Just left ->
            childrenFold func acc left (childrenDict node)


foldr : (Node a -> b -> b) -> b -> Node a -> b
foldr func acc node =
    foldl (::) [] node |> List.foldl func acc


childrenFold : (Node a -> b -> b) -> b -> Node a -> Children a -> b
childrenFold func acc left c =
    case nextNode left c of
        Nothing ->
            acc

        Just ((Tombstone _ _) as node) ->
            childrenFold func acc node c

        Just node ->
            childrenFold func (func node acc) node c


childrenDict : Node a -> Children a
childrenDict node =
    case node of
        Node _ c _ _ ->
            c

        Tombstone _ _ ->
            Dict.empty

        Root c ->
            c


next : Node a -> Maybe Int
next node =
    case node of
        Node _ _ _ n ->
            n

        Tombstone _ n ->
            n

        Root _ ->
            Nothing


nextNode : Node a -> Children a -> Maybe (Node a)
nextNode node c =
    next node |> Maybe.andThen (\n -> Dict.get n c)


nextNodeTuple : Node a -> Children a -> Maybe ( Int, Node a )
nextNodeTuple node c =
    Maybe.map2 Tuple.pair (next node) (nextNode node c)


updateNext : Int -> Node a -> Node a
updateNext n node =
    case node of
        Node val c p _ ->
            Node val c p (Just n)

        Tombstone p _ ->
            Tombstone p (Just n)

        Root _ ->
            node


child : Int -> Node a -> Maybe (Node a)
child ts node =
    childrenDict node |> Dict.get ts


descendant : List Int -> Node a -> Maybe (Node a)
descendant nodePath n =
    case nodePath of
        [] ->
            Nothing

        [ ts ] ->
            child ts n

        ts :: tss ->
            child ts n |> Maybe.andThen (descendant tss)


timestamp : Node a -> Int
timestamp node =
    let
        p =
            arrayPath node
    in
    Array.get (Array.length p - 1) p |> Maybe.withDefault 0


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


value : Node a -> Maybe a
value node =
    case node of
        Node v _ _ _ ->
            Just v

        Tombstone _ _ ->
            Nothing

        Root _ ->
            Nothing
