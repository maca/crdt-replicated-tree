module Stream
    exposing
        ( Stream
        , concatMap
        , append
        , cons
        , continue
        , create
        , empty
        , filter
        , fromList
        , map
        , range
        , singleton
        , stop
        , take
        , takeWhile
        , toList
        )

{-| Fast and simple stream library for Elm. Create streams of data or flatten
multiple operations over lists.


# Stream Type

@docs Stream


# Use Streams with Lists

@docs fromList, toList


# Create Streams

@docs singleton, empty, range, create, continue, stop


# Add to Streams

@docs cons, append


# Transform Streams

@docs map, filter, take, takeWhile, concatMap

-}


{-| Represent a stream with a thunk. Streams let you efficiently and lazily
transform values. Streams flatten operations like [`map`](#map) and
[`filter`](#filter) so you don't have to iterate through a stream multiple
times like a list.

    Stream.range 1 1000
        |> Stream.map (\n -> n * 2)
        |> Stream.filter (\n -> n > 10)
        |> Stream.take 3
        |> Stream.toList == [12, 14, 16]

You can use streams with lists of data too.

    [1, 2, 3, 4, 5]
        |> Stream.fromList
        |> Stream.map (\n -> 2 ^ n)
        |> Stream.filter (\n -> n > 8)
        |> Stream.toList == [16, 32]

-}
type alias Stream a =
    () -> StreamElement a


type StreamElement a
    = Cons a (Stream a)
    | Nil


type Next a
    = Continue a
    | Stop


{-| Create a stream from a list.

    fromList [1, 2, 3]

-}
fromList : List a -> Stream a
fromList list () =
    case list of
        [] ->
            Nil

        x :: xs ->
            Cons x (fromList xs)


{-| Convert a stream into a list. Evaluates every value in a stream. Be careful
with infinite streams, so use [`take`](#take) before calling.

    [1, 2, 3]
        |> fromList
        |> toList == [1, 2, 3]

-}
toList : Stream a -> List a
toList stream =
    let
        toList_ : StreamElement a -> List a -> List a
        toList_ streamElement list =
            case streamElement of
                Cons value nextStream ->
                    toList_ (nextStream ()) (value :: list)

                Nil ->
                    list
    in
        []
            |> toList_ (stream ())
            |> List.reverse


{-| A stream with no values.

    empty
        |> toList == []

-}
empty : Stream a
empty () =
    Nil


{-| Create a stream with a single value.

    singleton 42
        |> toList == [42]

-}
singleton : a -> Stream a
singleton value =
    cons value empty


{-| Add a value to the beginning of a stream.

    cons (singleton 1) (singleton 2)
        |> toList == [1, 2]

-}
cons : a -> Stream a -> Stream a
cons value stream () =
    Cons value stream


{-| Combine two streams.

    append (range 1 3) (range 4 6)
        |> toList == [1, 2, 3, 4, 5, 6]

-}
append : Stream a -> Stream a -> Stream a
append stream1 stream2 () =
    case stream1 () of
        Cons value nextStream ->
            Cons value (append nextStream stream2)

        Nil ->
            stream2 ()


{-| Create a stream of numbers with each number increasing by one. Provide the
starting and ending number as arguments.

    range 1 10
        |> toList == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-}
range : Int -> Int -> Stream Int
range start end =
    if start > end then
        empty
    else
        create
            (\n ->
                if n < end then
                    continue (n + 1)
                else
                    stop
            )
            start


{-| Create a stream with a generator function to produce
each value based on the previous value. Return the next value wrapped in
[`continue`](#continue) or stop the stream by returning [`stop`](#stop).
Provide the initial value as the second argument to `create`.

    naturalNumbers =
        create (\n -> continue (n + 1)) 1

    upTo5 =
        create
            (\n ->
                if n < 5 then
                    continue (n + 1)
                else
                    stop
            )
            1

-}
create : (a -> Next a) -> a -> Stream a
create generator initialValue () =
    let
        create_ : Next a -> StreamElement a
        create_ next =
            case next of
                Continue value ->
                    Cons value (\() -> create_ (generator value))

                Stop ->
                    Nil
    in
        create_ (continue initialValue)


{-| Used with [`create`](#create) to continue the stream with a new value.

    naturalNumbers =
        create (\n -> continue (n + 1)) 1

-}
continue : a -> Next a
continue =
    Continue


{-| Used with [`create`](#create) to stop the stream.

    upTo5 =
        create
            (\n ->
                if n < 5 then
                    continue (n + 1)
                else
                    stop
            )
            1

-}
stop : Next a
stop =
    Stop


{-| Transform every value in a stream with a function.

    [1, 2, 3]
        |> fromList
        |> map (\n -> n * 2)
        |> toList == [2, 4, 6]

-}
map : (a -> b) -> Stream a -> Stream b
map f stream () =
    case stream () of
        Cons value nextStream ->
            Cons (f value) (map f nextStream)

        Nil ->
            Nil


{-| Keep values that return `True` for the provided function.

    range 1 10
        |> filter (\n -> n > 7)
        |> toList == [8, 9, 10]

-}
filter : (a -> Bool) -> Stream a -> Stream a
filter f stream () =
    case stream () of
        Cons value nextStream ->
            if f value then
                Cons value nextStream
            else
                filter f nextStream ()

        Nil ->
            Nil


{-| Take only up to *n* values from the stream. Useful for consuming infinite
streams.

    naturalNumbers =
        create (\n -> continue (n + 1)) 1

    naturalNumbers
        |> take 3
        |> toList == [1, 2, 3]

-}
take : Int -> Stream a -> Stream a
take n stream () =
    if n <= 0 then
        Nil
    else
        case stream () of
            Cons value nextStream ->
                Cons value (take (n - 1) nextStream)

            Nil ->
                Nil


{-| Take values as long as the predicate function returns true.

    range 1 10
        |> takeWhile (\n -> n < 6)
        |> toList == [1, 2, 3, 4, 5]

-}
takeWhile : (a -> Bool) -> Stream a -> Stream a
takeWhile predicate stream () =
    case stream () of
        Cons value nextStream ->
            if predicate value then
                Cons value (takeWhile predicate nextStream)
            else
                Nil

        Nil ->
            Nil


{-| Map values to streams and flatten the resulting streams.

    -- Convert list of words to stream of letters
    ["hello", "there"]
        |> fromList
        |> concatMap (\word -> word |> String.split "" |> fromList)
        |> toList == ["h", "e", "l", "l", "o", "t", "h", "e", "r", "e"]

    -- Flatten inner stream ranges
    range 1 3
        |> concatMap (\n -> range n (n + 2))
        |> toList == [1, 2, 3, 2, 3, 4, 3, 4, 5]

    -- Skip values with `empty`
    range 1 10
        |> concatMap
            (\n ->
                if n < 6 then
                    empty
                else
                    singleton n
            )
        |> toList [6, 7, 8, 9, 10]

-}
concatMap : (a -> Stream b) -> Stream a -> Stream b
concatMap f stream () =
    case stream () of
        Cons value nextStream ->
            append (f value) (concatMap f nextStream) ()

        Nil ->
            Nil
