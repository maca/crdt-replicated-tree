module RGA.List exposing
  ( ListFail(..)
  , find
  , applyWhen
  , insertWhen
  , replaceWhen
  )

import Result exposing (Result(..))


type ListFail
  = NotFound
  | ApplicationFailed
  | AlreadyApplied


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
  case list of
    [] ->
      Nothing

    x :: xs ->
      if predicate x then
        Just x

      else
        find predicate xs


insertWhen : (a -> Bool) -> a
                         -> List a
                         -> Result ListFail (List a)
insertWhen predicate elem list =
  applyWhen predicate (\x -> Ok [elem, x]) list


replaceWhen : (a -> Bool) -> a
                          -> List a
                          -> Result ListFail (List a)
replaceWhen predicate elem list =
  applyWhen predicate (always (Ok [elem])) list


applyWhen : (a -> Bool) -> (a -> Result f (List a))
                        -> List a
                        -> Result ListFail (List a)
applyWhen predicate fun list =
  applyWhenHelp predicate fun [] list


applyWhenHelp : (a -> Bool) -> (a -> Result f (List a))
                            -> List a
                            -> List a
                            -> Result ListFail (List a)
applyWhenHelp predicate fun acc list =
  case list of
    [] ->
      Err NotFound

    x :: xs ->
      if predicate x then
        case fun x of
          Err _ ->
            Err ApplicationFailed

          Ok ins ->
            let
                alreadyApplied =
                  List.head acc
                    |> Maybe.map (\n -> List.any ((==) n) ins)
                    |> Maybe.withDefault False
            in
                if alreadyApplied || ins == [x] then
                  Err AlreadyApplied

                else
                  Ok <| (List.reverse acc) ++ ins ++ xs
      else
        applyWhenHelp predicate fun (x :: acc) xs


