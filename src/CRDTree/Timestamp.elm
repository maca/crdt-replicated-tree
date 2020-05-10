module CRDTree.Timestamp exposing
    ( increment
    , replicaId
    , incrementTo, init, operationId
    )

{-|

@docs Timestamp
@docs increment
@docs replicaId
@docs fromInt

-}

import Bitwise


replicasField : Int
replicasField =
    2 ^ 20


init : Int -> Int
init int =
    int


increment : Int -> Int
increment timestamp =
    timestamp + replicasField


incrementTo : Int -> Int -> Int
incrementTo ref toInc =
    let
        diff =
            operationId ref - operationId toInc
    in
    if diff > 0 then
        toInc + (diff * replicasField)

    else
        toInc


operationId : Int -> Int
operationId timestamp =
    timestamp // replicasField


replicaId : Int -> Int
replicaId timestamp =
    Bitwise.and timestamp (replicasField - 1)
