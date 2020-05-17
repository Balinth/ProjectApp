module ResultExtensions

let map2 fErr fOk a b =
    match a, b with
    | Ok aOk, Ok bOk -> fOk aOk bOk |> Ok
    | Error aErr, Error bErr -> fErr aErr bErr |> Error
    | Error aErr, _ -> Error aErr
    | _, Error bErr -> Error bErr

let applyResult errorHandler f a =
    match f, a with
    | Ok fOk, Ok aOk -> Ok( fOk aOk)
    | Error fErr, Error aErr -> Error (errorHandler fErr aErr)
    | Error fErr, _ -> Error fErr
    | _, Error aErr -> Error aErr

let returnResult a =
    Ok a

let mapResult fErr f=
    returnResult f
    |> applyResult fErr

let mapResultList f = mapResult List.append f
let applyResultList f = applyResult List.append f

let (>>=) x y = Result.bind y x
let (<!>) = mapResultList
let (<*>) = applyResultList
let lift2Result f a b =
    f <!> a <*> b

let lift3Result f a b c =
    f <!> a <*> b <*> c