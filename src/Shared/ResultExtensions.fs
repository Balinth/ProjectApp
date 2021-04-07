module ResultExtensions
open System

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()

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
let (|>>) x y = Result.map y x
let (<!>) = mapResultList
let (<*>) = applyResultList
let lift2Result f a b =
    f <!> a <*> b

let lift3Result f a b c =
    f <!> a <*> b <*> c