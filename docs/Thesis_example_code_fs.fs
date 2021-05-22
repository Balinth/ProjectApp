module Examples

module Math =
    let add a b = a + b // int -> int -> int

    //let answer = add 40 2 // int

    let add40 = add 40 // int -> int

    let answer = add40 2 // int
    let aPrime = add40 1 // int

    let convert (x:'a) : string =
        x.ToString()


open System

let dispose (obj:IDisposable) : unit =
    obj.Dispose()

open System.Collections.Generic

let addToDictionary (dict:IDictionary<'a,'b>) key value =
    let localValue = key // typeof<localValue> = 'a
    let localValue = value // typeof<localValue> = 'b
    dict.[key] <- localValue

let mutable variable = 1
variable <- 2


//let a = 10
//let b = 10
//let igaz = a = b
//let hamis = a <> b


let items = [1;0]
let moreItems = 2 :: items // [2;1;0]

type Point = {
    X: int
    Y: int
}
let p1 = {X=0;Y=0}
let p2 = {X=0;Y=0}
let igaz = p1 = p2
let getPointX point =
    point.X
//let tipusHiba = getPointX null
(*

type OptionalPoint =
    | Point of Point
    | Null
let optionalPoint = Point(p1)
let tipusHiba = getPointX optionalPoint
let getOptionalPointX point =
    match point with
    | Point point -> getPointX point
    | Null -> 0
let x = getOptionalPointX optionalPoint
let x = getOptionalPointX Null
*)