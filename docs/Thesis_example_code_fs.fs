module Examples

module Math =
    let add a b =
        a + b

    let answer = add 40 2

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


let a = 10
let b = 10
let igaz = a = b
let hamis = a <> b


let items = [1;0]
let moreItems = 2 :: items // [2;1;0]

let 