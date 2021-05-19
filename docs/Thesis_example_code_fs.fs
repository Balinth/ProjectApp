module Examples

module Math =
    let add a b =
        a + b

open System

let dispose (obj:IDisposable) : unit =
    obj.Dispose()