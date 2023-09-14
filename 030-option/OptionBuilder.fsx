// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
type OptionBuilder() =
    member _.Yield(x : 'a) : 'a option= Some x // You would normally define Return().
    member _.Bind(m : 'a option, f : 'a -> 'b option) : 'b option = Option.bind f m // Equivalent to flatMap

let option = OptionBuilder ()

option {
    let! x1 = Some 4
    let! x2 = if x1 % 2 = 0 then Some (x1 / 2) else None
    let! x3 = Some (x2 + 1)
    yield x3.ToString()
}
