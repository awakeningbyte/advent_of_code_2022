open System
let inputs=IO.File.ReadAllLines("input.txt")

let chunk = 14 // for part 1 use chunk 4
inputs[0].ToCharArray()
|> Array.windowed(chunk)
|> Array.findIndex(fun a -> 
    (a |> Set.ofArray |> Set.count) = (Array.length a)
)
|> fun x -> x + chunk
|> printfn "%i"