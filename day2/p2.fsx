open System

let inputs=IO.File.ReadAllLines("input.txt")
let scores = Map.empty.Add("X", 0).Add("Y",3).Add("Z",6)
let rules = 
    Map.empty.
        Add(("A","X"), 3).
        Add(("A","Y"), 1).
        Add(("A","Z"), 2).
        Add(("B","X"), 1).
        Add(("B","Y"), 2).
        Add(("B","Z"), 3).
        Add(("C","X"), 2).
        Add(("C","Y"), 3).
        Add(("C","Z"), 1)
inputs
|> Array.map(fun x -> 
    let [|o; m |] = x.Split(' ')
    (o, m)
    )
|> Array.map(fun x -> 
        match x with 
        | (a, b) -> rules[x] + scores[b]
    )
|> Array.sum
|> printfn "%i"