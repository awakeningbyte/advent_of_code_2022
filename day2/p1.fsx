open System

let inputs=IO.File.ReadAllLines("input.txt")
let scores = Map.empty.Add("X", 1).Add("Y",2).Add("Z",3)
let rules = 
    Map.empty.
        Add(("A","X"), 3).
        Add(("B","Y"), 3).
        Add(("C","Z"), 3).
        Add(("A","Y"), 6).
        Add(("A","Z"), 0).
        Add(("B","X"), 0).
        Add(("B","Z"), 6).
        Add(("C","X"), 6).
        Add(("C","Y"), 0)
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