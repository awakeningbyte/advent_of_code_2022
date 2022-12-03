open System
let inputs=IO.File.ReadAllLines("input.txt")

inputs
|> Array.map(fun x -> 
        let arr = x.ToCharArray()
        let n = (Array.length arr)
        let l,r  = Array.splitAt  (n / 2) arr
        [ yield! ['a'..'z']; yield! ['A'..'Z']]
        |> Seq.find(fun x -> (Array.contains x  l)  && (Array.contains x r )
    ))
|> Array.map(fun c -> 
    let s =match c with
        | x when c > 'Z' -> (int c) - (int 'a') + 1
        | _ -> ((int c) - (int 'A')) + 27

    // printfn "%c: %i = %i" c ((int c) - (int 'z')) s
    s
)
|> Array.sum
|> printfn "%i"
