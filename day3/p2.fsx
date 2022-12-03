open System
let inputs=IO.File.ReadAllLines("input.txt")

let cal a = 
    a
    |> List.map(fun (x: char[]) -> 
        x 
        |> Array.groupBy(fun c -> c)
        |> Array.map(fun (k,v) -> (k, (Seq.length v)))
        |> dict
    )
    |> fun [x;y;z] -> 
                [ yield! ['a'..'z']; yield! ['A'..'Z']]
                |> Seq.find(fun c -> (x.ContainsKey c) && (y.ContainsKey c) && (z.ContainsKey c))
                |> fun c -> 
                    let s =match c with
                        | x when c > 'Z' -> (int c) - (int 'a') + 1
                        | _ -> ((int c) - (int 'A')) + 27
                    // printfn "* %c %i" c s
                    s

inputs
    |> Array.fold(fun (score, acc) x -> 
        let a =  [yield! acc; x.ToCharArray()]
        if (List.length a) = 3 then
            let v = cal a
            (v+score, List.Empty)
        else
            (score, a)
           
    ) (0, list.Empty)
    |> fun (x,_) -> x
    |> printfn "%i"

