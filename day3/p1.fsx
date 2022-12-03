    open System
    let inputs=IO.File.ReadAllLines("input.txt")

    //part 1
    inputs
    |> Array.map( fun x -> 
            let arr = x.ToCharArray()
            let n = (Array.length arr)
            let l,r  = Array.splitAt  (n / 2) arr
            ( Set.intersect (Set.ofArray l) (Set.ofArray r)) |> Set.toArray |> Array.head )
    |> Array.map( fun c -> 
        match c with
            | x when c > 'Z' -> (int c) - (int 'a') + 1
            | _ -> ((int c) - (int 'A')) + 27 )
    |> Array.sum
    |> printfn "part 1: %i"

    //part 2
    let cal a = 
        a
        |> List.map Set.ofArray
        |> fun packs -> Set.intersectMany packs
        |> Seq.toArray
        |> Array.head
        |> fun c -> 
            match c with
                | x when c > 'Z' -> (int c) - (int 'a') + 1
                | _ -> ((int c) - (int 'A')) + 27

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
        |> printfn "part 2: %i"