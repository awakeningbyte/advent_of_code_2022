open System
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("input.txt")

let height = 8
let width = 9

let stacks =
    inputs
    |> Array.take(height)
    |> Array.map(fun x -> 
        (x+" ").ToCharArray() 
        |> Array.chunkBySize(4) 
        |> Array.map System.String
        )
    |> fun (arr) -> 
        [|0..width-1|]
        |> Array.map(fun i -> 
            ((i+1), 
                arr 
                |> Array.map(fun a -> a.[i]) 
                |> Array.filter(fun s ->  String.IsNullOrEmpty(s) = false)
                |> List.ofArray
            ) )
    |> dict
    |> Dictionary

let rec pop (x: int) (stack: string list) = 
    if x = 0 then
        ([], stack)
    else
        match stack with
        | j when List.isEmpty stack -> ([],[])
        | h::tail -> 
            match h.Trim() with
            | e when String.IsNullOrEmpty(e) -> pop x tail
            | _ ->
                let (r,t) = pop (x-1) tail
                ([yield! r; h], t) //only need change the h position to ahead to get part2 answer.  ([h; yield! r], t) 

inputs
|> Array.map (fun line -> 
    let s = line.Split(" ") 
    match s with
    | [|"move";x;"from";y;"to";z|] -> 
        let (p, r) =pop (int x) stacks[(int y)] 
        stacks[(int z)] <- [yield! p ; yield! stacks[(int z)] ]
        stacks[(int y)] <- r
    | _ -> () 
)

stacks
|> Seq.map(fun v ->  v.Value.Head)
|> Seq.toArray
|> printfn "%A"

// |> Seq.iter (fun k -> 
//     if k.Value.IsEmpty then
//         printfn " - "
//     else 
//         printfn "%A" k.Value.Head 
// )