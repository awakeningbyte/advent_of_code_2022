open System
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("input.txt")

let height = inputs |> Array.length
let width = inputs[0].Length
let paddingX = -1

type Point = {x: int; y: int} 
type Tree = {value: int; visible: int; position: Point}  

let rec scan  (acc: Tree[]) (line: Tree list) =
    match line with
    | [] -> 
        (Array.Reverse acc)
        acc
    | h::tail when (Array.isEmpty acc) -> 
        scan [| {h with visible = 0 }; |] tail
    | h::tail ->    
        acc
        |> Array.tryFindIndex(fun x -> x.value >= h.value)
        |> fun x ->
            match x with
            | Some(i) -> i+1
            | None -> acc.Length
        |> fun i -> scan [|{h with visible = h.visible * i}; yield! acc;  |] tail

let rotate (g: Tree[][]) =
    let width = g.[0].Length
    let height = g.Length

    [| for i in 0..(width - 1) -> (width - 1 - i) |]
    |> Array.map(fun x ->
        [|0..height-1|]
        |> Array.map(fun y -> g.[y][x] ))
    
inputs
|> Array.map(fun x -> x.ToCharArray() )
|> Array.mapi(fun i x ->
        x 
        |> Array.mapi (fun j c -> {value =(int c) - (int '0'); visible = 1; position = {x = j; y = i}}))
|> Array.map(fun line -> scan Array.empty (line |> List.ofArray))
|> rotate
|> Array.map(fun line -> scan Array.empty (line |> List.ofArray))
|> rotate
|> Array.map(fun line -> scan Array.empty (line |> List.ofArray))
|> rotate
|> Array.map(fun line -> scan Array.empty (line |> List.ofArray))
|> rotate
|> Array.collect(fun line -> line)
|> Array.maxBy(fun t -> t.visible)
|> printfn " %A"
