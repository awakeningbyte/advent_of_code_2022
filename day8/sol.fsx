open System
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("input.txt")

let height = inputs |> Array.length
let width = inputs[0].Length
let paddingY = [| for i in 1..height -> -1 |]
let paddingX = -1

type Point = {x: int; y: int} 
type Tree = {value: int; visible: bool; position: Point}  
let scan  =
    Array.map(fun line ->
            let (leftToRightResult, _) =
                line
                |> Array.mapFold(fun accX t ->
                    if accX < t.value then
                        { t with visible = true } , t.value
                    else
                        t , accX
                ) paddingX

            let (rightToLeftResult, _) =
                Array.mapFoldBack(fun t  acc -> 
                    if t.value > acc then
                        ({ t with visible = true}, t.value)
                    else
                        (t, acc)
                
                ) leftToRightResult -1
            
            rightToLeftResult ) 

let rotate (g: Tree[][]) =
    let width = g.[0].Length
    let height = g.Length
    printfn ("%i * %i") width height
    [|0..width-1|]
    |> Array.map(fun x ->
        [|0..height-1|]
        |> Array.map(fun y -> g.[y][x] ))
    

inputs
|> Array.map(fun x -> x.ToCharArray() )
|> Array.mapi(fun i x ->
        x 
        |> Array.mapi (fun j c -> {value =(int c) - (int '0'); visible = false; position = {x = j; y = i}}))
|> scan
|> rotate
|> scan
|> Array.sumBy(fun line -> 
    line 
    |> Array.filter(fun t -> t.visible)
    |> Array.length)
|> printfn "%i"
