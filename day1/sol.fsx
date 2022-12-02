open System
let inputs=IO.File.ReadAllLines("input.txt")

let (b,_) =
    inputs
    |> Array.fold(fun  (lst: seq<int>, acc: int) line ->
        match line with
        | _ when line.Length = 0 -> ( seq {yield! lst; acc}, 0)
        | l -> (lst, acc+(int line))

        ) (Seq.empty,0)

b
|> Seq.sortDescending
|> Seq.take(3)
|> Seq.sum
|> printfn "%i"
    