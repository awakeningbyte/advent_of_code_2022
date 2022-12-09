open System
let inputs=IO.File.ReadAllLines("input.txt")
type Position = {x: int; y: int}
let drag h h1 t =
  let p = ( h1.x - t.x, h1.y - t.y)
  match p with
  | (1,0) | (-1,0) | (0,0)-> t
  | (0,1) | (0,-1) -> t
  | (2,0) -> {t with x = t.x + 1}
  | (-2,0) -> {t with x = t.x - 1}
  | (0,2) -> {t with y = t.y + 1}
  | (0, -2) -> {t with y = t.y - 1}
  | (1,1)|(1,-1)|(-1,1)|(-1,-1) -> t
  | (1,2) | (-1,2) | (1,-2) | (-1,-2) | (-2,1) | (2,1) | (2,-1) | (-2,-1)-> h
  | p -> failwithf "%A not match" p


let rec move h t d n trace =
  if n = 0 then
    (h,t,trace)
  else 
    let h1 =
      match d with
      | "U" -> { h with y = h.y + 1}
      | "D" -> { h with y =h.y - 1}
      | "L" -> { h with x= h.x - 1 }
      | "R" -> { h with x = h.x + 1}
    let t1 = drag h h1 t
    move h1 t1 d (n - 1) [yield! trace; t1]

let (_,_,trace) =
  inputs
  |> Array.map(fun line -> line.Split(" "))
  |> Array.fold (fun (h,t, trace) [|d; n|]->
    let (h1, t1, trace1) = move h t d (int n) trace
    // printfn "%i" c
    (h1,t1,trace1)
  ) ({x=0;y=0}, {x=0;y=0}, List.empty<Position>)

printfn "%A" (trace |> List.distinct |> List.length)