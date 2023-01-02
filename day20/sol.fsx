open System
let inputs=IO.File.ReadAllLines("test.txt")

type Node = 
  {
    v: int
    visited: bool
  }
  static member New(v: string) = 
    { v=  (int v); visited =false}
let coods =
  inputs
  |> Array.filter(fun x -> not (String.IsNullOrEmpty x) )
  |> Array.map Node.New
  |> Array.toList

let N = coods.Length - 1

let rec run (input_coods: list<Node>) (ptr: int) =
  if ptr = N then
    input_coods
  else
    let { v=v; visited = visited } = input_coods[ptr]
    if visited || v = 0 then
      run input_coods (ptr+1)
    else
      let i =
        if v > 0 then 
          (v + ptr) % N
        else
          ((ptr + v) % N + N) % N
      printfn "%A : %A :%A" v ptr i
      input_coods |> Seq.iter (fun {v = x ; visited = _} -> printf "%A " x)
      printfn "\n"
      let coods2 = [yield! input_coods[0..(ptr-1)]; yield! input_coods[(ptr+1)..]]
      match i with
      | x when x <= ptr -> run (coods2 |> List.insertAt i {v = v; visited = true}) (ptr+1)
      | x -> run (coods2 |> List.insertAt i {v = v; visited = true}) ptr
run coods 0 
|> Seq.iter (fun {v = x ; visited = _} -> printf "%A " x)