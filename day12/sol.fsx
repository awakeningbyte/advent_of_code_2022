open System
let inputs=IO.File.ReadAllLines("test.txt")
type Position = 
  {x: int; y:int; }

type Node = 
  {c: char; upstream: list<Position>; v: bool; pos : Position}
  member this.isStart = this.c = 'S'
  member this.isEnd = this.c = 'E'
  member this.isVisited = this.v
  member this.neighbours (state: Map<Position, Node>) = 
    [(this.pos.x - 1, this.pos.y ); 
      (this.pos.x + 1, this.pos.y);
      (this.pos.x, this.pos.y - 1 ); 
      (this.pos.x, this.pos.y + 1)]
    |> List.filter(fun (x,y) -> 
      x >=0 && y >=0 )
    |> List.filter(fun (x,y) -> 
      x < inputs[0].Length && y < inputs.Length )
    |> List.map (fun (x,y) -> {x=x; y=y})
    |> List.filter(fun x ->
      state[x].isEnd || ((int state[x].c) - (int this.c)) = 1
    )
let grid =
  inputs
  |> Array.map(fun line -> line.ToCharArray())
  |> Array.mapi(fun i x -> 
    x
    |> Array.mapi(fun j c -> ( {c = c; upstream = List.empty; v=false; pos ={x= j; y=i;}}))
  )
  |> Array.collect( fun c -> c)

let start = 
  grid
  |> Array.find (fun c-> c.isStart)

let mem = 
  grid 
  |> Array.map(fun x -> (x.pos,x))
  |> Map.ofArray

let markVisited (state: Map<Position,Node>) (node: Node) =
  let node2 = {node with v = true}
  let state2 = state.Add (node.pos, node2)
  (state2, node2)

let rec explore (state: Map<Position,Node>) (node: Node) (best:int) =
  if node.isEnd then
    printfn "------\n%A : %A" node.pos (node.upstream |> List.map(fun x -> x))
    node.upstream.Length + 1
  else if node.isVisited || node.upstream.Length >= best then
    best
  else
    let (state2, node2) = markVisited state node
    let neighbours = 
      node.neighbours(state2)
      |> List.map(fun x -> state2[x])
      |> List.filter(fun x -> not x.isVisited)
      |> List.map(fun x -> {x with upstream = [yield! node2.upstream; node2.pos]})
    
    neighbours
    |>List.fold (fun acc n ->
      explore state2 n acc
    ) best

explore mem start (Map.count mem)
|> printfn "%i"