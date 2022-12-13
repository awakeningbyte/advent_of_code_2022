open System
let inputs=IO.File.ReadAllLines("input.txt")
type Position = 
  {x: int; y:int; }

type Node = 
  {c: char; upstream: list<Position>; v: bool; pos : Position}
  member this.isStart = this.c = 'S'
  member this.isEnd = this.c = 'E'
  member this.isVisited = this.v
  member this.neighbours = 
    [(this.pos.x - 1, this.pos.y ); 
      (this.pos.x + 1, this.pos.y);
      (this.pos.x, this.pos.y - 1 ); 
      (this.pos.x, this.pos.y + 1)]
    |> List.filter(fun (x,y) -> x >=0 && y >=0 )
    |> List.filter(fun (x,y) -> x < inputs[0].Length && y < inputs.Length )
    |> List.map (fun (x,y) -> {x=x; y=y})

let grid =
  inputs
  |> Array.map(fun line -> line.ToCharArray())
  |> Array.mapi(fun i x -> 
    x
    |> Array.mapi(fun j c -> ( {c = c; upstream = List.empty; v=false; pos ={x= j; y=i;}}))
  )
  |> Array.collect( fun c -> c)

let state = 
  grid 
  |> Array.map(fun x -> (x.pos,x))
  |> Map.ofArray

let start = 
  grid
  |> Array.find (fun c-> c.isStart)

let rec bfs ((st : Map<Position, Node>),(q: list<Position>)) =
  match q with
  | [] -> 
    st
    |> Seq.sortBy (fun m ->
        let (k, v) = m.Deconstruct()
        k.y * inputs[0].Length + k.x
    )
    |> Seq.iter(fun m ->
        let (k, v) = m.Deconstruct()
        if k.x = 0 then
         printf ("%i\t") (k.y + 1)
        if k.x = (inputs.[0].Length - 1) then
          printf ("\n")
         
        else
          if v.isVisited then
            printf "%c" v.c
          else
            printf "."
    )
    failwith "not found"
  | e::tail when st[e].isEnd ->
    st[e].upstream.Length
  | h::tail ->
    let node = st[h]
    let upstream = node.upstream
    let neighbours =
      node.neighbours
      |> List.map (fun x -> st[x])
      |> List.filter(fun x -> 
        (x.isEnd && node.c = 'z')
        || (node.isStart && x.c = 'a')
        || (int x.c) - (int node.c) = 1
        || (int x.c) - (int node.c) = 0
        || (int x.c) < (int node.c) 
      )
    neighbours
    |> List.fold(fun (accS: Map<Position,Node>, (accQ: list<Position>)) (n: Node) ->
      if n.isVisited then
        (accS, accQ)
      else
        (accS.Add (n.pos, {n with v = true; upstream = [yield! upstream; node.pos]}), [yield! accQ; n.pos])
    ) (st, tail)
    |> bfs
  
bfs (state, [start.pos]) |> printfn "%i"

