open System
let inputs=IO.File.ReadAllLines("input.txt")
let offsets =
  [(0,-1);(1,0);(0,1);(-1,0);(0,0)]
  // [(0,-1);(1,-1);(1,0);(1,1);(0,1);(0,0)]
let boundaryY= inputs[0].Length - 1
let boundaryX = inputs.Length - 1
type Point = { x: int; y: int }

type Node = { p: Point; t: Char; cnt: int } member this.isWall = this.t = '#'

let valley =
  inputs
  |> Array.mapi(fun i x -> 
      x.ToCharArray()
      |> Array.mapi(fun j c -> {p ={x = i; y = j}; t = c; cnt =0}))
  |> Array.collect(fun x -> x)

let start1 = valley |> Array.find(fun x -> x.t = '.')
let target1 = valley |> Array.findBack(fun x -> x.t = '.')

let mem =
  valley
  |> Array.filter(fun x -> not (x.t = '.'))
  |> Array.fold(fun (mp: Map<Point, List<Node>>)  node -> mp.Add (node.p, [node])) Map.empty

let moves (mp: Map<Point, List<Node>>)  (node: Node) =
  let { x = x0; y= y0 } = node.p

  offsets
  |> List.map(fun (x,y) -> { x = (x0+x); y =(y0+y) })
  |> List.filter(fun p -> 
    ((p.x >= 0) && (p.y > 0) && (p.x <= boundaryX) && (p.y < boundaryY)))
  |> List.filter(fun p -> not (mp.ContainsKey p))
  |> List.map(fun p -> {p = p; t='m'; cnt=node.cnt+1})

let blow (m: Map<Point, List<Node>>) (m2: Map<Point, List<Node>>) (n2: Node) =
  let p2 =
    match n2.t with
      | '>' -> {n2.p with y = if n2.p.y+1 = boundaryY then 1 else  n2.p.y+1 }
      | '<' -> {n2.p with y = if n2.p.y-1 = 0 then boundaryY - 1 else  n2.p.y-1 }
      | '^' -> {n2.p with x = if n2.p.x-1= 0 then boundaryX - 1 else n2.p.x-1}
      | 'v' -> {n2.p with x = if n2.p.x + 1 = boundaryX then 1 else n2.p.x + 1}
      | ukw -> failwithf "%A is unkown direction" ukw 
  if m2.ContainsKey p2 then
    m2.Add (p2, [yield! m2[p2]; {n2 with p=p2}])
  else
    m2.Add (p2, [{n2 with p=p2}])

let  update (m: Map<Point, List<Node>>)  =
  m
  |> Seq.fold(fun (mp: Map<Point,List<Node>>) (kv: Collections.Generic.KeyValuePair<Point,List<Node>>) -> 
    let (p, nodes) = (kv.Key,kv.Value)
    match nodes with
    | [n] when n.isWall -> mp.Add (p, nodes)
    | _ -> 
      nodes
      |> List.fold(fun m2 n2 -> 
        blow m m2 n2) mp
   ) Map.empty
   
let display (m: Map<Point,List<Node>>) =
  Seq.allPairs [0..boundaryX] [0..boundaryY]
  |> Seq.map (fun (x,y) -> {x =x; y=y})
  |> Seq.groupBy(fun p-> p.x)
  |> Seq.sortBy(fun (i,g)-> 1)
  |> Seq.map(fun (_, points) ->
    let s = Array.create<string> (boundaryY+1) "."
    points
    |> Seq.fold(fun (acc: string[]) p -> 
      if m.ContainsKey p then
        let nodes = m[p]
        match nodes with
        | [n] -> 
          acc[p.y] <- ( n.t.ToString())
        | ns -> 
          acc[p.y] <- ns.Length.ToString() 
      acc
    ) s
  )
|> Seq.iter (printfn "%A")

let rec bfs (m: Map<Point, List<Node>>) (que: List<Node>) target =
  let m2 = update m    
  let nexts =
      que 
      |> List.map( moves m2 )
      |> List.collect(fun x -> x)
      |> List.distinct
  if (nexts |> List.exists(fun (x: Node) -> x.p = target.p)) then
    (nexts.Head.cnt, m2)
  else
    bfs m2 nexts target

let (p1,mn1) = bfs mem [start1] target1 
let (p2,mn2) = bfs mn1 [target1] start1
let (p3, _) = bfs mn2 [start1] target1

p1 |> printfn "part1: %i"
(p1+p2+p3) |> printfn "part2: %i"
