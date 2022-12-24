open System
let inputs=IO.File.ReadAllLines("input.txt")

let Directions =[|
  (-1,-1);(-1,0);(-1,1); 
  (1,-1);(1,0);(1,1);
  (-1,-1);(0,-1);(1,-1)
  (-1,1);(0,1);(1,1);
|] 
type Point = 
  {
    x: int
    y: int
    c: char
  }
  member this.propose m dirs=
    let positions =
      dirs
      |> Array.map(fun (x,y) ->
        let p ={x = this.x+x ; y = this.y+y; c='#'}
        ((Map.containsKey p m), p)
      )
      |> Array.chunkBySize(3)
      |> Array.filter(fun x -> not( x |> Seq.exists(fun (f,_) -> f)))
      |> Array.map(fun x -> x[1])
      |> Array.map(fun (_,x) -> x)
      |> Array.toList
    if positions.Length = 4 || positions.Length = 0 then
      this
    else
      positions.Head
    
let ground =
  inputs
  |> Array.mapi(fun x row ->
    row.ToCharArray()
    |> Array.mapi(fun y c ->
      {x = x; y = y; c = c}
    )
  )
  |> Array.collect(fun r -> r|> Array.filter(fun p -> p.c = '#'))
  |> Array.toList

let display (ground: list<Point>) =
  let y0 = ground |> List.minBy (fun p -> p.y)
  let y1= ground |> List.maxBy (fun p -> p.y)
  ground 
  |> List.groupBy (fun g -> g.x)
  |> List.sortBy(fun (i,p) -> i)
  |> List.iter(fun (i,plist) ->
    let o = Array.create (y1.y - y0.y + 1) '.'
    let s =
      plist
      |> List.fold(fun (acc:char[]) p ->
        acc.[p.y - y0.y] <- '#'
        acc
      ) o
      |> String
    printfn "%i : %A \n" i s
  )

let mem =
  ground
  |> List.map(fun p -> (p, List.empty<Point>))
  |> Map.ofList

let rec round (m: Map<Point, List<Point>>)  dirs n =
  if n = 0 then
    m
  else
    let m2 =
      m
      |> Seq.fold(fun (pm: Map<Point, list<Point>>) p ->
        let p2 =p.Key.propose m dirs
        if pm.ContainsKey p2 then
          let plist = pm[p2]
          pm.Add (p2,[yield! plist; p.Key])
        else
          pm.Add (p2, [p.Key])
      ) Map.empty
      |> Seq.fold(fun (pm: Map<Point,list<Point>>) p ->
        if p.Value.Length > 1 then
          p.Value
          |> Seq.fold(fun acc y -> acc.Add (y, [])) pm
        else
          pm.Add (p.Key,[])
      ) Map.empty
    let dirs2= [|yield! dirs[3..]; yield! dirs[0..2]|]
    round m2 dirs2 (n-1)

round mem Directions 10
|> fun x ->
  display (x.Keys |> Seq.toList)


