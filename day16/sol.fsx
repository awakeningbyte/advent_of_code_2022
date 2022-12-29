open System
open System.Text.RegularExpressions
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("test.txt")

type Valve = 
  {
    name: string
    rate: int
    links: string list
    opened: bool
  }

let rex = Regex(@"Valve (\w+) has flow rate=(\d+); tunnel(?:s)? lead(?:s)? to valve(?:s)? (.+)")

let room =
  inputs
  |> Array.map(fun x -> 
      (rex.Match  x).Groups
      |> Seq.map (fun g -> g.Value )  
      |> Seq.skip(1)
      |> List.ofSeq
    )
  |> Array.map(fun [name; rate; links] ->
    (name, { name = name; rate = (int rate); links = (links.Split(",") |> Array.map(fun x -> x.Trim()) |> Array.toList); opened = false})
  )
  |> Map.ofSeq
// |> printfn "%A"
let NonZero = room.Values |> Seq.filter(fun x -> x.rate > 0) |> Seq.length
let mem = new Dictionary<int * string * seq<string>, int>()

let rec run  (room: Map<string,Valve>) (timer:int) (opens: Set<string>)  start: int =
  // let opens = 
  //   (room |> Seq.filter(fun x -> x.Value.opened))
  printfn "%A" timer
  let key =  
    opens
    |> Set.toList
    |> Seq.sort
    |> fun os -> 
      (timer, start, os)
  if mem.ContainsKey(key) then
    mem[key]
  else
    let release =
        room.Values
        |> Seq.filter(fun v -> v.opened)
        |> Seq.sumBy(fun v -> v.rate)

    if timer = 0 || (Seq.length opens) = NonZero then
      release
    else
      let curr = room[start]
      let result =
        if curr.rate = 0 || curr.opened then

          let m2 = if curr.opened then room else room.Add(curr.name, {curr with opened = true})
          let opens2 = opens.Add(curr.name)

          
          curr.links
          |> Seq.map(fun v -> room[v])
          |> Seq.map(fun v -> (run m2 (timer - 1) opens2 v.name))
          |> Seq.max
        else
          let m2 = room.Add(curr.name, {curr with opened = true})
          let opens2 = opens.Add(curr.name)
          let rt1 = run m2 (timer - 1) opens2 curr.name
          let key =  
            opens2
            |> Set.toList
            |> Seq.sort
            |> fun os -> 
              (timer, start, os)

          let rt2 =
            curr.links
            |> Seq.map(fun v -> room[v])
            |> Seq.map(fun v -> (run room (timer - 1) opens v.name))
            |> Seq.max
          if rt1 > rt2 then
            rt1
          else
            rt2
      mem[key] <-result
      mem[key]

run room 30 Set.empty "AA"
|> printfn "part1 %A"
