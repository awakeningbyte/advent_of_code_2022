open System
open System.Text.RegularExpressions
let inputs=IO.File.ReadAllLines("test.txt")

type Valve = 
  {
    name: string
    rate: int
    links: List<string>
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

let rec run  (room: Map<string,Valve>) (timer:int)  start =
  let release =
      room.Values
      |> Seq.filter(fun v -> v.opened)
      |> Seq.sumBy(fun v -> v.rate)
      
  let opens = (room |> Seq.filter(fun x -> x.Value.opened))
  if timer = 8 || (Seq.length opens) = (Map.count room) then
    (release, opens)
  else
    // printfn "time: %i" (31 - timer)

    let curr = room[start]
    let m2 = if curr.opened then room else room.Add(curr.name, {curr with opened = true})
    // printfn "links: %A" curr.links
    curr.links
    |> Seq.map(fun v -> room[v])
    // |> Seq.filter(fun v -> not (v.opened))
    |> Seq.sortByDescending(fun v -> v.rate)
    |> Seq.map(fun v -> (run m2 (timer + 1) v.name)  )
    |> Seq.maxBy(fun (x, m)-> x)

// room |> Seq.iter(fun x -> printfn "room: %A" x)
run room 0 "AA"
|> printfn "part1 %A"
