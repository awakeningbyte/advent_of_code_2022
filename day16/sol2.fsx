open System
open System.Text.RegularExpressions
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("test.txt")

type Valve = 
  {
    id: int
    name: string
    rate: int
    links: string list
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
  |> Array.mapi(fun i [name; rate; links] ->
    (name, {id = i; name = name; rate = (int rate); links = (links.Split(",") |> Array.map(fun x -> x.Trim()) |> Array.toList);})
  )
  |> Map.ofSeq
// |> printfn "%A"

let full =
  room.Values
  |> Seq.filter(fun v -> not (v.rate = 0))
  |> Seq.fold(fun acc v -> 
    let idx = 1L <<< v.id
    acc ||| idx
  ) 0L

let opens = 0L
let mem = new Dictionary<(string * int * int64), int>()

let rec maxflow (s: string) (opens: int64) (timer: int): int =
  if timer <= 0 || opens = full then
    printfn "final s: %A timer: %A opens: %A" s timer opens
    0
  else
  let key = (s, timer, opens)
  if mem.ContainsKey(key) then
    mem[key]
  else
    let curr = room[s]
    let idx = 1 <<< curr.id
    let rate = curr.rate
    if rate = 0 || (opens &&& idx) > 0 then
      curr.links
      |> List.map(fun x ->
        maxflow x opens (timer - 1)
      ) 
      |> List.max
    else
      //open
      let opens2 = (opens ||| idx)

      let rt1 =
        curr.links
        |> List.map(fun x ->
          (timer - 1) * rate + (maxflow x opens2 (timer - 2))
        )
        |> List.max
      //not open
      let rt2 =
        curr.links
        |> List.map(fun x ->
          maxflow x opens (timer - 1)
        )
        |> List.max
      
      mem[key] <- if rt1 > rt2 then rt1 else rt2
      if timer = 30 then
        printfn "%A" mem[key] 
        printfn "%A" ((maxflow "AA" 0 26) + mem[key] )
      
      mem[key]
