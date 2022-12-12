open System
open System.Collections
let inputs=IO.File.ReadAllLines("input.txt")

type ThrowTo = 
  {
    number: int
    item: int64
  }

type Monkey = 
  {
    number: int64
    items: int64 list
    test: int64
    pass: int
    noPass: int
    op: string * string * string
    count: int64
  } 

  static member Default = 
  {
    number = 0
    items = []
    test = 0
    pass = 0
    noPass = 0
    op = ("","", "")
    count = 0
  }

  member this.inspect level =
    let throws2 =
      this.items 
      |> List.map(fun w ->
          let m =
            match this.op with
              | ("old","*", "old") -> w * w
              | ("old","+", "old") -> w + w
              | ("old","*", n) -> w * (int64 n)
              | ("old", "+", n) -> w + (int64 n)
              | _ -> failwith "unknow operation"
          let i = m / level
          match i % this.test with
            | 0L ->
              { number=this.pass; item = i }
            | _ -> 
              { number = this.noPass; item = i})
            
    ({this with items= List.empty ; count = this.count + (int64 (List.length throws2))}, throws2)

  static member parse (lines: string[])=
    lines
    |> Array.map(fun line -> line.Trim().Split(":")) 
    |> Array.fold(fun m line->
      match line with
      |[|""|] -> m
      | [|h;""|] -> 
        let b = h.Split(" ")
        {m with number = (int b.[1])} 
      | [|"Starting items"; starts|] ->
        { m with items = (starts.Split(",") |> Array.map int64 |> Array.toList)} 
      | [|"Operation"; o|] -> 
        let [|_;_;o;s;n|] = o.Trim().Split(" ")
        {m with op = (o,s, n)}
      | [|"Test"; o|] -> 
        let [|_;_;n|] = o.Trim().Split(" ")
        { m with test = (int n)}
      | [|"If true"; o|] ->
        let [|_;_;_; n|] = o.Trim().Split(" ")
        { m with pass = (int n)}
      | [|"If false"; o|] ->
        let [|_;_;_; n|] = o.Trim().Split(" ")
        { m with noPass = (int n)}
      | x -> failwithf "unknown input %A" x
    ) Monkey.Default


let state =
  inputs
  |> Array.chunkBySize(7)
  |> Array.map Monkey.parse
  |> Array.map(fun x -> 
      (x.number, x)
    )
  |> Map.ofArray

let gcd = 
  state
  |> Map.fold(fun acc _ m ->
    acc * m.test
  ) 1L

let round state (inspect_fn: Monkey -> (Monkey * list<ThrowTo>))  =
  state
  |> Map.fold(fun (acc:Map<int64,Monkey>) i (_: Monkey) ->
      let m = acc[i] 
      let (m1,throw2) = (inspect_fn m)

      let acc2 = acc.Add (m1.number, m1)
      let tmp =
        throw2
        |> List.fold(fun (a: Map<int64,Monkey>) t ->
          let r = a[t.number]
          let n =
            if t.item < gcd then
              t.item
            else
              t.item % gcd
          a.Add (r.number,{r with items = [yield! r.items; n]})
        ) acc2
      tmp
    ) state 

let fn1 =
  [1..20]
  |> List.fold(fun s _ ->
    round s (fun (m: Monkey) -> m.inspect 3L)
  ) state

let fn2 =
  [1..10000]
  |> List.fold(fun s i ->
    printfn ". %i"i
    round s (fun (m: Monkey) -> m.inspect 1L)
  ) state

let answer f =
  f
  |> Map.toList
  |> List.map(fun (_,m) -> m)
  |> List.sortByDescending(fun m -> m.count)
  |> List.take(2)
  |> List.fold(fun acc m -> acc * m.count) 1L

printfn "part1: %i" (answer fn1)
printfn "part2: %A" (answer fn2)