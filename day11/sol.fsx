open System
open System.Collections
let inputs=IO.File.ReadAllLines("input.txt")

type ThrowTo = 
  {
    number: int
    item: int
  }

type Monkey = 
  {
    number: int
    items: int list
    test: int
    pass: int
    noPass: int
    op: string * string * string
    count: int
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

  member this.inspect  =
    let throws2 =
      this.items 
      |> List.map(fun w ->
          let m =
            match this.op with
              | ("old","*", "old") -> w * w
              | ("old","+", "old") -> w + w
              | ("old","*", n) -> w * (int n)
              | ("old", "+", n) -> w + (int n)
              | _ -> failwith "unknow operation"
          let i = m / 3
          match i % this.test with
            | 0 ->
              { number=this.pass; item = i }
            | _ -> 
              { number = this.noPass; item = i})
            
    ({this with items= List.empty ; count = this.count + (List.length throws2)}, throws2)

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
        { m with items = (starts.Split(",") |> Array.map int |> Array.toList)} 
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

let round state  =
  state
  |> Map.fold(fun (acc:Map<int,Monkey>) i (_: Monkey) ->
      let m = acc[i] 
      let (m1,throw2) = m.inspect

      let acc2 = acc.Add (m1.number, m1)
      let tmp =
        throw2
        |> List.fold(fun (a: Map<int,Monkey>) t ->
          let r = a[t.number]
          a.Add (r.number,{r with items = [yield! r.items; t.item]})
        ) acc2

      // printfn "-------\n Monkey %A: \n thows: \n%A \n" m1 throw2
      tmp
    ) state 
  // |> printfn "\n===============\n%A"
let fnl =
  [1..20]
  |> List.fold(fun s _ ->
    round s
  ) state

fnl
|> Map.toList
|> List.map(fun (_,m) -> m)
|> List.sortByDescending(fun m -> m.count)
|> List.take(2)
|> List.fold(fun acc m -> acc * m.count) 1
|> printfn "%A"
