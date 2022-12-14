open System
let inputs=IO.File.ReadAllLines("test.txt")

type Package =
  | Single of int
  | Multi of list<Package>

type Pair = {items: list<Package>; closed: bool}
let rec parse (s:list<char>) (stack: list<Pair>) (cur: Option<Pair>) (buff: list<char>)=
  match s with
  | [] -> stack.Head
  | '['::tail -> 
    match cur with
    | Some(p) -> 
      parse tail [p;yield! stack;] (Some({items=list.Empty; closed=false})) list.Empty
    | None -> parse tail stack (Some({items=list.Empty; closed=false})) list.Empty
      //parse tail [yield! acc; "["] List.empty
  | ']'::tail ->
    let p = 
      if buff.IsEmpty then 
        { cur.Value with closed = true }
      else {cur.Value with closed = true; items = [yield! cur.Value.items; Single(int (System.String.Concat buff))]}
    
    if stack.IsEmpty then
      parse tail [ p ] None list.Empty
    else
      let c::r = stack
      parse tail r (Some({c with items=[yield! c.items; Multi(p.items)]})) list.Empty
  | ','::tail ->
    if buff.IsEmpty then 
      parse tail stack cur  list.Empty
    else 
      parse tail stack (Some({cur.Value with items = [yield! cur.Value.items; Single(int (System.String.Concat buff))]}))  list.Empty
  | c::tail -> 
      parse tail stack cur [yield! buff; c]

inputs
|> Seq.filter(fun line -> line.Length > 0)
|> Seq.map(fun x ->
   parse (x.ToCharArray() |> Array.toList) list.Empty None list.Empty)
|> Seq.map(fun x -> x.items)
|> Seq.chunkBySize(2)
|> Seq.iteri(fun i x -> printfn "%i\n%A\n" (i + 1) x)
|> ignore
