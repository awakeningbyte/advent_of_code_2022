open System
open System.Collections.Generic
let inputs=IO.File.ReadAllLines("input.txt") |> Array.toList

type FileType =
    | File of int * string
    | Dir of (string list)

let dict = new Dictionary<(string list), seq<FileType>>()

let rec travel (lines: string list) pwd = 
    match lines with
    | head::tail ->
        let segs = head.Split(" ")
        match segs with
        | [|"$";"cd";"/"|] -> 
            if not (dict.ContainsKey ["/"]) then
                dict.Add (["/"], Seq.empty<FileType>)
            travel tail ["/" ]
        | [|"$";"cd";".."|] -> travel tail (List.tail pwd)
        | [|"$";"cd";d|] -> 
            let f = [d; yield! pwd]
            if not (dict.ContainsKey f) then
                dict.Add (f, Seq.empty<FileType>)
            travel tail  f
        | [|"$";"ls"|] -> travel tail pwd
        | [|"dir";d |] -> 
            let f = [d; yield! pwd]
            let ls = dict.[pwd]
            dict.[pwd] <- [yield! ls; Dir(f)]
            travel tail pwd
        | [|s; n|] ->
            let ls = dict.[pwd]
            dict.[pwd] <- [yield! ls; File((int s), n)]
            travel tail pwd  
    | [] -> ()

travel inputs ["/"]

let mem = new Dictionary<(string list), int>()

let rec calSize (x: FileType) = 
    match x with
    | File(s, n) -> s
    | Dir(y) ->
        if mem.ContainsKey(y) then
            mem.[y]
        else
            let contents  = dict.[y]
            contents
            |> Seq.map calSize 
            |> Seq.sum

let rootSize = calSize (Dir ["/"])

printfn "-- %i" (70000000 - rootSize)
dict.Keys
|> Seq.map(fun x ->
    calSize (Dir x)
)
|> Seq.sort
|> Seq.filter(fun s -> s > (30000000- 70000000 + rootSize))
|> Seq.min
|> printfn "%i"