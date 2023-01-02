open System
let inputs=IO.File.ReadAllLines("input.txt")
let Directions = [|'R';'D';'L';'U'|]
let empIdx =
  inputs
  |> Array.findIndex(String.IsNullOrEmpty)

let rec parse (str: string) (instructions: list<(char * int)> )=
  let chars = str.ToCharArray() |> List.ofArray
  match chars with
  | [] -> instructions
  | h::tail when (Array.contains h Directions) -> 
    let i = tail |> List.tryFindIndex(fun c -> (Array.contains c Directions))
    match i with
    | Some(x) ->
      let n =  tail[0..(x-1)] |> Array.ofList |> String |> int
      let t = tail[x..] |> Array.ofList |> String

      let instructions2 = [yield! instructions; (h, n)]
      parse t instructions2
    | None -> [yield! instructions; (h, (int (tail |> Array.ofList |> String)))]
  | _ -> failwith "unknow instruction"

let board = inputs[0..(empIdx -  1)] 
let instructions = parse ("R"+inputs[empIdx+1]) []

let rec move (board: string[]) (d, n) (x,y) =
  if n = 0 then
    ((x,y), true)
  else
    match d with
    | 'R' -> 
      let row = board[x]
      let o = row.IndexOfAny([|'.';'#'|])
      let y1 = (y+1) % (row.Trim().Length) + o

      let c = board[x][y1]
      if c = '#' then
        ((x,y), false)
      else
        move board (d, (n - 1)) (x,y1)
    | 'L' -> 
      let row = board[x]
      let o = row.IndexOfAny([|'.';'#'|])
      let N = (row.Trim().Length)
      let y1 = ((y-1) % N + N) % N + o

      let c = board[x][y1]
      if c = '#' then
        ((x,y), false)
      else
        move board (d, (n - 1)) (x,y1)
    | 'D' ->
      let x1 = 
        if (x + 1) = board.Length || (board[x+1][y]=' ') then 
          (board |> Array.findIndex(fun row -> row.Length > y && (row[y] = '.' || row[y]='#')))
        else
          x + 1

      let c = board[x1][y]
      if c = '#' then
        ((x,y), false)
      else
        move board (d, (n - 1)) (x1,y)
    | 'U' ->
      let x1 = 
        if (x - 1) < 0 || (board[x-1][y]=' ') then 
          (board |> Array.findIndexBack (fun row -> row.Length > y && (row[y] = '.' || row[y]='#')))
        else
          x - 1

      let c = board[x1][y]
      if c = '#' then
        ((x,y), false)
      else
        move board (d, (n - 1)) (x1,y)
    | _ -> failwith "unknow direction to go" 
    // let c = board[x][y]
    // match c with
    // | '#' ->

let rec go board (instructions: list<(char*int)>) (x, y) =
  match instructions with
  | [] -> (x,y)
  | h::tail ->
      let (r, f) = move board h (x,y)
      if f then
        go board tail r
      else
        r

      // match h with
      
      // | ('R', n) -> (x,y)
      // | ('L', n) -> (x,y)
      // | ('U', n) -> (x,y)
      // | ('D', n) -> (x,y)
      // | _ -> failwith "unknown operation"

go board instructions (0, 8)
|> printfn "%A"


