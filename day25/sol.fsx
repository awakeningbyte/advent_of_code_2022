open System
let inputs=IO.File.ReadAllLines("input.txt")

let parse (s:string) =
  Array.foldBack(fun c (acc,i) ->
    let u =  5.0 ** i
    let acc2 =
      match c with
      |'=' -> acc - u * 2.0
      |'-' -> acc - u 
      | n when ((int n) - (int '0') < 10) -> acc + (float ((int n) - (int '0'))) * u
      | _ -> failwith "unknown input"

    (acc2, (i+1.0))
    ) (s.ToCharArray()) (0.0,0.0)

let rec encode ( acc: string, x) (n: float) =
  if n > 0 then
    let u = 5.0 
    let r = (int (n % u))
    match r + x with
    | 4 -> (encode ("-"+acc, 1) (Math.Floor (n / 5.0)))
    | 3 -> (encode ("="+acc, 1) (Math.Floor (n / 5.0)))
    | 5 -> (encode ("0"+acc, 1) (Math.Floor (n / 5.0)))
    | _ -> (encode ((r+x).ToString()+acc, 0) (Math.Floor (n / 5.0)))
  else
    if x > 0 then
      (x.ToString() + acc)
    else
      acc


inputs 
|> Array.map parse  
|> Array.sumBy(fun (x,_) ->x)
|> encode ("",0)  
|> printfn "part1: %A"