open System
let inputs=IO.File.ReadAllLines("input.txt")

inputs
|> Array.map(fun x -> 
    x.Split(",")
    )
|> Array.map(fun [|a;b|] -> 
        let x = a.Split("-") |> Array.map int
        let y = b.Split("-") |> Array.map int
        Array.zip x y
    )
|> Array.map (fun [|(x1,y1);(x2,y2)|] ->
    let p1 =
        if ((x1 >= y1 && x2 <= y2)) || ((x1 <= y1) && (x2 >= y2)) then 
            1 
        else 
            0
    let p2 =
        if (y1 >=x2 && y1<=x1) || (y2 <= x2 && y2 >= x1) || (x1 >=y1 && x1<=y2) || (x2 >= y1 && x2 <= y2) then 
            1 
        else 
            0
    (p1,p2)
)
|> Array.reduce(fun (p1,p2) (x,y) -> (p1+x, p2+y)) 
|> printfn "%A"