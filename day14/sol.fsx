// open System
// let inputs=IO.File.ReadAllLines("test.txt")

// let cave =
//   inputs
//   |> Seq.collect(fun line -> 
//       line.Split("->")
//       |> Array.map(fun u ->
//         let [|x;y|] = u.Split(",") |> Array.map int
//         (x,y))
//       |> Array.pairwise
//     )
//   |> Seq.sortBy(fun ((x,y),_) -> (x * 1000 + y))

// let fall p cave abyss =
//   cave