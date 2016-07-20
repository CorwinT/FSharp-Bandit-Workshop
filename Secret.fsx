open System
open System.Diagnostics
open System.Threading

let rnd =
     let seedGenerator = new Random()
     let localGenerator = new ThreadLocal<Random>(fun _ ->
       lock seedGenerator (fun _ ->
         let seed = seedGenerator.Next()
         new Random(seed)))
     fun (min, max) ->
       localGenerator.Value.Next(min, max)

let randomPick seq = (Seq.sortBy (fun _ -> rnd(1,1000000)) >> Seq.head) seq
let bandits = [|
  (fun (_:unit) -> (seq { for x in 1. .. 100. -> if x <= 95. then 0. else 47000. }) |> randomPick)
  (fun _ -> (seq { for x in 1. .. 100. -> 25. * x + 1650. }) |> randomPick)
  (fun _ -> (seq { for x in 1. .. 100. -> if x < 50. then 0. else 3200. }) |> randomPick)
  (fun _ -> (seq { for x in 1. .. 100. -> if x <= 99. then 650. else 500000. }) |> randomPick)
  (fun _ -> (seq { for x in 1. .. 200. -> if x <= 199. then 0. else 850000. }) |> randomPick)
|]

let timed func a =
  let watch = new Stopwatch()
  watch.Start()
  let result = func a
  watch.Stop()
  printf "Time Taken: %ims" watch.ElapsedMilliseconds
  result
