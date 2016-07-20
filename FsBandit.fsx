#load "packages/FsLab/FsLab.fsx"
#load "packages/FsLab/Themes/AtomChester.fsx"
#load "Secret.fsx"

open Secret
open System
open FSharp.Charting

(**
FsBandits Workshop
==================

Task 1
|> Explore the bandits using FSharpInteractive.
|> Find the average result for each bandit.
|> How many pulls do we need to start getting consistent results?
|> Plot the average values

Extensions
|> Explore Improving performance by performing work in parallel over multiple cores
(Note: Array.map can be written as Array.Parallel.map)

**)

//Example Timed Function
let example a =
  a ** 0.8

let example' = timed example
example' 3.

//Example performing an action 100 times
[|1 .. 100|] |> Array.map (fun _ -> example 3.4)
// or
[| for x in 1 .. 100 -> example 3.4 |]

//Evaluating a Bandit
bandits.[0]()

//Plotting a graph
let highData = [ for x in 1.0 .. 100.0 -> (x, 3000.0 + x ** 2.0) ]
Chart.Line(highData,Name="Rates").WithYAxis(Min=2000.0).WithXAxis(Log=true)

(**

* With large amounts of pulls we can start getting quite accurate results
* What if we are limited by the number of pulls we can make?

Task 2
|> Create an EpsilonGreedy Algorithm
|> Run a Monte Carlo simulation using your algorithm, allow for 5000 picks
|> Try running many iterations of the simulation and average over the results
|> How does the Algorithm Perform with different values of epsilon?
|> Plot the total output for different values of epsilon

Extensions
|> Plot the history of outcomes for multiple values of epsilon
|> Plot the cumulative history of outcomes for multiple values of epsilon

**)
type HistoricalResult = { Iteration: int; Bandit: int; Outcome: float}

let rec Simulation imax i previousResults algorithm =
  let banditNo = algorithm previousResults
  let newResult = { Iteration = i; Outcome = bandits.[banditNo](); Bandit = banditNo }
  let combinedResult = newResult :: previousResults
  if (i >= imax) then
    combinedResult
  else
    Simulation imax (i + 1) combinedResult algorithm

let Simulation' = Simulation 5000 1 []

let epsilonGreedyAlgorithm e previousResults =
  // Write Bandit Here
  rnd(0,5)

let runSimulation e = Simulation' (epsilonGreedyAlgorithm e)

let runSimulations bandit n =
  [|1 .. n|] |> Array.Parallel.map (fun _ -> Simulation' bandit) |> Array.toList

let results = runSimulations (epsilonGreedyAlgorithm 0.1) 100

///// Helper Functions //////
let TotalOutcome = List.sumBy (fun x -> x.Outcome)
let AvgTotalOutcome = List.averageBy TotalOutcome
let ModeBy func =
  List.groupBy func
  >> List.maxBy (snd >> List.length)
  >> fst
let MostCommonBandit =
  ModeBy (fun x -> x.Bandit)
let AvgMostCommonBandit =
  ModeBy MostCommonBandit
//////////////////////////////

results |> AvgMostCommonBandit

// Array of values from 0 to 1 in 0.1 increments
[|0. .. 0.1 .. 1.0|]

(**

Extension - Task 3
|> Implement the UCB1 Bandit Algorithm: Details should be available online!
|> Try plotting the output of this algorithm over time
|> How does it compare to epsilon greedy?

**)
