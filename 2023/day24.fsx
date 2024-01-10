// WARNING
// This code is not working as intended.
// I misunderstood the problem and the solution requires different approach.
// ( calculating point of intersection between two linear functions )


open System.Text.RegularExpressions

type Hailstone =
    { px: int
      py: int
      vx: int
      vy: int }
    
type Trajectory = Trajectory of (int * int) list

let testStart = 7
let testEnd = 27

let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun line ->
        let m = Regex.Match(
            line,
            "(?<px>\d+), (?<py>\d+).*@ (?<vx>(?:-| )\d+), (?<vy>(?:-| )\d+)")
        { px = int m.Groups.["px"].Value
          py = int m.Groups.["py"].Value
          vx = int m.Groups.["vx"].Value
          vy = int m.Groups.["vy"].Value })
    |> Seq.toList
    
let generateTrajectory hailstone =
    let initial = (hailstone.px, hailstone.py)
    let result = List.unfold (fun state ->
        if fst state >= testEnd
            || snd state >= testEnd
            || fst state <= testStart
            || snd state <= testStart
        then
            None
        else
            Some (state, (fst state + hailstone.vx, snd state + hailstone.vy))) initial
    Trajectory result
    
let rec combinations list =
    match list with
    | [] -> []
    | x::xs -> (List.map (fun y -> (x, y)) xs) @ (combinations xs)
    

let isColliding (Trajectory t1, Trajectory t2) =
    t1
    |> List.exists (fun (x1, y1) ->
        t2
        |> List.exists (fun (x2, y2) ->
            x1 = x2 && y1 = y2))

input
|> List.map generateTrajectory
|> combinations
|> List.filter isColliding
|> List.length