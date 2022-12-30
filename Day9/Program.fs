open Utilities

let input = Input.loadLines

type Point = { X: int; Y: int }

let generateCommand (line: string) =
    let parts = line.Split ' ' |> (fun l -> l[0], int l[1])

    match parts with
    | "R", n -> List.replicate n (fun p -> { p with X = p.X + 1 })
    | "L", n -> List.replicate n (fun p -> { p with X = p.X - 1 })
    | "U", n -> List.replicate n (fun p -> { p with Y = p.Y + 1 })
    | "D", n -> List.replicate n (fun p -> { p with Y = p.Y - 1 })
    | _ -> failwith "Invalid command"

type State =
    { HeadPosition: Point
      TailPosition: Point
      VisitedByTail: Set<Point> }

let initialState =
    { HeadPosition = { X = 0; Y = 0 }
      TailPosition = { X = 0; Y = 0 }
      VisitedByTail = Set.singleton { X = 0; Y = 0 } }

let transform state command =
    let newHeadPosition = command state.HeadPosition

    let xDiff = newHeadPosition.X - state.TailPosition.X
    let yDiff = newHeadPosition.Y - state.TailPosition.Y

    // H - head
    // T - tail
    // T, B, L, R - sides
    //
    //       T
    //   . H H H .
    //   H . . . H
    // L H . T . H R
    //   H . . . H
    //   . H H H .
    //       B

    let tailTransformation =
        match xDiff, yDiff with
        // Top
        | _, 2 -> (fun p -> { p with Y = p.Y + 1 })
        // Bottom
        | _, -2 -> (fun p -> { p with Y = p.Y - 1 })
        // Left
        | -2, _ -> (fun p -> { p with X = p.X - 1 })
        // Right
        | 2, _ -> (fun p -> { p with X = p.X + 1 })
        // Tail is still touching the head
        | _, _ -> id

    let newTailPosition = tailTransformation state.TailPosition

    { HeadPosition = newHeadPosition
      TailPosition = newTailPosition
      VisitedByTail = state.VisitedByTail |> Set.add newTailPosition }

input
|> List.collect generateCommand
|> List.fold transform initialState
|> (fun s -> s.VisitedByTail)
|> Set.count
|> printfn "Part 1: %d"