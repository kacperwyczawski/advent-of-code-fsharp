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

    printfn "\n   moved head to %d %d" newHeadPosition.X newHeadPosition.Y

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
    //
    // in match expression below, diffs are matched clockwise

    let tailTransformation =
        match xDiff, yDiff with
        | 0, 2 -> (fun p -> { X = p.X; Y = p.Y + 1 }) // T
        | 1, 2
        | 2, 1 -> (fun p -> { X = p.X + 1; Y = p.Y + 1 }) // TR
        | 2, 0 -> (fun p -> { X = p.X + 1; Y = p.Y }) // R
        | 2, -1
        | 1, -2 -> (fun p -> { X = p.X + 1; Y = p.Y - 1 }) // BR
        | 0, -2 -> (fun p -> { X = p.X; Y = p.Y - 1 }) // B
        | -1, -2
        | -2, -1 -> (fun p -> { X = p.X - 1; Y = p.Y - 1 }) // BL
        | -2, 0 -> (fun p -> { X = p.X - 1; Y = p.Y }) // L
        | -2, 1
        | -1, 2 -> (fun p -> { X = p.X - 1; Y = p.Y + 1 }) // TL
        | _ -> id // tail is still touching head

    let newTailPosition = tailTransformation state.TailPosition

    if newTailPosition = state.TailPosition then
        printfn "   tail is still touching the head"
    else
        printfn "   moved tail to %d %d" newTailPosition.X newTailPosition.Y

    { HeadPosition = newHeadPosition
      TailPosition = newTailPosition
      VisitedByTail = state.VisitedByTail |> Set.add newTailPosition }

input
|> List.collect generateCommand
|> List.fold transform initialState
|> (fun s -> s.VisitedByTail)
|> Set.count
|> printfn "Part 1: %d"
