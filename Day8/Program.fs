open Utilities

let input = Input.loadLines

type View =
    | Top of int list list
    | Bottom of int list list
    | Left of int list list
    | Right of int list list

type Point = { X: int; Y: int }

// steps:
// parse input
// for all views:
//     get list of visible trees
// distinct concatenated list
// count

let parseInput (input: string list) =
    let charToInt c = int c - 48

    let raw: int list list =
        input |> List.map List.ofSeq |> List.map (List.map charToInt)

    let left = Left raw

    let right = raw |> List.map List.rev |> Right

    let top = raw |> List.transpose |> Top

    let bottom = raw |> List.transpose |> List.map List.rev |> Bottom

    [ left; right; top; bottom ]

let getVisibleTrees (view: View) =
    let getInLine pointModifier startingPoint grid =
        let rec visibleInLine line highest visibleTrees point =
            match line with
            | [] -> visibleTrees
            | head :: tail ->
                match highest with
                | None -> visibleInLine tail (Some head) (point :: visibleTrees) (pointModifier point)
                | Some h ->
                    if head > h then
                        visibleInLine tail (Some head) (point :: visibleTrees) (pointModifier point)
                    else
                        visibleInLine tail (Some h) visibleTrees (pointModifier point)

        grid |> List.mapi (fun i line -> visibleInLine line None [] (startingPoint i))
        
    match view with
    | Top grid -> getInLine (fun p -> { p with Y = p.Y + 1 }) (fun i -> { X = i; Y = 0 }) grid
    | Bottom grid -> getInLine (fun p -> { p with Y = p.Y - 1 }) (fun i -> { X = i; Y = grid.Length - 1 }) grid
    | Left grid -> getInLine (fun p -> { p with X = p.X + 1 }) (fun i -> { X = 0; Y = i }) grid
    | Right grid -> getInLine (fun p -> { p with X = p.X - 1 }) (fun i -> { X = grid.Length - 1; Y = i }) grid

let solution =
    input
    |> parseInput
    |> List.map getVisibleTrees // views, lines and points
    |> List.concat // views and points
    |> List.concat // points
    |> List.distinct
    |> List.length

printfn "%d" solution