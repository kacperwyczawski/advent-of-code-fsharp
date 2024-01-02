open Utilities

let parseInput (input: string list) =
    let charToInt c = int c - 48
    input |> List.map List.ofSeq |> List.map (List.map charToInt)
    
let input = parseInput Input.loadLines

// --- Part One ---

type OutsideView =
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

let getOutsideViews grid =
    let left = Left grid
    let right = grid |> List.map List.rev |> Right
    let top = grid |> List.transpose |> Top
    let bottom = grid |> List.transpose |> List.map List.rev |> Bottom
    [ left; right; top; bottom ]

let getVisibleTrees (view: OutsideView) =
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

let solutionOne =
    input
    |> getOutsideViews
    |> List.map getVisibleTrees // views, lines and points
    |> List.concat // views and points
    |> List.concat // points
    |> List.distinct
    |> List.length

printfn "Part 1: %d" solutionOne

// --- Part Two ---

type TreeHouseSpot =
    { Height: int
      ViewTop: int list
      ViewBottom: int list
      ViewLeft: int list
      ViewRight: int list }

let getTreeHouseSpots forest =
    let height = forest |> List.length
    let width = forest |> List.head |> List.length
    
    let rec getView (forest: int list list) x y acc breakCondition coordsModifier =
        if breakCondition x y then
            acc |> List.rev
        else
            let updatedX, updatedY = coordsModifier x y
            getView forest updatedX updatedY (forest[y][x] :: acc) breakCondition coordsModifier

    let rec getViews x y acc =
        if y >= height then // if we are at the bottom of the forest, we are done
            acc
        else if x >= width then // if we are at the end of the line, go to the next line
            getViews 0 (y + 1) acc
        else
            let newSpot =
                { Height = forest[y][x]
                  ViewTop = getView forest x (y - 1) [] (fun _ y -> y < 0) (fun x y -> x, y - 1)
                  ViewBottom = getView forest x (y + 1) [] (fun _ y -> y >= height) (fun x y -> x, y + 1)
                  ViewLeft = getView forest (x - 1) y [] (fun x _ -> x < 0) (fun x y -> x - 1, y)
                  ViewRight = getView forest (x + 1) y [] (fun x _ -> x >= width) (fun x y -> x + 1, y) }

            getViews (x + 1) y (newSpot :: acc)

    getViews 0 0 []

let getScenicScore spot =
    let viewingDistance height line =
        line
        |> List.tryFindIndex (fun x -> x >= height)
        |> (function
        | Some x -> x + 1
        | None -> line.Length)

    let top = spot.ViewTop |> viewingDistance spot.Height
    let bottom = spot.ViewBottom |> viewingDistance spot.Height
    let left = spot.ViewLeft |> viewingDistance spot.Height
    let right = spot.ViewRight |> viewingDistance spot.Height

    top * bottom * left * right

let solutionTwo =
    input
    |> getTreeHouseSpots
    |> List.map getScenicScore
    |> List.max
    
printfn "Part 2: %d" solutionTwo