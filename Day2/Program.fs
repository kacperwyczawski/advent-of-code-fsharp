open System
open System.IO

let input = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> Array.toList

// A - rock
// B - paper
// C - scissors

// X - rock (1 point)
// Y - paper (2 points)
// Z - scissors (3 points)

// win - 6 points
// draw - 3 points
// lose - 0 points

let getScore = function
    | "A X" -> 1 + 3
    | "A Y" -> 2 + 6
    | "A Z" -> 3 + 0
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 1 + 6
    | "C Y" -> 2 + 0
    | "C Z" -> 3 + 3
    | _ -> failwith "Invalid input"
    
let totalScore = input |> List.map getScore |> List.sum

printfn "Total score: %d" totalScore

// --- Part Two ---

// X - lose
// Y - draw
// Z - win

let getScore2 = function
    | "A X" -> 3 + 0
    | "A Y" -> 1 + 3
    | "A Z" -> 2 + 6
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 2 + 0
    | "C Y" -> 3 + 3
    | "C Z" -> 1 + 6
    | _ -> failwith "Invalid input"
    
let totalScore2 = input |> List.map getScore2 |> List.sum

printfn "Total score: %d" totalScore2