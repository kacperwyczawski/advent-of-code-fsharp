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