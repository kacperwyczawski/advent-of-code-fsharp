﻿let input =
    [ "1000"
      "2000"
      "3000"
      ""
      "4000"
      ""
      "5000"
      "6000"
      ""
      "7000"
      "8000"
      "9000"
      ""
      "10000" ]

let parseInput input =
    List.map (fun s -> if s = "" then None else Some(int s)) input

let splitList (calories: int option list) : int list list =
    let result =
        calories
        |> List.fold
            (fun (acc: int list list) (calorie: int option) ->
                match calorie with
                | None -> [] :: acc
                | Some calorie ->
                    match acc with
                    // if acc is empty, append new list with calorie to acc
                    | [] -> [ [ calorie ] ]
                    // append calorie to first list in acc (which is the last list modified)
                    | head :: tail -> (calorie :: head) :: tail)
            [] // <-- initial value of acc

    result |> List.rev

let allElves =
    input |> parseInput |> splitList |> List.map (List.map int) |> List.map List.sum

let winner = allElves |> List.max

printfn
    "#%d elf is carrying most calories: %d"
    (allElves |> List.findIndex (fun x -> x = winner) |> (fun x -> x + 1))
    winner
