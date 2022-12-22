﻿open System
open System.IO

let input = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> Array.exactlyOne

let signalSubroutine markerLength signal =
    let rec loop (index: int) (acc: char list) : int =
        if acc |> List.take markerLength |> List.distinct |> List.length = markerLength then
            index + markerLength
        else
            loop (index + 1) (List.tail acc)
            
    loop 0 signal

input
|> List.ofSeq
|> signalSubroutine 4
|> printfn "Part 1: %d"

input
|> List.ofSeq
|> signalSubroutine 14
|> printfn "Part 2: %d"