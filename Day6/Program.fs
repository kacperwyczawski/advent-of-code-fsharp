open System
open System.IO

let input = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> Array.exactlyOne

let signalSubroutine signal =
    let rec loop (index: int) (acc: char list) : int =
        if [ acc[0]; acc[1]; acc[2]; acc[3] ] |> List.distinct |> List.length = 4 then
            index + 4
        else
            loop (index + 1) (List.tail acc)
            
    loop 0 signal

input
|> List.ofSeq
|> signalSubroutine
|> printfn "%d"