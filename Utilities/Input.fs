namespace Utilities

open System
open System.IO

module Input =
    let loadLines = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> List.ofArray
    let loadText = File.ReadAllText $@"C:\Users\{Environment.UserName}\Desktop\input.txt"