namespace Utilities

open System
open System.IO

module Input =
    let private desktopPath =
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop)

    let loadLines =
        [| desktopPath; "input.txt" |]
        |> Path.Combine
        |> File.ReadAllLines
        |> List.ofArray

    let loadTestLines =
        [| desktopPath; "test.txt" |]
        |> Path.Combine
        |> File.ReadAllLines
        |> List.ofArray

    let loadText =
        [| desktopPath; "input.txt" |]
        |> Path.Combine
        |> File.ReadAllText

    let loadTestText =
        [| desktopPath; "test.txt" |]
        |> Path.Combine
        |> File.ReadAllText
