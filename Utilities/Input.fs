namespace Utilities

open System
open System.IO

module Input =
    let private getPath fileName =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), fileName + ".txt")

    let loadLines = "input" |> getPath |> File.ReadAllLines |> List.ofArray

    let loadTestLines = "test" |> getPath |> File.ReadAllLines |> List.ofArray

    let loadText = "input" |> getPath |> File.ReadAllText

    let loadTestText = "test" |> getPath |> File.ReadAllText
