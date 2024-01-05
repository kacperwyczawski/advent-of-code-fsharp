open System.IO
open System.Text.RegularExpressions

let humanNumbers = [|
    "zero" // actually it seems that the input doesn't contain "zero"
    "one"
    "two"
    "three"
    "four"
    "five"
    "six"
    "seven"
    "eight"
    "nine"
|]

[<TailCall>]
let rec firstNumber text =
    let regexMatch = Regex.Match(text, "^\d")
    if regexMatch.Success then
        int regexMatch.Value
    else
        humanNumbers
        |> Array.tryFindIndex (fun (x: string) -> text.StartsWith(x))
        |> Option.defaultValue (text.Substring(1) |> firstNumber)

[<TailCall>]
let rec lastNumber text =
    let regexMatch = Regex.Match(text, "\d$")
    if regexMatch.Success then
        int regexMatch.Value
    else
        humanNumbers
        |> Array.tryFindIndex (fun (x: string) -> text.EndsWith(x))
        |> Option.defaultValue (text.Remove(text.Length - 1) |> lastNumber)

File.ReadLines "input.txt"
|> Seq.map (fun line -> 10 * firstNumber line + lastNumber line)
|> Seq.sum
|> printfn "%i"