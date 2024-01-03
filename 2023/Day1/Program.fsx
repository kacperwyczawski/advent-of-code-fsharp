open System.IO
open System.Text.RegularExpressions

let extractNumber (text: string) =
    let isDigit c =
        Regex.IsMatch(string c, "\d")
    let first =
        Seq.find isDigit text
    let last =
        Seq.findBack isDigit text
    int (string first + string last)

File.ReadLines "input.txt"
|> Seq.map extractNumber
|> Seq.sum
|> printfn "%i"