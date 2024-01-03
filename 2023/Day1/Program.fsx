open System.IO
open System.Text.RegularExpressions

let extractNumber (text: string) =
    let (|StartsWithDigit|_|) text =
        let regexMatch = Regex.Match(text, "^\d")
        if regexMatch.Success then
            Some(int regexMatch.Value)
        else
            None
    
    let (|EndsWithDigit|_|) text =
        let regexMatch = Regex.Match(text, "\d$")
        if regexMatch.Success then
            Some(int regexMatch.Value)
        else
            None
    
    let rec firstNumber text =
        match text with
        | StartsWithDigit x -> x
        | x when x.StartsWith("one") -> 1
        | x when x.StartsWith("two") -> 2
        | x when x.StartsWith("three") -> 3
        | x when x.StartsWith("four") -> 4
        | x when x.StartsWith("five") -> 5
        | x when x.StartsWith("six") -> 6
        | x when x.StartsWith("seven") -> 7
        | x when x.StartsWith("eight") -> 8
        | x when x.StartsWith("nine") -> 9
        | x when x.StartsWith("zero") -> 0
        | _ -> firstNumber(text.Substring(1))
        
    let rec lastNumber text =
        match text with
        | EndsWithDigit x -> x
        | x when x.EndsWith("one") -> 1
        | x when x.EndsWith("two") -> 2
        | x when x.EndsWith("three") -> 3
        | x when x.EndsWith("four") -> 4
        | x when x.EndsWith("five") -> 5
        | x when x.EndsWith("six") -> 6
        | x when x.EndsWith("seven") -> 7
        | x when x.EndsWith("eight") -> 8
        | x when x.EndsWith("nine") -> 9
        | x when x.EndsWith("zero") -> 0
        | _ -> lastNumber(text.Substring(0, text.Length - 1))
    
    10 * firstNumber text + lastNumber text

File.ReadLines "input.txt"
|> Seq.map extractNumber
|> Seq.sum
|> printfn "%i"