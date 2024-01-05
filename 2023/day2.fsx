open System.Text.RegularExpressions
    
type Game =
    { Id: int
      MaxRedPick: int
      MaxGreenPick: int
      MaxBluePick: int }

let parseLine line =
    let parsePicks color =
        Regex.Matches(line, "(\d+) " + color)
        |> Seq.cast<Match>
        |> Seq.map _.Groups[1].Value
        |> Seq.map int
        |> Seq.max
    { Id = int (Regex.Match(line, "^Game (\d+)").Groups[1].Value)
      MaxRedPick = parsePicks "red"
      MaxGreenPick = parsePicks "green"
      MaxBluePick = parsePicks "blue" }

let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map parseLine
    
input
|> Seq.filter (fun game ->
    game.MaxRedPick <= 12
    && game.MaxGreenPick <= 13
    && game.MaxBluePick <= 14)
|> Seq.map _.Id
|> Seq.sum
|> printfn "Part one: %i"

input
|> Seq.map (fun game ->
    game.MaxRedPick
    * game.MaxGreenPick
    * game.MaxBluePick)
|> Seq.sum
|> printfn "Part two: %i"