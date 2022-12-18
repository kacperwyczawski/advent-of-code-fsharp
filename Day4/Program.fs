open System
open System.IO

let input = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> Array.toList

type Elf = { sectionStart: int; sectionEnd: int }

let parseElfPair (str: string) : Elf * Elf =
    let pair = str.Split(',')
    let firstElf = pair.[0].Split('-')
    let secondElf = pair.[1].Split('-')
    
    { sectionStart = int firstElf.[0]; sectionEnd = int firstElf.[1] },
    { sectionStart = int secondElf.[0]; sectionEnd = int secondElf.[1] }

let parseInput = List.map parseElfPair

let orderBySize pair =
    let length =
        (pair |> fst |> (fun e -> e.sectionEnd - e.sectionStart)),
        (pair |> snd |> (fun e -> e.sectionEnd - e.sectionStart))

    if fst length < snd length then
        pair
    else
        (snd pair, fst pair)

let fullyContains pair =
    let smaller, larger = pair |> orderBySize

    smaller.sectionStart >= larger.sectionStart
    && smaller.sectionEnd <= larger.sectionEnd

let solution = input |> parseInput |> List.filter fullyContains |> List.length

printfn "%d" solution
