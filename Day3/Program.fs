﻿open System
open System.IO

let input = File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt" |> Array.toList


type Item = { category: char }

type Compartment = { items: Item list }

type Backpack =
    { firstCompartment: Compartment
      secondCompartment: Compartment }

let parseInput input =

    let parseItem c = { category = c }

    let parseCompartment string =
        let items = string |> Seq.map parseItem |> List.ofSeq
        { items = items }

    let parseBackpack string =
        let compartments =
            string |> Seq.splitInto 2 |> Seq.map parseCompartment |> List.ofSeq

        { firstCompartment = List.head compartments
          secondCompartment = List.last compartments }

    input |> Seq.map parseBackpack |> List.ofSeq

let getPriority item =
    let value = int item.category

    if value >= 65 && value <= 90 then value - 38
    elif value >= 97 && value <= 122 then value - 96
    else failwith "Invalid item category"

let intersect backpack =
    let firstCompartment = backpack.firstCompartment.items |> Set.ofList
    let secondCompartment = backpack.secondCompartment.items |> Set.ofList

    (Set.intersect firstCompartment secondCompartment) |> Set.toList

let solution =
    input
    |> parseInput
    |> List.map intersect
    |> List.concat
    |> List.map getPriority
    |> List.sum

printfn "%d" solution