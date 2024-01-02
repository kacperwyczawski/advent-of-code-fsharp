open System
open System.IO

type Crate = { name: char }
type Stack = { number: int; crates: Crate list }
type Yard = { stacks: Stack list }

let yard =
    { stacks =
        [ { number = 1
            crates = // F T C L R P G Q
              [ { name = 'F' }
                { name = 'T' }
                { name = 'C' }
                { name = 'L' }
                { name = 'R' }
                { name = 'P' }
                { name = 'G' }
                { name = 'Q' } ] }
          { number = 2
            crates = // N Q H W R F S J
              [ { name = 'N' }
                { name = 'Q' }
                { name = 'H' }
                { name = 'W' }
                { name = 'R' }
                { name = 'F' }
                { name = 'S' }
                { name = 'J' } ] }
          { number = 3
            crates = // F B H W P M Q
              [ { name = 'F' }
                { name = 'B' }
                { name = 'H' }
                { name = 'W' }
                { name = 'P' }
                { name = 'M' }
                { name = 'Q' } ] }
          { number = 4
            crates = // V S T D F
              [ { name = 'V' }
                { name = 'S' }
                { name = 'T' }
                { name = 'D' }
                { name = 'F' } ] }
          { number = 5
            crates = // Q L D W V F Z
              [ { name = 'Q' }
                { name = 'L' }
                { name = 'D' }
                { name = 'W' }
                { name = 'V' }
                { name = 'F' }
                { name = 'Z' } ] }
          { number = 6
            crates = // Z C L S
              [ { name = 'Z' }; { name = 'C' }; { name = 'L' }; { name = 'S' } ] }
          { number = 7
            crates = // Z B M V D F
              [ { name = 'Z' }
                { name = 'B' }
                { name = 'M' }
                { name = 'V' }
                { name = 'D' }
                { name = 'F' } ] }
          { number = 8
            crates = // T J B
              [ { name = 'T' }; { name = 'J' }; { name = 'B' } ] }
          { number = 9
            crates = // Q N B G L S P H
              [ { name = 'Q' }
                { name = 'N' }
                { name = 'B' }
                { name = 'G' }
                { name = 'L' }
                { name = 'S' }
                { name = 'P' }
                { name = 'H' } ] } ] }

let input =
    File.ReadAllLines $@"C:\Users\{Environment.UserName}\Desktop\input.txt"
    |> Array.toList

type Move =
    { crateCount: int
      fromStack: int
      toStack: int }

let parseInputLine (line: string) =
    let split = line.Split(' ')

    { crateCount = int split[1]
      fromStack = int split[3]
      toStack = int split[5] }
    
type Crane = CrateMover9000 | CrateMover9001

let executeMove (crane: Crane) (yard: Yard) (move: Move) : Yard =
    // 1. take crates from stack
    // 2. reverse crates
    // 3. put crates on stack

    let getPayload =
        yard.stacks
        |> List.find (fun s -> s.number = move.fromStack) // select stack to move from
        |> fun s -> s.crates // unwrap the stack
        |> List.rev // reverse crates in stack
        |> List.take move.crateCount // take crates from the end
        |> List.rev // reverse crates again
        
    let payload =
        match crane with
        | CrateMover9000 -> List.rev getPayload
        | CrateMover9001 -> getPayload

    let resultYard =
        { yard with
            stacks =
                yard.stacks
                |> List.map (fun s ->
                    if s.number = move.fromStack then
                        { s with crates = s.crates |> List.rev |> List.skip move.crateCount |> List.rev } // remove crates from "from stack"
                    elif s.number = move.toStack then
                        { s with crates = s.crates @ payload } // append payload to "to stack"
                    else
                        s) } // leave other stacks untouched

    resultYard

let parsedInput = input |> List.map parseInputLine

let part1Yard = parsedInput |> List.fold (executeMove CrateMover9000) yard
let part2Yard = parsedInput |> List.fold (executeMove CrateMover9001) yard

let getAnswer yard =
    yard.stacks
    |> List.map (fun stack -> List.last stack.crates) // get last crate of each stack
    |> List.map (fun crate -> crate.name) // get name of each crate
    |> Array.ofList // convert to array
    |> String // convert to string
    
printfn "Part 1: %s" (getAnswer part1Yard)
printfn "Part 2: %s" (getAnswer part2Yard)