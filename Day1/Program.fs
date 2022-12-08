let input =
    [ "1000"
      "2000"
      "3000"
      ""
      "4000"
      ""
      "5000"
      "6000"
      ""
      "7000"
      "8000"
      "9000"
      ""
      "10000" ]

let rec splitList (calories: string list) : string list list =
    let rec splitOne (acc: string list) (list: string list) : string list * string list =
        match list with
        // return if there is no more elements
        | [] -> (acc, list)
        | head :: tail ->
            if head = "" then
                // if encounter empty string, return (without empty string)
                printfn "splitOne: empty string encountered, returning %A and %A" acc list.Tail
                (acc, list.Tail)
            else
                // add head to accumulator
                printfn "splitOne: adding %A to accumulator" head
                splitOne (head :: acc) tail

    let rec splitAll (acc: string list list) (list: string list) : string list list =
        if list = [] then
            // return if there is no more elements
            printfn "splitAll: no more elements, returning %A" acc
            acc
        else
            let oneElf, rest = splitOne [] list
            // add elf's calories array to accumulator
            printfn "splitAll: adding %A to accumulator" oneElf
            splitAll (oneElf :: acc) rest

    let result =
        calories |> splitAll [] |> List.rev

    printfn "splitList: returning %A" result
    result

let allElves =
    input
    |> splitList
    |> List.map (List.map int)
    |> List.map List.sum

let winner = allElves |> List.max

printfn
    "%d. elf is carrying most calories: %d"
    (allElves
     |> List.findIndex (fun x -> x = winner)
     |> (fun x -> x + 1))
    winner