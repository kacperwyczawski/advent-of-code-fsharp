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

let splitList (calories: string list) : string list list =
    let result = 
        calories
        |> List.fold (fun (acc: string list list) (calorie: string) ->
            if calorie = "" then
                // append new empty list to acc
                [] :: acc
            else
                match acc with
                // if acc is empty, append new list with calorie to acc
                | [] -> [[calorie]]
                // append calorie to first list in acc (which is the last list modified)
                | head :: tail -> (calorie :: head) :: tail
            ) []
        
    result |> List.rev

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