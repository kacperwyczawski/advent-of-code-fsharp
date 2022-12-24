open Utilities

let input = Input.loadLines

// list string is used to represent path
type FileSystem =
    { CurrentPath: string list
      Items: Map<string list, int> }

// we take path from current path in file system
let addSize (size: int) (fileSystem: FileSystem) : FileSystem =
    // example: [ "a"; "b"; "/" ] -> [ [ "a"; "b"; "/" ]; [ "b"; "/" ]; [ "/" ] ]
    let breakPath (path: 'a list) : 'a list list =
        let rec loop (path: 'a list) (acc: 'a list list) : 'a list list =
            match path with
            | [] -> acc
            | _ -> loop (List.tail path) (path :: acc)

        loop path []

    // folder
    let addSizeToPath (size: int) (fileSystem: FileSystem) (path: string list) : FileSystem =
        let updateSize =
            function
            | Some currentSize -> Some(currentSize + int size)
            | None -> Some(int size)

        { fileSystem with Items = fileSystem.Items |> Map.change path updateSize }

    let folder = addSizeToPath size

    fileSystem.CurrentPath |> breakPath |> List.fold folder fileSystem

let parseLine (fileSystem: FileSystem) (line: string) : FileSystem =
    let split = line.Split(' ') |> List.ofArray

    match split with
    | [ "$"; "cd"; ".." ] -> { fileSystem with CurrentPath = fileSystem.CurrentPath |> List.tail }
    | [ "$"; "cd"; name ] -> { fileSystem with CurrentPath = name :: fileSystem.CurrentPath }
    | [ "$"; "ls" ] -> fileSystem
    | [ "dir"; _ ] -> fileSystem
    | [ size; _ ] -> fileSystem |> addSize (int size)
    | _ -> failwithf "Unknown line: %s" line

let emptyFileSystem = { CurrentPath = []; Items = Map.empty }

let solution =
    input
    |> List.fold parseLine emptyFileSystem
    |> fun fs -> fs.Items |> Map.values
    |> Seq.filter (fun size -> size < 100000)
    |> Seq.sum
    
printfn "%d" solution