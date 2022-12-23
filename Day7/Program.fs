// thanks to https://github.com/gustavgahm for the idea of using Map of path and items to represent file system

open System.Text.RegularExpressions
open Utilities

let input = Input.loadLines

type File = { Name: string; Size: int }
type Directory = { Name: string }

type FileSystemItem =
    | File of File
    | Directory of Directory

type Path = Path of string list

let goBack path =
    match path with
    | Path (x :: xs) -> Path xs
    | _ -> path
    
let goTo name path =
    match path with
    | Path xs -> Path (name :: xs)

type FileSystem =
    { CurrentPath: Path
      Items: Map<Path, FileSystemItem list> }

let addToFileSystem fileSystem path item =
    let items = fileSystem.Items |> Map.tryFind path
    let updatedItems =
        match items with
        | Some items -> items @ [item]
        | None -> [item]
    { fileSystem with Items = Map.add path updatedItems fileSystem.Items }

let emptyFileSystem =
    { CurrentPath = Path []
      Items = Map.empty }
    
// types of lines:
// $ cd <name>
// $ ls
// dir <name>
// <size> <name>

// active patterns
let (|ChangeDirectory|_|) (str: string) =
    if str.StartsWith "$ cd " then
        Some (str.Substring 5)
    else
        None

let (|ListItems|_|) (str: string) =
    if str = "$ ls" then Some() else None

let (|Directory|_|) (str: string) =
    if str.StartsWith("dir ") then
        let dir = { Name = str.Substring(4) }
        FileSystemItem.Directory(dir) |> Some
    else
        None

let (|File|_|) (str: string) =
    let regexMatch = Regex(@"(\d+) (.+)").Match(str)

    if regexMatch.Success then
        let size: int = int (regexMatch.Groups.[1].Value)
        let name: string = regexMatch.Groups.[2].Value
        let file = { Name = name; Size = size }
        FileSystemItem.File(file) |> Some
    else
        None

let processLine fileSystem line =
    match line with
    | ChangeDirectory name ->
        if name = ".." then
            { fileSystem with CurrentPath = fileSystem.CurrentPath |> goBack }
        else
            { fileSystem with CurrentPath = fileSystem.CurrentPath |> goTo name }
    | ListItems -> fileSystem
    | Directory dir ->
        addToFileSystem fileSystem fileSystem.CurrentPath dir
    | File file ->
        addToFileSystem fileSystem fileSystem.CurrentPath file
    | other -> other |> sprintf "Unrecognized command: `%s`" |> failwith

let fileSystem =
    input
    |> List.fold processLine emptyFileSystem
    
printfn "file system: %A" fileSystem