module Utilities

open System
open System.IO
open System.Linq
let trimContentEnd (content: string) =
    let lines =
      content
        .Replace(String [| '\010' |], Environment.NewLine)
        .Split Environment.NewLine

    lines
    |> Array.skipWhile String.IsNullOrWhiteSpace
    |> Array.rev
    |> Array.skipWhile String.IsNullOrWhiteSpace
    |> Array.rev
    |> Array.map (fun line -> line.Trim())


/// Recursively tries to find the parent of a file starting from a directory
let rec findParent (directory: string) (fileToFind: string) =
    let path = if Directory.Exists(directory) then directory else Directory.GetParent(directory).FullName
    let files = Directory.GetFiles(path)
    if files.Any(fun file -> Path.GetFileName(file).ToLower() = fileToFind.ToLower())
    then path
    else findParent (DirectoryInfo(path).Parent.FullName) fileToFind


let path xs = Path.Combine(Array.ofList xs)

let solutionRoot = findParent __SOURCE_DIRECTORY__ "Snowflaqe.sln";
let tests = path [ solutionRoot; "tests" ]