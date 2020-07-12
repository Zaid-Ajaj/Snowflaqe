module Utilities

open System

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