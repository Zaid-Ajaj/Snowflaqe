[<AutoOpen>]
module Snowflake.Utilities

open System
open System.IO

let resolveFile (path: string) =
    if Path.IsPathRooted path
    then path
    else Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))