[<AutoOpen>]
module Snowflaqe.Utilities

open System
open System.IO
open Newtonsoft.Json.Linq

let resolveFile (path: string) =
    if Path.IsPathRooted path
    then path
    else Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))

let stringOrNone (json: JToken) (key: string) = 
    if json.Type = JTokenType.Object then 
        let dict = unbox<JObject> json 
        if dict.ContainsKey key && not (String.IsNullOrWhiteSpace (string dict.[key]))
        then Some (string dict.[key])
        else None 
    else None

let listOrNone (xs: ResizeArray<'t>) = 
    if isNull xs 
    then [ ]
    else List.ofSeq xs