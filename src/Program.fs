open System
open Newtonsoft.Json.Linq
open System.IO
open Snowflaqe
open Snowflaqe.Types
open BlackFox.ColoredPrintf

type Config = {
    schema: string
    queries: string
    project : string
}

type CommandLineArgs =
    | Validate of Config
    | Generate of Config

let logo = """
     _____                      __ _
    /  ___|                    / _| |
    \ `--. _ __   _____      _| |_| | __ _  __ _  ___
     `--. \ '_ \ / _ \ \ /\ / /  _| |/ _` |/ _` |/ _ \
    /\__/ / | | | (_) \ V  V /| | | | (_| | (_| |  __/
    \____/|_| |_|\___/ \_/\_/ |_| |_|\__,_|\__, |\___|
                                              | |
                                              |_|

    ❤️  Open source https://www.github.com/Zaid-Ajaj/Snowflaqe
    ⚖️  MIT LICENSE
"""

let readConfig (file: string) =
    let path = resolveFile file
    try
        if not (File.Exists path) then
            Error (sprintf "Could not find configuration file %s" path)
        else
            let contents = File.ReadAllText path
            let parsedJson = JObject.Parse(contents)
            if not (parsedJson.ContainsKey "schema") then
                Error "Configuration file missing 'schema' element"
            elif parsedJson.["schema"].Type <> JTokenType.String then
                Error "The 'schema' configuration element must be a string"
            elif not (parsedJson.ContainsKey "queries") then
                Error "Configuration file missing 'queries' element"
            elif parsedJson.["queries"].Type <> JTokenType.String then
                Error "The 'queries' configuration element must be a string"
            elif not (parsedJson.ContainsKey "project") then
                Error "Configuration file missing 'project' element"
            elif parsedJson.["project"].Type <> JTokenType.String then
                Error "The 'project' configuration element must be a string"
            else
                let queriesPath = string parsedJson.["queries"]

                let fullQueriesPath =
                    if Path.IsPathRooted queriesPath
                    then queriesPath
                    else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, queriesPath))

                Ok {
                    schema = string parsedJson.["schema"]
                    queries = fullQueriesPath
                    project = string parsedJson.["project"]
                }
    with
    | ex -> Error ex.Message

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--config"; configFile; "--validate" |] ->
        Console.WriteLine(logo)
        colorprintfn "Reading configuration from $green[%s]" (resolveFile configFile)
        match readConfig configFile with
        | Error errorMessage ->
            Console.WriteLine(errorMessage)
            1
        | Ok config ->
            colorprintfn "⏳ Loading schema from $yellow[%s]" config.schema
            match Introspection.loadSchema config.schema with
            | Error errorMessage ->
                colorprintfn "$red[%s]" errorMessage
                1
            | Ok schema ->
                printfn "✔️  Schema loaded successfully"
                colorprintfn "⏳ Validating queries within $blue[%s]" config.queries
                let mutable errorCount = 0
                let queryFiles = Directory.GetFiles(config.queries, "*.gql")
                for queryFile in queryFiles do
                    let query = File.ReadAllText queryFile
                    match Query.parse query with
                    | Error parseError ->
                        colorprintf "❌ Could not parse query $red[%s]:\n%s\n" queryFile parseError
                        errorCount <- errorCount + 1
                    | Ok parsedQuery ->
                        match Query.validate parsedQuery schema with
                        | ValidationResult.Success ->
                            colorprintfn "✔️  Query $blue[%s] is valid" queryFile
                        | otherwise ->
                            errorCount <- errorCount + 1
                            colorprintfn "❌ Error while validating query $red[%s]" queryFile
                            colorprintfn "    |---- $red[%A]" otherwise
                errorCount

    | _ ->

        failwith "No config provided"
