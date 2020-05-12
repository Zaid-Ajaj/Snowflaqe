open System
open System.Text
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

let runConfigFile (configFile: string) =
    colorprintfn "Reading configuration from $green[%s]" (resolveFile configFile)
    match readConfig configFile with
    | Error errorMessage ->
        Console.WriteLine(errorMessage)
        1
    | Ok config ->
        colorprintfn "⏳ Loading GraphQL schema from $green[%s]" config.schema
        match Introspection.loadSchema config.schema with
        | Error errorMessage ->
            colorprintfn "$red[%s]" errorMessage
            1
        | Ok schema ->
            printfn "✔️  Schema loaded successfully"
            colorprintfn "⏳ Validating queries within $green[%s]" config.queries
            let mutable errorCount = 0
            let queryFiles = Directory.GetFiles(config.queries, "*.gql")
            for queryFile in queryFiles do
                let query = File.ReadAllText queryFile
                match Query.parse query with
                | Error parseError ->
                    colorprintf "⚠️ Could not parse query $red[%s]:\n%s\n" queryFile parseError
                    errorCount <- errorCount + 1
                | Ok parsedQuery ->
                    match Query.validate parsedQuery schema with
                    | ValidationResult.Success ->
                        colorprintfn "✔️  Query $blue[%s] is valid" queryFile
                    | ValidationResult.SchemaDoesNotHaveQueryType ->
                        errorCount <- errorCount + 1
                        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
                        colorprintfn "    Schema doesn't implement a Query type"
                    | ValidationResult.SchemaDoesNotHaveMutationType ->
                        errorCount <- errorCount + 1
                        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
                        colorprintfn "    Schema doesn't implement a Mutation type"
                    | ValidationResult.NoQueryOrMutationProvided ->
                        errorCount <- errorCount + 1
                        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
                        colorprintfn "    No query or mutation request were provided"
                    | ValidationResult.QueryErrors errors ->
                        errorCount <- errorCount + 1
                        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
                        for error in errors do
                            match error with
                            | QueryError.UnknownField (fieldName, parent, typeName) ->
                                colorprintfn "   Unknown field $yellow[%s] selected from $green[%s] of type $blue[%s]" fieldName parent typeName
                            | QueryError.UnknownInputVariable (variableName, typeName) ->
                                colorprintfn "   Input variable $blue[%s] has unknown type $yellow[%s]" variableName typeName
                            | QueryError.ExpandedScalarField (fieldName, parentSelection, typeName) ->
                                colorprintfn "   Field $yellow[%s] selected from $green[%s] of type $blue[%s] is a scalar and cannot be expanded further" fieldName parentSelection typeName

            errorCount

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Encoding.UTF8
    Console.WriteLine(logo)

    match argv with
    | [| "--config"; configFile; "--validate" |] -> runConfigFile configFile
    | ([||] | [| "--validate" |]) ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile ->
                runConfigFile configFile
    | _ ->

        failwith "No config provided"
