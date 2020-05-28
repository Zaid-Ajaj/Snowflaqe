open System
open System.Linq
open System.Text
open Newtonsoft.Json.Linq
open System.IO
open Snowflaqe
open Snowflaqe.Types
open BlackFox.ColoredPrintf
open FsAst
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.Range
open Fantomas

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
                    elif Path.HasExtension queriesPath
                    then resolveFile queriesPath
                    else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, queriesPath))

                Ok {
                    schema = string parsedJson.["schema"]
                    queries = fullQueriesPath
                    project = string parsedJson.["project"]
                }
    with
    | ex -> Error ex.Message

let runConfig (config: Config) =
    colorprintfn "⏳ Loading GraphQL schema from $green[%s]" config.schema
    match Introspection.loadSchema config.schema with
    | Error errorMessage ->
        colorprintfn "$red[%s]" errorMessage
        1
    | Ok schema ->
        printfn "✔️  Schema loaded successfully"
        colorprintfn "⏳ Validating queries within $green[%s]" config.queries
        let mutable errorCount = 0
        let queryFiles = Directory.GetFiles(config.queries, "*.gql") |> Seq.map Path.GetFullPath
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
                        | QueryError.UnknownFieldArgument (argumentName, fieldName, typeName) ->
                            colorprintfn "   Field $blue[%s] has an unknown argument $yellow[%s]" fieldName argumentName
                        | QueryError.MissingRequiredArgument (argumentName, fieldName, typeName) ->
                            colorprintfn "   Field $blue[%s] is missing required argument $yellow[%s]" fieldName argumentName
                        | QueryError.ArgumentAndVariableTypeMismatch(fieldName, argumentName, argumentType, variableName, variableType) ->
                            colorprintfn "   Field $blue[%s] contains type mismatch for argument $blue[%s] of type $green[%s] which references variable $yellow[%s] of type $yellow[%s]" fieldName argumentName argumentType ("$" + variableName) variableType
                        | QueryError.ArgumentTypeMismatch(fieldName, argumentName, argumentType, providedType) ->
                            colorprintfn "   Field $blue[%s] contains invalid provided value for argument $blue[%s] of type $green[%s]" fieldName argumentName argumentType
                        | QueryError.NullUsedForNonNullableType(fieldName, argumentName, argumentType) ->
                            colorprintfn "   Field $blue[%s] cannot use value null for non-nullable argument $blue[%s] of type $green[%s]" fieldName argumentName argumentType
                        | QueryError.NullableVariableUsedWithNonNullableArgument(fieldName, argumentName, variableName) ->
                            colorprintfn "   Field $blue[%s] references a nullable variable $yellow[%s] for non-nullable argument $blue[%s]" fieldName argumentName variableName
                        | QueryError.UnknownEnumCase(fieldName, argumentName, argumentType, enumCase) ->
                            colorprintfn "   Field $blue[%s] uses an unknown enum case $yellow[%s] for argument $blue[%s] of type $green[%s]" fieldName enumCase argumentName argumentType
                        | QueryError.UnknownInputObjectField(inputObjectName, unknownField) ->
                            colorprintfn "   Unknown field $yellow[%s] used from input object of type $green[%s]" unknownField inputObjectName
                        | QueryError.UsedNonDeclaredVariable(fieldName, argumentName, variableName) ->
                            colorprintfn "   Field $blue[%s] references a variable $yellow[%s] in argument $blue[%s] that was not declared" fieldName variableName argumentName
                        | QueryError.MissingRequiredFieldFromInputObject(inputObjectName, inputObjectType, requiredFieldName) ->
                            colorprintfn "   Input object $blue[%s] of type $green[%s] is missing required field $yellow[%s]" inputObjectName inputObjectType requiredFieldName
        errorCount

let runConfigFile (configFile: string) =
    colorprintfn "Reading configuration from $green[%s]" (resolveFile configFile)
    match readConfig configFile with
    | Error errorMessage ->
        Console.WriteLine(errorMessage)
        1
    | Ok config -> runConfig config

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Encoding.UTF8
    Console.WriteLine(logo)

    match argv with
    | [| "--config"; configFile|] ->
        runConfigFile configFile

    | [| |] ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile ->
                runConfigFile configFile

    | [| "--queries"; queries; "--schema"; schema; |] ->
        let config = { schema = schema; queries = queries; project = "GraphqlClient" }
        runConfig config

    | [| "--schema"; schema; "--queries"; queries |] ->
        let config = { schema = schema; queries = queries; project = "GraphqlClient" }
        runConfig config

    | [| "--generate" |] ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile ->

                match runConfigFile configFile with
                | 0 ->
                    printfn "Generate code here"
                    match readConfig configFile with
                    | Error _ -> 0
                    | Ok config ->
                        match Introspection.loadSchema config.schema with
                        | Error _ -> 0
                        | Ok schema ->
                            let globalTypes = CodeGen.createGlobalTypes schema
                            let ns = CodeGen.createNamespace config.project globalTypes
                            let file = CodeGen.createFile "Types.fs" [ ns ]
                            let content = CodeGen.formatAst file
                            printfn "%s" content
                            0

                | n ->
                    n

    | _ ->

        failwith "No config provided"
