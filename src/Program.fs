open System
open System.Linq
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
    output: string
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
            elif parsedJson.["output"].Type <> JTokenType.String then
                Error "The 'output' configuration element must be a string"
            else
                let queriesPath = string parsedJson.["queries"]
                let outputPath = string parsedJson.["output"]

                let fullQueriesPath =
                    if Path.IsPathRooted queriesPath
                    then queriesPath
                    else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, queriesPath))

                let fullOutputPath =
                    if Path.IsPathRooted outputPath
                    then outputPath
                    else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, outputPath))

                Ok {
                    schema = string parsedJson.["schema"]
                    queries = fullQueriesPath
                    project = string parsedJson.["project"]
                    output = outputPath
                }
    with
    | ex -> Error ex.Message

let validateAndPrint (queryFile: string) (document: GraphqlDocument) (schema: GraphqlSchema) =
    match Query.validate document schema with
    | ValidationResult.Success ->
        colorprintfn "✔️  Query $blue[%s] is valid" queryFile
        true
    | ValidationResult.SchemaDoesNotHaveQueryType ->
        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
        colorprintfn "    Schema doesn't implement a Query type"
        false
    | ValidationResult.SchemaDoesNotHaveMutationType ->
        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
        colorprintfn "    Schema doesn't implement a Mutation type"
        false
    | ValidationResult.NoQueryOrMutationProvided ->
        colorprintfn "⚠️  Error while validating query $red[%s]" queryFile
        colorprintfn "    No query or mutation request were provided"
        false
    | ValidationResult.QueryErrors errors ->
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
        false

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
                if not (validateAndPrint queryFile parsedQuery schema)
                then errorCount <- errorCount + 1
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
        let config = { schema = schema; queries = queries; project = "GraphqlClient"; output = "./output" }
        runConfig config

    | [| "--schema"; schema; "--queries"; queries |] ->
        let config = { schema = schema; queries = queries; project = "GraphqlClient"; output = "./output" }
        runConfig config

    | [| "--generate" |] ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile ->
                printfn "Generate code here"
                match readConfig configFile with
                | Error errorMessage ->
                    colorprintfn "$red[%s]" errorMessage
                    1
                | Ok config ->
                    colorprintfn "⏳ Loading GraphQL schema from $green[%s]" config.schema
                    match Introspection.loadSchema config.schema with
                    | Error errorMessage ->
                        colorprintfn "$red[%s]" errorMessage
                        1
                    | Ok schema ->
                        let mutable invalidQuery = false
                        let queryFiles = Directory.GetFiles(config.queries, "*.gql") |> Seq.map Path.GetFullPath
                        for queryFile in queryFiles do
                            if not invalidQuery then
                                let query = File.ReadAllText queryFile
                                match Query.parse query with
                                | Error parseError ->
                                    colorprintf "⚠️ Could not parse query $red[%s]:\n%s\n" queryFile parseError
                                    invalidQuery <- true
                                | Ok query ->
                                    invalidQuery <- not (validateAndPrint queryFile query schema)

                        if invalidQuery then
                            1
                        else

                        let generatedFiles = ResizeArray<string>()
                        let globalTypes = CodeGen.createGlobalTypes schema
                        let globalTypesModule = CodeGen.createNamespace config.project globalTypes
                        let file = CodeGen.createFile "Types.fs" [ globalTypesModule ]
                        let globalTypesContent = CodeGen.formatAst file
                        if not (Directory.Exists config.output) then
                            Directory.CreateDirectory(config.output)
                            |> ignore

                        for file in Directory.GetFiles(config.output) do File.Delete file

                        let projectPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".fsproj"))
                        let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, "Types.fs"));

                        colorprintfn "✏️  Generating project $green[%s]" projectPath
                        File.WriteAllText(projectPath, CodeGen.sampleFableProject)
                        colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
                        File.WriteAllText(globalTypesPath, globalTypesContent)

                        generatedFiles.Add(projectPath)
                        generatedFiles.Add(globalTypesPath)

                        for queryFile in queryFiles do
                            let query = File.ReadAllText queryFile
                            match Query.parse query with
                            | Error _ -> ()
                            | Ok query ->
                                let queryName = Query.findOperationName query
                                let queryFileName = Path.GetFileNameWithoutExtension queryFile
                                let moduleName = queryName |> Option.defaultValue queryFileName
                                let queryTypes = CodeGen.generateTypes "Query" query schema
                                let generatedModule = CodeGen.createQualifiedModule [ config.project; moduleName ] queryTypes
                                let generatedModuleContent = CodeGen.formatAst (CodeGen.createFile moduleName [ generatedModule ])
                                let fullPath = Path.GetFullPath(Path.Combine(config.output, moduleName + ".fs"))
                                colorprintfn "✏️  Generating module $green[%s]" fullPath
                                File.WriteAllText(fullPath, generatedModuleContent)
                                generatedFiles.Add(fullPath)
                                ()
                        0

    | _ ->
        0