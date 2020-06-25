open System
open System.Linq
open System.Text
open Newtonsoft.Json.Linq
open System.IO
open Snowflaqe
open Snowflaqe.Types
open BlackFox.ColoredPrintf
open FSharp.Compiler.SyntaxTree

type CustomErrorType = {
    typeName : string
    typeDefinition : SynModuleDecl
}

type Config = {
    schema: string
    queries: string
    project : string
    output: string
    errorType: CustomErrorType
    target: OutputTarget
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
            elif not (isNull parsedJson.["errorType"]) && parsedJson.["errorType"].Type <> JTokenType.Object then
                Error "The 'errorType' configuration element must be an object"
            elif not (isNull parsedJson.["target"]) && parsedJson.["target"].Type <> JTokenType.String then
                Error "The 'target' configuration element must be a string"
            elif not (isNull parsedJson.["target"]) && (parsedJson.["target"].ToObject<string>().ToLower() <> "fable" && parsedJson.["target"].ToObject<string>().ToLower() <> "fsharp" && parsedJson.["target"].ToObject<string>().ToLower() <> "shared") then
                Error "The 'target' configuration element must be either 'fable' (default), 'fsharp' or 'shared'"
            else
                let errorType =
                    if isNull parsedJson.["errorType"]
                    then Ok { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() }
                    else
                        match CodeGen.parseErrorType (unbox parsedJson.["errorType"]) with
                        | Error errorMsg -> Error errorMsg
                        | Ok (typeName, typeDefinition) -> Ok { typeName = typeName; typeDefinition = typeDefinition }

                match errorType with
                | Error errorMessage -> Error errorMessage
                | Ok errorType ->
                    let queriesPath = string parsedJson.["queries"]
                    let outputPath = string parsedJson.["output"]

                    let target =
                        if isNull parsedJson.["target"] || string parsedJson.["target"] = "fable"
                        then OutputTarget.Fable
                        elif not (isNull parsedJson.["target"]) && string parsedJson.["target"] = "fsharp"
                        then OutputTarget.FSharp
                        else OutputTarget.Shared

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
                        errorType = errorType
                        target = target
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

let rec deleteFilesAndFolders directory isRoot =
    for file in Directory.GetFiles directory
        do File.Delete file
    for subdirectory in Directory.GetDirectories directory do
        deleteFilesAndFolders subdirectory false
        if not isRoot then Directory.Delete subdirectory

let generate (configFile: string) =
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
            let generatedModules = ResizeArray<string * string * bool>()

            let globalTypes = [
                yield! CodeGen.createGlobalTypes schema
                yield config.errorType.typeDefinition
            ]

            let globalTypesModule = CodeGen.createNamespace [ config.project ] globalTypes
            let file = CodeGen.createFile "Types.fs" [ globalTypesModule ]
            let globalTypesContent = CodeGen.formatAst file
            if not (Directory.Exists config.output) then
                Directory.CreateDirectory(config.output)
                |> ignore

            if config.target = OutputTarget.Shared then
                let projectPaths = [
                    Path.Combine(config.output, "shared")
                    Path.Combine(config.output, "dotnet")
                    Path.Combine(config.output, "fable")
                ]

                for path in projectPaths do
                    if not (Directory.Exists path) then
                        ignore (Directory.CreateDirectory path)

            deleteFilesAndFolders config.output true

            let projectPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".fsproj"))

            match config.target with
            | OutputTarget.FSharp ->
                let stringEnumAttrPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".StringEnum.fs"));
                colorprintfn "✏️  Generating StringEnum attribute $green[%s]" stringEnumAttrPath
                File.WriteAllText(stringEnumAttrPath, CodeGen.createDummyStringEnumAttribute())
                generatedFiles.Add(stringEnumAttrPath)
                let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".Types.fs"));
                colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
                File.WriteAllText(globalTypesPath, globalTypesContent)
                generatedFiles.Add(globalTypesPath)

            | OutputTarget.Fable ->
                let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".Types.fs"));
                colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
                File.WriteAllText(globalTypesPath, globalTypesContent)
                generatedFiles.Add(globalTypesPath)

            | OutputTarget.Shared ->
                let stringEnumAttrPath = Path.GetFullPath(Path.Combine(config.output, "shared", config.project + ".StringEnum.fs"));
                colorprintfn "✏️  Generating StringEnum attribute $green[%s]" stringEnumAttrPath
                File.WriteAllText(stringEnumAttrPath, CodeGen.createDummyStringEnumAttribute())
                generatedFiles.Add(stringEnumAttrPath)
                let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, "shared", config.project + ".Types.fs"));
                colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
                File.WriteAllText(globalTypesPath, globalTypesContent)
                generatedFiles.Add(globalTypesPath)

            for queryFile in queryFiles do
                let query = File.ReadAllText queryFile
                match Query.parse query with
                | Error _ -> ()
                | Ok query ->
                    let queryName = Query.findOperationName query
                    let queryFileName = Path.GetFileNameWithoutExtension queryFile
                    let moduleName = queryName |> Option.defaultValue queryFileName
                    let queryTypes = CodeGen.generateTypes "Query" config.errorType.typeName query schema
                    let generatedModule = CodeGen.createQualifiedModule [ config.project; moduleName ] queryTypes
                    let generatedModuleContent = CodeGen.formatAst (CodeGen.createFile moduleName [ generatedModule ])
                    match config.target with
                    | OutputTarget.Fable
                    | OutputTarget.FSharp ->
                        let fullPath = Path.GetFullPath(Path.Combine(config.output, config.project + "." + moduleName + ".fs"))
                        colorprintfn "✏️  Generating module $green[%s]" fullPath
                        File.WriteAllText(fullPath, generatedModuleContent)
                        generatedFiles.Add(fullPath)
                        generatedModules.Add(queryFile, moduleName, generatedModuleContent.Contains "type InputVariables")
                        ()
                    | OutputTarget.Shared ->
                        let fullPath = Path.GetFullPath(Path.Combine(config.output, "shared", config.project + "." + moduleName + ".fs"))
                        colorprintfn "✏️  Generating module $green[%s]" fullPath
                        File.WriteAllText(fullPath, generatedModuleContent)
                        generatedFiles.Add(fullPath)
                        generatedModules.Add(queryFile, moduleName, generatedModuleContent.Contains "type InputVariables")
                        ()

            match config.target with
            | OutputTarget.Fable ->
                let graphqlClientPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".GraphqlClient.fs"))
                colorprintfn "✏️  Generating GraphQL client $green[%s]" graphqlClientPath
                generatedFiles.Add(graphqlClientPath)
                let members =
                    generatedModules
                    |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleClientMember (File.ReadAllText(path)) name hasVars)
                    |> String.concat "\n"
                let clientContent = CodeGen.sampleGraphqlClient config.project config.errorType.typeName members
                File.WriteAllText(graphqlClientPath, clientContent)
                colorprintfn "✏️  Generating Fable project $green[%s]" projectPath
                let files =
                    generatedFiles
                    |> Seq.map (fun file -> sprintf "        <Compile Include=\"%s\" />" (Path.GetFileName file))
                    |> String.concat "\n"

                File.WriteAllText(projectPath, CodeGen.sampleFableProject files)

            | OutputTarget.FSharp ->
                colorprintfn "✏️  Generating F# project $green[%s]" projectPath

                let members =
                    generatedModules
                    |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleFSharpClientMember (File.ReadAllText(path)) name hasVars)
                    |> String.concat "\n"

                let graphqlClientPath = Path.GetFullPath(Path.Combine(config.output, config.project + ".GraphqlClient.fs"))
                generatedFiles.Add(graphqlClientPath)
                let clientContent = CodeGen.sampleFSharpGraphqlClient config.project config.errorType.typeName members
                File.WriteAllText(graphqlClientPath, clientContent)

                let files =
                    generatedFiles
                    |> Seq.map (fun file -> sprintf "        <Compile Include=\"%s\" />" (Path.GetFileName file))
                    |> String.concat "\n"

                File.WriteAllText(projectPath, CodeGen.sampleFSharpProject files)

            | OutputTarget.Shared ->

                let files =
                    generatedFiles
                    |> Seq.map (fun file -> sprintf "        <Compile Include=\"%s\" />" (Path.GetFileName file))
                    |> String.concat "\n"

                let sharedProjectPath = Path.GetFullPath(Path.Combine(config.output, "shared", config.project + ".Shared.fsproj"))
                colorprintfn "✏️  Generating shared F# project $green[%s]" sharedProjectPath
                File.WriteAllText(sharedProjectPath, CodeGen.sampleSharedProject files)

                let fsharpGraphqlClientPath = Path.GetFullPath(Path.Combine(config.output, "dotnet", config.project + ".GraphqlClient.fs"))
                let fsharpMembers =
                    generatedModules
                    |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleFSharpClientMember (File.ReadAllText(path)) name hasVars)
                    |> String.concat "\n"
                let dotnetClientContent = CodeGen.sampleFSharpGraphqlClient config.project config.errorType.typeName fsharpMembers
                File.WriteAllText(fsharpGraphqlClientPath, dotnetClientContent)
                let sharedFSharpProject = Path.GetFullPath(Path.Combine(config.output, "dotnet", config.project + ".Dotnet.fsproj"))
                colorprintfn "✏️  Generating F# dotnet project $green[%s]" sharedFSharpProject
                File.WriteAllText(sharedFSharpProject, CodeGen.sampleSharedFSharpProject config.project)

                let fableMembers =
                    generatedModules
                    |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleClientMember (File.ReadAllText(path)) name hasVars)
                    |> String.concat "\n"

                let fableGraphqlClientPath = Path.GetFullPath(Path.Combine(config.output, "fable", config.project + ".GraphqlClient.fs"))
                let fableClientContent = CodeGen.sampleGraphqlClient config.project config.errorType.typeName fableMembers
                File.WriteAllText(fableGraphqlClientPath, fableClientContent)
                let sharedFableProject = Path.GetFullPath(Path.Combine(config.output, "fable", config.project + ".Fable.fsproj"))
                colorprintfn "✏️  Generating Fable project $green[%s]" sharedFableProject
                File.WriteAllText(sharedFableProject, CodeGen.sampleSharedFableProject config.project)
            0

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
        let config = { schema = schema; queries = queries; project = "GraphqlClient"; output = "./output"; errorType = { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() }; target = OutputTarget.Fable }
        runConfig config

    | [| "--schema"; schema; "--queries"; queries |] ->
        let config = { schema = schema; queries = queries; project = "GraphqlClient"; output = "./output"; errorType = { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() }; target = OutputTarget.Fable }
        runConfig config

    | [| "--generate" |] ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile -> generate configFile

    | [| "--config"; configFile; "--generate" |] ->
        generate configFile
    | [| "--generate"; "--config"; configFile |] ->
        generate configFile
    | _ ->
        0