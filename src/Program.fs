open System
open System.Globalization
open System.IO
open System.Text
open System.Xml
open System.Xml.Linq
open FSharp.Compiler.SyntaxTree
open FSharp.Data.LiteralProviders
open BlackFox.ColoredPrintf
open Newtonsoft.Json.Linq
open Snowflaqe
open Snowflaqe.Types

type CustomErrorType = {
    typeName : string
    typeDefinition : SynModuleDecl
}

type Config = {
    schema : string
    queries : string
    project : string
    output : string
    errorType : CustomErrorType
    target : OutputTarget
    createProjectFile : bool
    overrideClientName : string option
    copyLocalLockFileAssemblies : bool option
    emitMetadata: bool
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

let [<Literal>] projectFile = TextFile<"./Snowflaqe.fsproj">.Text

let projectVersion =
    let doc = XmlDocument()
    use content = new MemoryStream(Text.Encoding.UTF8.GetBytes projectFile)
    doc.Load(content)
    doc.GetElementsByTagName("Version").[0].InnerText

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
            elif not (isNull parsedJson.["overrideClientName"]) && parsedJson.["overrideClientName"].Type <> JTokenType.String then
                Error "The 'overrideClientName' configuration element must be a string"
            elif not (isNull parsedJson.["target"]) && (parsedJson.["target"].ToObject<string>().ToLower() <> "fable" && parsedJson.["target"].ToObject<string>().ToLower() <> "fsharp" && parsedJson.["target"].ToObject<string>().ToLower() <> "shared") then
                Error "The 'target' configuration element must be either 'fable' (default), 'fsharp' or 'shared'"
            elif not (isNull parsedJson.["createProjectFile"]) && parsedJson.["createProjectFile"].Type <> JTokenType.Boolean then
                Error "The 'createProjectFile' configuration element must be a boolean"
            elif not (isNull parsedJson.["copyLocalLockFileAssemblies"]) && parsedJson.["copyLocalLockFileAssemblies"].Type <> JTokenType.Boolean then
                Error "The 'copyLocalLockFileAssemblies' configuration element must be a boolean"
            elif not (isNull parsedJson.["emitMetadata"]) && parsedJson.["emitMetadata"].Type <> JTokenType.Boolean then
                Error "The 'emitMetadata' configuration element must be a boolen"
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

                    let createProjectFile =
                        if isNull parsedJson.["createProjectFile"]
                        then true
                        else parsedJson.["createProjectFile"].ToObject<bool>()

                    let fullQueriesPath =
                        if Path.IsPathRooted queriesPath
                        then queriesPath
                        else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, queriesPath))

                    let fullOutputPath =
                        if Path.IsPathRooted outputPath
                        then outputPath
                        else Path.GetFullPath(Path.Combine(Directory.GetParent(path).FullName, outputPath))

                    let overrideClientName =
                        if isNull parsedJson.["overrideClientName"]
                        then None
                        else Some (string parsedJson.["overrideClientName"])

                    let copyLocalLockFileAssemblies =
                        if isNull parsedJson.["copyLocalLockFileAssemblies"]
                        then None
                        else Some (parsedJson.["copyLocalLockFileAssemblies"].ToObject<bool>())

                    let emitMetadata =
                        if isNull parsedJson.["emitMetadata"]
                        then false
                        else parsedJson.["emitMetadata"].ToObject<bool>()

                    Ok {
                        schema = string parsedJson.["schema"]
                        queries = fullQueriesPath
                        project = string parsedJson.["project"]
                        output = fullOutputPath
                        errorType = errorType
                        target = target
                        createProjectFile = createProjectFile
                        overrideClientName = overrideClientName
                        copyLocalLockFileAssemblies = copyLocalLockFileAssemblies
                        emitMetadata = emitMetadata
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
            | QueryError.UnknownSubType(interfaceOrUnion, subType, parentField) ->
                colorprintfn "   Unknown subtype or union case $yellow[%s] was used as field of $blue[%s] of type $green[%s]" subType parentField interfaceOrUnion
            | QueryError.MissingTypeNameField(interfaceOrUnion, subType, parentField) ->
                colorprintfn "   Missing required $yellow[__typename] on sub type $blue[%s] of type $green[%s] selected on field $green[%s]" subType interfaceOrUnion parentField
            | QueryError.MissingTypeNameOnInterface(interfaceName, parentField) ->
                colorprintfn "   Missing required $yellow[__typename] on interface $green[%s] from selection $blue[%s]" interfaceName parentField
            | QueryError.InvalidInlineFragment(typeName, invalidFragment, parentField) ->
                colorprintfn "   Field $blue[%s] uses inline fragment $yellow[%s] which isn't allowed on type $green[%s]" parentField invalidFragment typeName
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

let generate (config: Config) =
    colorprintfn "⏳ Loading GraphQL schema from $green[%s]" config.schema
    match Introspection.loadSchema config.schema with
    | Error errorMessage ->
        colorprintfn "$red[%s]" errorMessage
        Error 1
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
            Error 2
        else

        let write (path: string) (contents: string) (originalQuery: string option) =
            if config.emitMetadata then
                let now = DateTime.Now.ToString("dd-MM-yyyy", CultureInfo.InvariantCulture)
                let builder = StringBuilder()
                let fileName =
                    originalQuery
                    |> Option.map (fun name -> $"from input query {Path.GetFileName(name)}")
                    |> Option.defaultValue ""

                let contentsWithMetadata =
                    builder
                        .AppendLine($"// Auto generated by Snowflaqe v{projectVersion} on {now} {fileName}")
                        .AppendLine($"// Learn more about Snowflaqe at https://github.com/Zaid-Ajaj/Snowflaqe")
                        .Append(contents)
                        .ToString()

                File.WriteAllText(path, contentsWithMetadata)
            else
                File.WriteAllText(path, contents)

        let generatedFiles = ResizeArray<string>()
        let generatedModules = ResizeArray<string * string * bool>()

        let globalTypes = [
            yield! CodeGen.createGlobalTypes schema
            yield config.errorType.typeDefinition
        ]

        let typesFileName = "Types.fs"
        let globalTypesModule = CodeGen.createNamespace [ config.project ] globalTypes
        let file = CodeGen.createFile typesFileName [ globalTypesModule ]
        let globalTypesContent = CodeGen.formatAst file typesFileName
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

        let projPath =
            if config.createProjectFile
            then Path.GetFullPath(Path.Combine(config.output, config.project + ".fsproj"))
            else Path.GetFullPath(Path.Combine(config.output, config.project + ".props"))

        let fileName file =
            if config.project.Contains "."
            then CodeGen.normalizeModuleName  file
            else config.project + "." + CodeGen.normalizeModuleName file

        let inline createCompileXElement createProjectFile outputDirectoryName (file: string) =
            if createProjectFile
            then MSBuildXElement.Compile($".\{Path.GetFileName file}")
            else MSBuildXElement.Compile($".\{outputDirectoryName}\{Path.GetFileName file}")

        match config.target with
        | OutputTarget.FSharp ->
            let stringEnumAttrPath = Path.GetFullPath(Path.Combine(config.output, fileName "StringEnum.fs"));
            colorprintfn "✏️  Generating StringEnum attribute $green[%s]" stringEnumAttrPath
            write stringEnumAttrPath (CodeGen.createDummyStringEnumAttribute()) None
            generatedFiles.Add(stringEnumAttrPath)

            let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, fileName "Types.fs"));
            colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
            write globalTypesPath globalTypesContent None
            generatedFiles.Add(globalTypesPath)

        | OutputTarget.Fable ->
            let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, fileName "Types.fs"));
            colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
            write globalTypesPath globalTypesContent None
            generatedFiles.Add(globalTypesPath)

        | OutputTarget.Shared ->
            let stringEnumAttrPath = Path.GetFullPath(Path.Combine(config.output, "shared", fileName "StringEnum.fs"));
            colorprintfn "✏️  Generating StringEnum attribute $green[%s]" stringEnumAttrPath
            write stringEnumAttrPath (CodeGen.createDummyStringEnumAttribute()) None
            generatedFiles.Add(stringEnumAttrPath)
            let globalTypesPath = Path.GetFullPath(Path.Combine(config.output, "shared", fileName "Types.fs"));
            colorprintfn "✏️  Generating module $green[%s]" globalTypesPath
            write globalTypesPath globalTypesContent None
            generatedFiles.Add(globalTypesPath)

        for queryFile in queryFiles do
            let query = File.ReadAllText queryFile
            match Query.parse query with
            | Error _ -> ()
            | Ok query ->
                let queryName = Query.findOperationName query
                let queryFileName = Path.GetFileNameWithoutExtension queryFile
                let moduleName =
                    queryName
                    |> Option.defaultValue queryFileName
                    |> CodeGen.normalizeModuleName
                let queryTypes = CodeGen.generateTypes "Query" config.errorType.typeName query schema
                let generatedModule = CodeGen.createQualifiedModule [ config.project; moduleName ] queryTypes
                let generatedModuleContent = CodeGen.formatAst (CodeGen.createFile moduleName [ generatedModule ]) (Path.GetFileName queryFile)
                match config.target with
                | OutputTarget.Fable
                | OutputTarget.FSharp ->
                    let fullPath = Path.GetFullPath(Path.Combine(config.output, fileName (moduleName + ".fs")))
                    colorprintfn "✏️  Generating module $green[%s]" fullPath
                    write fullPath generatedModuleContent (Some queryFile)
                    generatedFiles.Add(fullPath)
                    generatedModules.Add(queryFile, moduleName, generatedModuleContent.Contains "type InputVariables")

                | OutputTarget.Shared ->
                    let fullPath = Path.GetFullPath(Path.Combine(config.output, "shared", fileName (moduleName + ".fs")))
                    colorprintfn "✏️  Generating module $green[%s]" fullPath
                    write fullPath generatedModuleContent (Some queryFile)
                    generatedFiles.Add(fullPath)
                    generatedModules.Add(queryFile, moduleName, generatedModuleContent.Contains "type InputVariables")

        let clientName =
            config.overrideClientName
            |> Option.defaultValue (
                if config.project.Contains "."
                then "GraphqlClient"
                else sprintf "%sGraphqlClient" config.project
            )

        let packageReferences = [ MSBuildXElement.PackageReferenceUpdate("FSharp.Core", "5.0.1") ]

        let outputDirectoryName = DirectoryInfo(config.output).Name

        match config.target with
        | OutputTarget.Fable ->
            let graphqlClientPath = Path.GetFullPath(Path.Combine(config.output, fileName "GraphqlClient.fs"))
            colorprintfn "✏️  Generating GraphQL client $green[%s]" graphqlClientPath
            generatedFiles.Add(graphqlClientPath)
            let members =
                generatedModules
                |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleClientMember (File.ReadAllText(path)) name hasVars)
                |> String.concat "\n"
            let clientContent = CodeGen.sampleGraphqlClient config.project clientName config.errorType.typeName members
            write graphqlClientPath clientContent None
            colorprintfn "✏️  Generating Fable $green[%s]" projPath
            let files =
                generatedFiles
                |> Seq.map
                    (fun file -> createCompileXElement config.createProjectFile outputDirectoryName file)
            let packageReferences =
                [ yield! packageReferences
                  yield MSBuildXElement.PackageReferenceInclude("Fable.SimpleHttp", "3.0.0")
                  yield MSBuildXElement.PackageReferenceInclude("Fable.SimpleJson", "3.19.0") ]

            let contentItems = [
                XElement.ofStringName("Content",
                    XAttribute.ofStringName("Include", "*.fs; *.js"),
                    XAttribute.ofStringName("Exclude", "**\*.fs.js"),
                    XAttribute.ofStringName("PackagePath", "fable\\"))
            ]

            let generator =
                if config.createProjectFile
                then CodeGen.generateProjectDocument
                else CodeGen.generatePropsDocument

            let document =
                generator
                    { NugetPackageReferences = packageReferences
                      Files = files
                      CopyLocalLockFileAssemblies = None
                      ContentItems = contentItems
                      ProjectReferences = Seq.empty }
            document.WriteTo(projPath)
            files

        | OutputTarget.FSharp ->
            colorprintfn "✏️  Generating F# $green[%s]" projPath

            let members =
                generatedModules
                |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleFSharpClientMember (File.ReadAllText(path)) name hasVars)
                |> String.concat "\n"

            let graphqlClientPath = Path.GetFullPath(Path.Combine(config.output, fileName "GraphqlClient.fs"))
            generatedFiles.Add(graphqlClientPath)
            let clientContent = CodeGen.sampleFSharpGraphqlClient config.project clientName config.errorType.typeName members
            write graphqlClientPath clientContent None

            let files =
                generatedFiles
                |> Seq.map
                    (fun file -> createCompileXElement config.createProjectFile outputDirectoryName file)
            let packageReferences =
                [ yield! packageReferences
                  yield MSBuildXElement.PackageReferenceInclude("Fable.Remoting.Json", "2.14.0") ]
            let generator =
                if config.createProjectFile
                then CodeGen.generateProjectDocument
                else CodeGen.generatePropsDocument
            let document =
                generator
                    { NugetPackageReferences = packageReferences
                      Files = files
                      CopyLocalLockFileAssemblies = None
                      ContentItems = Seq.empty
                      ProjectReferences = Seq.empty }
            document.WriteTo(projPath)
            files

        | OutputTarget.Shared ->
            let files =
                generatedFiles
                |> Seq.map
                    (fun file ->
                        if config.createProjectFile
                        then MSBuildXElement.Compile(Path.GetFileName file)
                        else MSBuildXElement.Compile($".\{outputDirectoryName}\shared\{Path.GetFileName file}"))
            let sharedDocPath =
                if config.createProjectFile
                then Path.GetFullPath(Path.Combine(config.output, "shared", config.project + ".Shared.fsproj"))
                else Path.GetFullPath(Path.Combine(config.output, "shared", config.project + ".props"))
            colorprintfn "✏️  Generating shared F# $green[%s]" sharedDocPath
            let generator =
                if config.createProjectFile
                then CodeGen.generateProjectDocument
                else CodeGen.generatePropsDocument
            let document =
                generator
                    { NugetPackageReferences = packageReferences
                      Files = files
                      CopyLocalLockFileAssemblies = None
                      ContentItems = Seq.empty
                      ProjectReferences = Seq.empty }
            File.WriteAllText(sharedDocPath, document.ToString())

            let fsharpGraphqlClientPath = Path.GetFullPath(Path.Combine(config.output, "dotnet", fileName "GraphqlClient.fs"))
            let fsharpMembers =
                generatedModules
                |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleFSharpClientMember (File.ReadAllText(path)) name hasVars)
                |> String.concat "\n"
            let dotnetClientContent = CodeGen.sampleFSharpGraphqlClient config.project clientName config.errorType.typeName fsharpMembers
            write fsharpGraphqlClientPath dotnetClientContent None
            let sharedFSharpDocument =
                if config.createProjectFile
                then Path.GetFullPath(Path.Combine(config.output, "dotnet", config.project + ".Dotnet.fsproj"))
                else Path.GetFullPath(Path.Combine(config.output, "dotnet", config.project + ".props"))
            colorprintfn "✏️  Generating F# dotnet $green[%s]" sharedFSharpDocument
            let packageReferences =
                [ yield! packageReferences
                  yield MSBuildXElement.PackageReferenceInclude("Fable.Remoting.Json", "2.14.0") ]
            let projectReferences =
                if config.createProjectFile
                then MSBuildXElement.ProjectReference($"..\shared\{config.project}.Shared.fsproj")
                else MSBuildXElement.ProjectReference($".\{outputDirectoryName}\\shared\{config.project}.props")
                |> Seq.singleton
            let files =
                if config.createProjectFile
                then MSBuildXElement.Compile($".\{config.project}.GraphqlClient.fs")
                else MSBuildXElement.Compile($".\{outputDirectoryName}\\dotnet\{config.project}.GraphqlClient.fs")
                |> Seq.singleton
            let generator =
                if config.createProjectFile
                then CodeGen.generateProjectDocument
                else CodeGen.generatePropsDocument
            let document =
                generator
                    { NugetPackageReferences = packageReferences
                      Files = files
                      CopyLocalLockFileAssemblies = config.copyLocalLockFileAssemblies
                      ContentItems = Seq.empty
                      ProjectReferences = projectReferences }
            document.WriteTo(sharedFSharpDocument)

            let fableMembers =
                generatedModules
                |> Seq.map (fun (path, name, hasVars) -> CodeGen.sampleClientMember (File.ReadAllText(path)) name hasVars)
                |> String.concat "\n"

            let fableGraphqlClientPath = Path.GetFullPath(Path.Combine(config.output, "fable", fileName "GraphqlClient.fs"))
            let fableClientContent = CodeGen.sampleGraphqlClient config.project clientName config.errorType.typeName fableMembers
            write fableGraphqlClientPath fableClientContent None
            let sharedFableDocument =
                if config.createProjectFile
                then Path.GetFullPath(Path.Combine(config.output, "fable", config.project + ".Fable.fsproj"))
                else Path.GetFullPath(Path.Combine(config.output, "fable", config.project + ".props"))
            colorprintfn "✏️  Generating Fable $green[%s]" sharedFableDocument
            let packageReferences = [
                    MSBuildXElement.PackageReferenceUpdate("FSharp.Core", "5.0.1")
                    MSBuildXElement.PackageReferenceInclude("Fable.SimpleHttp", "3.0.0")
                    MSBuildXElement.PackageReferenceInclude("Fable.SimpleJson", "3.19.0")
                ]
            let files =
                if config.createProjectFile
                then MSBuildXElement.Compile($"{config.project}.GraphqlClient.fs")
                else MSBuildXElement.Compile($".\{outputDirectoryName}\\fable\{config.project}.GraphqlClient.fs")
                |> Seq.singleton
            let contentItems =
                XElement.ofStringName("Content",
                    XAttribute.ofStringName("Include", "*.fs; *.js"),
                    XAttribute.ofStringName("Exclude", "**\*.fs.js"),
                    XAttribute.ofStringName("PackagePath", "fable\\"))
                |> Seq.singleton
            let projectReferences =
                if config.createProjectFile
                then MSBuildXElement.ProjectReference($"..\shared\{config.project}.Shared.fsproj")
                else MSBuildXElement.ProjectReference($".\{outputDirectoryName}\\shared\{config.project}.props")
                |> Seq.singleton
            let generator =
                if config.createProjectFile
                then CodeGen.generateProjectDocument
                else CodeGen.generatePropsDocument
            let document =
                generator
                    { NugetPackageReferences = packageReferences
                      Files = files
                      CopyLocalLockFileAssemblies = None
                      ContentItems = contentItems
                      ProjectReferences = projectReferences }
            document.WriteTo(sharedFableDocument)
            Seq.empty
        |> Seq.map (fun xel -> xel.Attribute(XName.Get("Include")).Value)
        |> Ok

[<EntryPoint>]
let main argv =
    if argv = [| "--version" |] then
        printfn "%s" projectVersion
        0
    else
    Console.OutputEncoding <- Encoding.UTF8
    Console.WriteLine(logo)

    let generateOrExit configFile =
        match readConfig configFile with
        | Error errorMessage ->
            colorprintfn "$red[%s]" errorMessage
            1
        | Ok config -> generate config |> ignore; 0

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
        let config = {
            schema = schema;
            queries = queries;
            project = "GraphqlClient";
            output = "./output";
            errorType = { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() };
            target = OutputTarget.Fable; overrideClientName = None
            createProjectFile = true
            copyLocalLockFileAssemblies = None
            emitMetadata = false
        }

        runConfig config

    | [| "--schema"; schema; "--queries"; queries |] ->
        let config = {
            schema = schema;
            queries = queries;
            project = "GraphqlClient";
            output = "./output";
            errorType = { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() };
            target = OutputTarget.Fable; overrideClientName = None
            createProjectFile = true
            copyLocalLockFileAssemblies = None
            emitMetadata = false
        }

        runConfig config

    | [| "--generate" |] ->
        Directory.GetFiles(Environment.CurrentDirectory)
        |> Seq.tryFind (fun file -> file.ToLower().EndsWith("snowflaqe.json"))
        |> function
            | None ->
                colorprintfn "⚠️  No configuration file found. Expecting JSON file $yellow[%s] in the current working directory" "snowflaqe.json"
                1
            | Some configFile -> generateOrExit configFile

    | [| "--config"; configFile; "--generate" |] ->
        generateOrExit configFile
    | [| "--generate"; "--config"; configFile |] ->
        generateOrExit configFile
    | _ ->
        printfn "The combination of arguments was not recognized: %A" (List.ofArray argv)
        0
