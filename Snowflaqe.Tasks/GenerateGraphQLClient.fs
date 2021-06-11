namespace Snowflaqe.Tasks

open System
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open Program
open Snowflaqe
open Snowflaqe.Types
open System.IO

type public GenerateGraphQLClient() =
    inherit Task()

    [<Required>]
    member val public Platform : string = String.Empty with get, set

    [<Required>]
    member val public Configuration : string = String.Empty with get, set

    member val public EmitMetadata = false with get, set

    member val public ErrorType =
        { typeName = "ErrorType"; typeDefinition = CodeGen.defaultErrorType() } with get, set

    member val public OutputPath : string = String.Empty with get, set

    member val public Project = "GraphqlClient" with get, set

    member val public Queries = "" with get, set

    member val public Schema = "" with get, set

    member val public Target = "fable" with get, set

    [<Output>]
    member val public GeneratedFiles = Array.empty<string> with get, set

    static member private TryParseTarget =
        function
        | "fable" -> Some OutputTarget.Fable
        | "fsharp" -> Some OutputTarget.FSharp
        | "shared" -> Some OutputTarget.Shared
        | _ -> None

    override this.Execute() =
        System.Diagnostics.Debugger.Launch() |> ignore
        let config =
            { schema = this.Schema
              queries = this.Queries
              project = this.Project
              output =
                if String.IsNullOrEmpty(this.OutputPath) then
                    Path.Combine("obj", this.Configuration, this.Platform, "Snowflaqe")
                else this.OutputPath
              errorType = this.ErrorType
              target = (GenerateGraphQLClient.TryParseTarget (this.Target.ToLower())) |> Option.defaultValue OutputTarget.Fable
              createProjectFile = false
              overrideClientName = None
              copyLocalLockFileAssemblies = None
              emitMetadata = this.EmitMetadata }
        let validationCode = runConfig config
        let executionCode =
            if validationCode = 0 then
                match generate config with
                | Error code -> code
                | Ok files ->
                    this.GeneratedFiles <-
                        files |> Seq.map (fun f -> Path.Combine(config.output, Path.GetFileName(f))) |> Seq.toArray
                    0
            else validationCode
        executionCode = 0
