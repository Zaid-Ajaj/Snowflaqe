open System
open Newtonsoft.Json.Linq
open System.IO
open Snowflaqe

type Config = {
    schema: string
    queries: string
}

type CommandLineArgs =
    | Validate of Config
    | Generate of Config

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--config"; configFile |] ->
        let configFile = resolveFile configFile

        if not (File.Exists configFile) then
            failwithf "File %s was not found" configFile
        else
            let configJson = JToken.Parse(File.ReadAllText configFile)
            let schemaUrl = string configJson.["schema"]
            let queriesPath = string configJson.["queries"]

            let fullQueriesPath =
                if Path.IsPathRooted queriesPath
                then queriesPath
                else Path.GetFullPath(Path.Combine(Directory.GetParent(configFile).FullName, queriesPath))

            let queryFiles = Directory.GetFiles(fullQueriesPath, "*.gql")

            for queryFile in queryFiles do
                let query = File.ReadAllText queryFile
                let queryAst = Query.parse query

                ()

            let loadedSchema = Introspection.loadSchema schemaUrl

            match loadedSchema with
            | Error error -> failwith error
            | Ok schema -> printfn "%A" schema

            printfn "Success!"
            0 // return an integer exit code

    | _ ->

        failwith "No config provided"
