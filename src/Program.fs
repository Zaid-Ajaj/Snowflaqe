open System
open Newtonsoft.Json.Linq
open System.IO
open GraphQLParser
open GraphQLParser.AST
open Snowflake.Types
open Snowflake

module Ast =
    [<RequireQualifiedAccess>]
    type AstNode =
        | Name of GraphQLName
        | SelectionSet of GraphQLSelectionSet
        | Query of GraphQLOperationDefinition
        | Mutation of GraphQLOperationDefinition
        | Subscripiton of GraphQLOperationDefinition

    type GraphqlQuery = { nodes : AstNode list }

    let fromNode (node: ASTNode) : option<AstNode> =
        match node.Kind with
        | ASTNodeKind.Name -> Some (AstNode.Name (unbox<GraphQLName> node))
        | ASTNodeKind.OperationDefinition ->
            let operationDef = unbox<GraphQLOperationDefinition> node
            match operationDef.Operation with
            | OperationType.Query -> Some (AstNode.Query operationDef)
            | OperationType.Mutation ->  Some (AstNode.Mutation operationDef)
            | OperationType.Subscription -> Some (AstNode.Subscripiton operationDef)
            | _ -> None

        | ASTNodeKind.SelectionSet -> Some (AstNode.SelectionSet (unbox<GraphQLSelectionSet> node))
        | _ -> None

    let fromDocument (document: GraphQLDocument) : GraphqlQuery =
        let nodes =
            document.Definitions
            |> Seq.choose fromNode
            |> List.ofSeq

        {  nodes = nodes }

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
