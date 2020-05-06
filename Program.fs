open System
open FSharp.Data.LiteralProviders
open System.Net.Http
open System.Net
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.IO
open GraphQLParser
open GraphQLParser.AST

module Types =
    [<RequireQualifiedAccess>]
    type GraphqlScalar =
        | Int
        | String
        | Float
        | Bool
        | Custom of name:string

    type GraphqlEnumValue =  {
        name : string
        description : string option
    }

    type GraphqlEnum = {
        name : string
        description : string option
        values : GraphqlEnumValue list
    }

    [<RequireQualifiedAccess>]
    type GraphqlFieldType =
        | Scalar of fieldType:GraphqlScalar
        | ObjectRef of referencedObject:string
        | NonNull of GraphqlFieldType
        | List of GraphqlFieldType

    type GraphqlField = {
        fieldName : string
        fieldType : GraphqlFieldType
        args :  (string * GraphqlFieldType) list
    }

    type GraphqlObject = {
        name: string
        description: string option
        fields : GraphqlField  list
    }

    [<RequireQualifiedAccess>]
    type GraphqlType =
        | Scalar of GraphqlScalar
        | Object of GraphqlObject
        | Enum of GraphqlEnum

    type GraphqlSchema = { types : GraphqlType list }

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


open Types

let httpClient = new HttpClient()

let [<Literal>] introspectionQuery = TextFile<"Introspection.gql">.Text

module Patterns =
    open Types

    let (|Scalar|_|) (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "SCALAR" ->
            match typeJson.["name"].ToString() with
            | "Int" -> Some (Scalar (GraphqlScalar.Int))
            | "String" -> Some (Scalar GraphqlScalar.String)
            | ("Bool"|"Boolean") -> Some (Scalar GraphqlScalar.Bool)
            | "Float" -> Some (Scalar GraphqlScalar.Float)
            | customScalar -> Some (Scalar (GraphqlScalar.Custom customScalar))
        | _ -> None

    let (|Enum|_|) (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "ENUM" ->
            let name = typeJson.["name"].ToString()
            let description = Option.ofObj typeJson.["description"] |> Option.map string |> Option.filter (not << String.IsNullOrWhiteSpace)
            let values : GraphqlEnumValue list =  [
                for enumValue in typeJson.["enumValues"] ->
                    {
                        name = enumValue.["name"].ToString()
                        description = Option.ofObj enumValue.["description"] |> Option.map string |> Option.filter (not << String.IsNullOrWhiteSpace)
                    }
            ]

            Some { name = name; description = description; values = values  }
        | _ ->
            None

    let (|ObjectRef|_|) (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "OBJECT" when isNull typeJson.["fields"] ->
            let typeName = typeJson.["name"].ToString()
            Some typeName
        | _ ->
            None

    let (|NonNull|_|) (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "NON_NULL" -> Some typeJson.["ofType"]
        | _ -> None

    let (|List|_|) (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "LIST" -> Some (typeJson.["ofType"])
        | _ -> None

    let rec tryParseFieldType (fieldJson: JToken)  =
        match fieldJson with
        | Scalar scalar -> Some (GraphqlFieldType.Scalar scalar)
        | ObjectRef referencedObject -> Some (GraphqlFieldType.ObjectRef  referencedObject)
        | NonNull innerType ->
            match tryParseFieldType innerType with
            | Some field -> Some (GraphqlFieldType.NonNull field)
            | None -> None

        | List innerType ->
            match tryParseFieldType innerType with
            | Some field -> Some (GraphqlFieldType.List field)
            | None -> None

        | otherwise -> None

    let (|Object|_|)  (typeJson: JToken) =
        match typeJson.["kind"].ToString() with
        | "OBJECT" ->
            let name = typeJson.["name"].ToString()
            let description = Option.ofObj typeJson.["description"] |> Option.map string |> Option.filter String.IsNullOrWhiteSpace
            let fields = unbox<JArray> typeJson.["fields"]
            let graphqlFields =
                fields
                |> List.ofSeq
                |> List.choose (fun field ->
                    let fieldName = field.["name"].ToString()
                    let parsedFieldType = tryParseFieldType field.["type"]
                    let args = List.choose id [
                        for arg in unbox<JArray> field.["args"] do
                            let argName = arg.["name"].ToString()
                            match tryParseFieldType arg.["type"] with
                            | Some argType -> Some (argName, argType)
                            | None -> None
                    ]

                    match parsedFieldType with
                    | Some fieldType -> Some  { fieldName = fieldName; fieldType = fieldType; args = args }
                    | None -> None
                )

            Some {
                name = name
                description = description
                fields = graphqlFields
            }

        | _ ->
            None

let validateQuery (query: Ast.GraphqlQuery) (schema: GraphqlSchema) =
    true

let parseSchema (content: string) : Types.GraphqlSchema =
    let contentJson = JToken.Parse(content)
    let typesJson = unbox<JArray> contentJson.["data"].["__schema"].["types"]
    let graphqlTypes = [
        for typeJson in typesJson do
            match typeJson with
            | Patterns.Scalar scalar -> Some (GraphqlType.Scalar scalar)
            | Patterns.Object object -> Some (GraphqlType.Object object)
            | Patterns.Enum enum -> Some (GraphqlType.Enum enum)
            | _ -> None
    ]

    { types = graphqlTypes |> List.choose id }

let lexer = Lexer()
let parser = Parser(lexer)

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--config"; configFile |] ->
        let configFilePath =
            if Path.IsPathRooted configFile
            then configFile
            else Path.GetFullPath(Path.Combine(Environment.CurrentDirectory, configFile))

        if not (File.Exists configFilePath) then
            failwithf "File %s was not found" configFilePath
        else

            let configJson = JToken.Parse(File.ReadAllText configFilePath)
            let schemaUrl = string configJson.["schema"]
            let queriesPath = string configJson.["queries"]

            let fullQueriesPath =
                if Path.IsPathRooted queriesPath
                then queriesPath
                else Path.GetFullPath(Path.Combine(Directory.GetParent(configFilePath).FullName, queriesPath))

            let requestBody = JObject()
            requestBody.Add(JProperty("query", introspectionQuery))

            let responseAsync = httpClient.PostAsync(schemaUrl, new StringContent(requestBody.ToString(), Encoding.UTF8, "application/json"))
            let reponse = Async.RunSynchronously (Async.AwaitTask responseAsync)
            let content = Async.RunSynchronously (Async.AwaitTask (reponse.Content.ReadAsStringAsync()))
            let schema = parseSchema content


            let queryFiles = Directory.GetFiles(fullQueriesPath, "*.gql")

            for queryFile in queryFiles do
                let query = File.ReadAllText queryFile
                let ast = parser.Parse(Source(query))
                let queryAst = Ast.fromDocument ast

                ()


            printfn "Success!"
            0 // return an integer exit code

    | _ ->
        failwith "No config provided"
