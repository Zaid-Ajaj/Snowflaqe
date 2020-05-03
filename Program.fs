open System
open FSharp.Data.LiteralProviders
open System.Net.Http
open System.Net
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq

module Types =
    [<RequireQualifiedAccess>]
    type GraphqlScalar =
        | ID
        | Int
        | String
        | Float
        | Bool
        | Custom of name:string

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
        description: string
        fields : GraphqlField  list
    }

    [<RequireQualifiedAccess>]
    type GraphqlType =
        | Scalar of GraphqlScalar
        | Object of GraphqlObject

    type GraphqlSchema = { types : GraphqlType list }

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
            let description = typeJson.["description"].ToString()
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

let parseSchema (content: string) : Types.GraphqlSchema =
    let contentJson = JToken.Parse(content)
    let typesJson = unbox<JArray> contentJson.["data"].["__schema"].["types"]
    let graphqlTypes = [
        for typeJson in typesJson do
            match typeJson with
            | Patterns.Scalar scalar -> Some (GraphqlType.Scalar scalar)
            | Patterns.Object object -> Some (GraphqlType.Object object)
            | _ -> None
    ]

    { types = graphqlTypes |> List.choose id }

[<EntryPoint>]
let main argv =
    let url = "https://graphql-pokemon.now.sh"

    let requestBody = JObject()
    requestBody.Add(JProperty("query", introspectionQuery))

    let responseAsync = httpClient.PostAsync(url, new StringContent(requestBody.ToString(), Encoding.UTF8, "application/json"))
    let reponse = Async.RunSynchronously (Async.AwaitTask responseAsync)
    let content = Async.RunSynchronously (Async.AwaitTask (reponse.Content.ReadAsStringAsync()))
    let schema = parseSchema content
    printfn "%A" schema
    //Console.WriteLine content
    0 // return an integer exit code
