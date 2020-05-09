module Snowflake.Schema

open System
open Newtonsoft.Json.Linq
open Snowflake.Types

let stringOrNone (json: JToken) (key: string) = 
    if json.Type = JTokenType.Object then 
        let dict = unbox<JObject> json 
        if dict.ContainsKey key && not (String.IsNullOrWhiteSpace (string dict.[key]))
        then Some (string dict.[key])
        else None 
    else None

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
        let description = stringOrNone typeJson "description"
        let values = [
            for enumValue in typeJson.["enumValues"] ->
                {
                    name = string enumValue.["name"]
                    description = stringOrNone enumValue "description" 
                }
        ]

        Some { name = name; description = description; values = values  }
    | _ ->
        None

let (|ObjectRef|_|) (typeJson: JToken) =
    match typeJson.["kind"].ToString() with
    | "OBJECT" ->
        let typeName = typeJson.["name"].ToString()
        Some typeName
    | _ ->
        None

let (|EnumRef|_|) (typeJson: JToken) =
    match typeJson.["kind"].ToString() with
    | "ENUM"  ->
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
    | ObjectRef referencedObject -> Some (GraphqlFieldType.ObjectRef referencedObject)
    | EnumRef enumRef -> Some (GraphqlFieldType.EnumRef enumRef)
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
        let description = stringOrNone typeJson "description"
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

let parse (content: string) =
    try 
        let contentJson = JToken.Parse(content)
        let graphqlTypes = [
            for typeJson in contentJson.["data"].["__schema"].["types"] ->
                match typeJson with
                | Scalar scalar -> Some (GraphqlType.Scalar scalar)
                | Object object -> Some (GraphqlType.Object object)
                | Enum enum -> Some (GraphqlType.Enum enum)
                | _ -> None
        ]

        let query = 
            if isNull contentJson.["data"].["__schema"].["queryType"] 
            then None 
            else stringOrNone contentJson.["data"].["__schema"].["queryType"] "name"

        let mutation = 
            if isNull contentJson.["data"].["__schema"].["mutationType"] 
            then None
            else stringOrNone contentJson.["data"].["__schema"].["mutationType"] "name"

        Ok { 
            types = graphqlTypes |> List.choose id
            queryType = query
            mutationType = mutation
        }
    
    with 
    | ex -> Error ex.Message