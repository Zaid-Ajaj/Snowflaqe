[<RequireQualifiedAccess>]
module Snowflaqe.Schema

open System
open Newtonsoft.Json.Linq
open Snowflaqe.Types

let (|Scalar|_|) (typeJson: JToken) =
    match typeJson.["kind"].ToString() with
    | "SCALAR" ->
        match typeJson.["name"].ToString() with
        | "ID" -> Some (Scalar (GraphqlScalar.ID))
        | "Int" -> Some (Scalar (GraphqlScalar.Int))
        | "String" -> Some (Scalar GraphqlScalar.String)
        | "Boolean" -> Some (Scalar GraphqlScalar.Boolean)
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

let (|InputObjectRef|_|) (typeJson: JToken) =
    match typeJson.["kind"].ToString() with
    | "INPUT_OBJECT" ->
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
    | InputObjectRef referencedObject -> Some (GraphqlFieldType.InputObjectRef referencedObject)
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

let (|InputObject|_|)  (typeJson: JToken) : GraphqlInputObject option =
    match typeJson.["kind"].ToString() with
    | "INPUT_OBJECT" ->
        let name = typeJson.["name"].ToString()
        let description = stringOrNone typeJson "description"
        let fields = unbox<JArray> typeJson.["inputFields"]
        let graphqlFields =
            fields
            |> List.ofSeq
            |> List.choose (fun field ->
                let fieldName = field.["name"].ToString()
                let parsedFieldType = tryParseFieldType field.["type"]
                match parsedFieldType with
                | Some fieldType -> Some  { fieldName = fieldName; fieldType = fieldType }
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
                | InputObject object -> Some (GraphqlType.InputObject object)
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

let findTypeByName (name: string) (schema: GraphqlSchema) =
    schema.types
    |> List.tryFind (function
        | GraphqlType.Enum enumDef -> enumDef.name = name
        | GraphqlType.Object objectDef -> objectDef.name = name
        | GraphqlType.InputObject objectDef -> objectDef.name = name
        | GraphqlType.Scalar scalar -> false)

let findQuery (schema: GraphqlSchema) = 
    match schema.queryType with 
    | None -> None 
    | Some typeName -> 
        match findTypeByName typeName schema with 
        | Some (GraphqlType.Object queryType) -> Some queryType 
        | _ -> None

let findMutation (schema: GraphqlSchema) = 
    match schema.mutationType with 
    | None -> None 
    | Some typeName -> 
        match findTypeByName typeName schema with 
        | Some (GraphqlType.Object queryType) -> Some queryType 
        | _ -> None