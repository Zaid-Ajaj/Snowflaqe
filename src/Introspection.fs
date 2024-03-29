﻿[<RequireQualifiedAccess>]
module Snowflaqe.Introspection

open System
open FSharp.Data.LiteralProviders
open System.Text
open System.Net.Http
open GraphQL.Utilities
open GraphQLParser.AST
open Newtonsoft.Json.Linq
open System.IO
open GraphQL
open GraphQL.NewtonsoftJson

let private httpClient = new HttpClient()

let [<Literal>] IntrospectionQuery = TextFile<"Introspection.gql">.Text

module CustomScalars =
    type CustomScalarGraphType() =
       inherit GraphQL.Types.StringGraphType()

    let private findScalarNames (definition: string) =
        let types = GraphQLParser.Parser.Parse(GraphQLParser.ROM(definition.AsMemory()))

        types.Definitions
        |> Seq.filter (fun def -> def :? GraphQLScalarTypeDefinition)
        |> Seq.map (fun def -> (def :?> GraphQLScalarTypeDefinition).Name.StringValue)

    let private filterBuiltInScalars (scalarsNames: string seq) =
        let builtInScalars =
            GraphQL.Types.SchemaTypes.BuiltInScalarMappings.Values
            |> Seq.map (fun scalarType -> (Activator.CreateInstance(scalarType) :?> GraphQL.Types.IGraphType))
            |> Seq.filter (fun scalarObject -> scalarObject <> null)
            |> Seq.map (fun scalarType -> scalarType.Name)
            |> Set

        Set.difference (scalarsNames |> Set) builtInScalars

    let findCustomScalarNames = findScalarNames >> filterBuiltInScalars

    let buildCustomScalarGraphTypes (scalarNames: string seq) =
        let createCustomScalar scalarName =
            let scalarType = CustomScalarGraphType()
            scalarType.Name <- scalarName
            scalarType

        scalarNames |> Seq.map createCustomScalar

let fromSchemaDefinition (definition: string) =
    try
        // graphql-dotnet will fail validation for unions and interfaces when executing a query
        // if ResolveType or IsTypeOf are not present. This is not applicable in case of introspection query
        // when no actual data is involved and there is nothing to resolve. We bypass the validation by
        // always providing a dummy ResolveType. See https://github.com/Zaid-Ajaj/Snowflaqe/issues/70 for context.
        let configureSchemaBuilder (schemaBuilder: SchemaBuilder) =
            schemaBuilder.Types.ForAll(fun typeConfig -> typeConfig.ResolveType <- fun _ -> null) |> ignore

        let configureExecutionOptions = System.Action<ExecutionOptions>(fun options ->
           options.Query <- IntrospectionQuery
           options.ThrowOnUnhandledException <- true
        )

        let graphqlServer = GraphQL.Types.Schema.For(definition, configureSchemaBuilder)

        CustomScalars.findCustomScalarNames definition
        |> CustomScalars.buildCustomScalarGraphTypes
        |> Seq.iter graphqlServer.RegisterType

        let schemaJson =
            graphqlServer.ExecuteAsync(GraphQLSerializer(), configureExecutionOptions)
            |> Async.AwaitTask
            |> Async.RunSynchronously

        Schema.parse schemaJson
    with
    | ex -> Error ex.Message

let loadSchema (schema: string) =
    // Handle schema as the URL to a GraphQL backend
    if schema.StartsWith "http://" || schema.StartsWith "https://" then
        try
            let requestBody = JObject()
            requestBody.Add(JProperty("query", IntrospectionQuery))

            let responseAsync = httpClient.PostAsync(schema, new StringContent(requestBody.ToString(), Encoding.UTF8, "application/json"))
            let response = Async.RunSynchronously (Async.AwaitTask responseAsync)
            let content = Async.RunSynchronously (Async.AwaitTask (response.Content.ReadAsStringAsync()))
            Schema.parse content
        with
        | ex ->
            let errorMsg = sprintf "Error while trying to load schema %s\n%s" schema ex.Message
            Error errorMsg

    // Handle schema input as JSON file (output from the Introspection query)
    else if schema.EndsWith ".json" && File.Exists (resolveFile schema) then
        let content = File.ReadAllText (resolveFile schema)
        Schema.parse content

    else if schema.EndsWith ".json" && not (File.Exists (resolveFile schema)) then
        let errorMsg = sprintf "File %s was not found" (resolveFile schema)
        Error errorMsg

    // Handle schema input as Graphql schema definition (schema-first approach)
    else if (schema.EndsWith ".graphql" || schema.EndsWith ".gql") && File.Exists (resolveFile schema) then
        try
            let schemaContent = File.ReadAllText (resolveFile schema)
            fromSchemaDefinition schemaContent
        with
        | ex -> Error ex.Message

    else if (schema.EndsWith ".graphql" || schema.EndsWith ".gql") && not (File.Exists (resolveFile schema)) then
        let errorMsg = sprintf "File %s was not found to load the schema from" (resolveFile schema)
        Error errorMsg
    else
        Error (sprintf "Unexpected input %s" schema)
