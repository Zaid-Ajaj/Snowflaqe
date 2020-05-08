open Expecto
open Snowflake
open Snowflake.Types

let queryParsing =
    testList "Query parsing" [

        test "Simple queries can be parsed" {
            let query = Query.parse """
                query {
                    artists {
                        name
                        bio
                    }
                }
            """

            match query with
            | Ok (GraphqlDocument.Query parsedQuery) ->
                Expect.equal parsedQuery.name None "This query has no name defined"
            | Ok (_) ->
                failwith "Unexpected results"
            | Error error ->
                failwith error
        }

        test "Named queries can be parsed" {
            let query = Query.parse """
                query getArtists {
                    artists {
                        name
                        bio
                    }
                }
            """

            match query with
            | Ok (GraphqlDocument.Query parsedQuery) ->
                Expect.equal parsedQuery.name (Some "getArtists") "Query name can be extracted"
            | Ok (_) ->
                failwith "Unexpected results"
            | Error error ->
                failwith error
        }

        test "Schema can be parsed from GraphQL definition" {
            let schema = Introspection.loadSchema """
                type Query {
                    composites: [Composite!]!
                }

                type Composite {
                    id: String!
                    values: [String!]!
                }
            """

            Expect.isOk schema "Schema can be parsed"
        }

        test "Query.findTypeByName works" {
            let schema = Introspection.loadSchema """
                type Query {
                    hello: String
                }
            """

            match schema with
            | Error error -> failwith error
            | Ok schema ->
                match Query.findTypeByName "Query" schema with
                | Some (GraphqlType.Object objectDef) ->
                    Expect.equal "Query" objectDef.name "Query can queries"
                    Expect.equal 1 objectDef.fields.Length "Query only has one field"
                | otherwise ->
                    failwithf "Unexpected %A" otherwise
        }

        test "Query can be validated against schema" {
            let schema = Introspection.loadSchema """
                type Query {
                    hello: String
                }
            """

            let query = Query.parse """
                query {
                    hello
                }
            """

            Expect.isOk schema "Schema can be parsed"
            Expect.isOk query "Query can be parsed"

            match query, schema with
            | Ok query, Ok schema ->
                let valid = Query.validate query schema
                Expect.isFalse valid "Query should be valid"

            | _ ->
                failwith "Unexpected result"
        }
]


let snowflakeTests = testList "Snowflake" [ queryParsing ]

[<EntryPoint>]
let main argv = runTests defaultConfig snowflakeTests