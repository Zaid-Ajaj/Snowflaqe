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
            | Ok document ->
                Expect.equal 1 document.nodes.Length "Document has one element"
                match document.nodes.[0] with 
                | GraphqlNode.Query parsedQuery -> 
                    Expect.equal parsedQuery.name None "Query name can be extracted"
                | _ ->
                    failwith "Unexpected node"
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
            | Ok document ->
                Expect.equal 1 document.nodes.Length "Document has one element"
                match document.nodes.[0] with 
                | GraphqlNode.Query parsedQuery -> 
                    Expect.equal parsedQuery.name (Some "getArtists") "Query name can be extracted"
                | _ ->
                    failwith "Unexpected node"
            | Error error ->
                failwith error
        }

        test "Schema can be parsed from GraphQL definition" {
            let schema = Introspection.fromSchemaDefinition """
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

        test "Query.findQueryType works" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    hello: String
                }
            """

            match schema with
            | Error error -> failwith error
            | Ok schema ->
                match Schema.findQuery schema with
                | Some queryType ->
                    Expect.equal "Query" queryType.name "Query can queries"
                    Expect.equal 1 queryType.fields.Length "Query only has one field"
                    Expect.equal "hello" queryType.fields.[0].fieldName "Field name is read correctly"
                    Expect.isEmpty queryType.fields.[0].args "Field has no arguments"
                    Expect.equal (GraphqlFieldType.Scalar(GraphqlScalar.String)) queryType.fields.[0].fieldType "Field type is correct"
                | otherwise ->
                    failwithf "Unexpected %A" otherwise
        }

        test "Query with fragments can be parsed" {
            let query = Query.parse """
                fragment NameParts on Person {
                  firstName
                  lastName
                }

                query GetPerson {
                  people {
                    ...NameParts
                    phone
                  }
                }
            """

            match query with 
            | Ok document -> Expect.equal 2 document.nodes.Length "There are two nodes: query and fragment definition"
            | Error error -> failwith error
        }

        test "Query with fragments can be expanded" {
            let queryAst = Query.parse """
                fragment NameParts on Person {
                  firstName
                  lastName
                }

                query GetPerson {
                  people {
                    ...NameParts
                    phone
                  }
                }
            """

            match queryAst with 
            | Ok document -> 
                let modifiedDocument = Query.expandDocumentFragments document
                match Query.findOperation modifiedDocument with 
                | Some (GraphqlOperation.Query query) -> 
                    let people = query.selectionSet.nodes.[0]
                    match people with 
                    | GraphqlNode.Field { name = "people"; selectionSet = Some { nodes = nodes } } -> 
                        Expect.equal 3 nodes.Length "people have selectionSet of three nodes"
                    | otherResults -> failwithf "Unexpected %A" otherResults  
                | otherResults -> failwithf "Unexpected %A" otherResults 
            | Error error -> failwith error
        }

        test "Simple query can be validated against schema" {
            let schema = Introspection.fromSchemaDefinition """
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
                let validationResult = Query.validate query schema
                Expect.equal ValidationResult.Success validationResult "Query should be valid"

            | otherResults -> failwithf "Unexpected %A" otherResults 
        }

        test "Query validation: unknown fields can be detected" {
            let schema = Introspection.fromSchemaDefinition """
            type Query {
                hello: String
            }
            """

            let query = Query.parse """
            query {
                hello
                whatsUp
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let validationResult = Query.validate query schema
                match validationResult with 
                | ValidationResult.FieldValidation [ FieldValidationError.UnknownField (unkownField, queryType) ] ->
                    Expect.equal unkownField "whatsUp" "Unknown field is detected properly"
                    Expect.equal queryType "Query" "Type is propagted"

                | otherResults -> failwithf "Unexpected %A" otherResults 

            | otherResults -> failwithf "Unexpected %A" otherResults 
        }

        test "Query validation: not allowed to expland non-object fields" {
            let schema = Introspection.fromSchemaDefinition """
            type Query {
                hello: String
            }
            """

            let query = Query.parse """
            query {
                hello {
                    whatsUp
                }
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let validationResult = Query.validate query schema
                match validationResult with 
                | ValidationResult.FieldValidation [ FieldValidationError.ExpandedScalarField (scalarField, queryType) ] ->
                    Expect.equal scalarField "hello" "Scalar field cannot be expanded"
                    Expect.equal queryType "Query" "Type is propagted"

                | otherResults -> failwithf "Unexpected %A" otherResults 

            | otherResults -> failwithf "Unexpected %A" otherResults 
         }


        test "Query validation: detect unknown fields within nested selections" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    composites: [Composite!]!
                }

                type Composite {
                    id: String!
                    values: [String!]!
                }
            """

            let query = Query.parse """
            query {
                composites {
                    whatever
                }
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let validationResult = Query.validate query schema
                match validationResult with 
                | ValidationResult.FieldValidation [ FieldValidationError.UnknownField (scalarField, queryType) ] ->
                    Expect.equal scalarField "whatever" "Scalar field cannot be expanded"
                    Expect.equal queryType "Composite" "Type is propagted"

                | otherResults -> failwithf "Unexpected %A" otherResults 

            | otherResults -> failwithf "Unexpected %A" otherResults 
         }

        test "Query validation: not allowed to expland non-object fields in nested selection fields" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    composites: [Composite!]!
                }

                type Composite {
                    id: String!
                    values: [String!]!
                }
            """

            let query = Query.parse """
            query {
                composites {
                    values {
                        hello
                    }
                }
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let validationResult = Query.validate query schema
                match validationResult with 
                | ValidationResult.FieldValidation [ FieldValidationError.ExpandedScalarField (scalarField, queryType) ] ->
                    Expect.equal scalarField "values" "Scalar field cannot be expanded"
                    Expect.equal queryType "Composite" "Type is propagted"

                | otherResults -> failwithf "Unexpected %A" otherResults 

            | otherResults -> failwithf "Unexpected %A" otherResults 
         }
]


let snowflakeTests = testList "Snowflake" [ queryParsing ]

[<EntryPoint>]
let main argv = runTests defaultConfig snowflakeTests