open Expecto
open Snowflaqe
open Snowflaqe.Types

let [<Literal>] typesFileName = "Types.fs"

let trimContentEnd (content: string) = Utilities.trimContentEnd content

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

        test "Queries with aliases can be parsed" {
            let query = Query.parse """
                query getArtists {
                    artists {
                        artistName: name
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
                | ValidationResult.QueryErrors [ QueryError.UnknownField (unkownField, parentSelection, queryType) ] ->
                    Expect.equal unkownField "whatsUp" "Unknown field is detected properly"
                    Expect.equal parentSelection "query" "Query as parent selection"
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
                | ValidationResult.QueryErrors [ QueryError.ExpandedScalarField (scalarField, "query", queryType) ] ->
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
                | ValidationResult.QueryErrors [ QueryError.UnknownField (scalarField, parentSelection, queryType) ] ->
                    Expect.equal scalarField "whatever" "Scalar field cannot be expanded"
                    Expect.equal parentSelection "composites" "Parent selection can be detected"
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
                | ValidationResult.QueryErrors [ QueryError.ExpandedScalarField (scalarField, parentSelection, queryType) ] ->
                    Expect.equal scalarField "values" "Scalar field cannot be expanded"
                    Expect.equal parentSelection "composites" "Parent selection can be detected"
                    Expect.equal queryType "Composite" "Type is propagted"

                | otherResults -> failwithf "Unexpected %A" otherResults

            | otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Query validation: succeeds when the query is valid" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    composites: [Composite!]!
                }

                type Composite {
                    id: String!
                    values: [String!]!
                    timestamp: Int!
                }
            """

            let query = Query.parse """
            query {
                composites {
                    id
                    values
                    timestamp
                }
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let validationResult = Query.validate query schema
                Expect.equal validationResult ValidationResult.Success "The query is valid"

            | otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Query validation with respect to fragments" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    composites: [Composite!]!
                }

                type Composite {
                    id: String!
                    values: [String!]!
                    timestamp: Int!
                }
            """

            let query = Query.parse """

            fragment CompositeParts on Composite {
                id
                values {
                    value
                }
            }

            query {
                composites {
                    ...CompositeParts
                    timestamp
                }
            }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.QueryErrors [ QueryError.ExpandedScalarField (scalarField, "composites", typeName) ] ->
                    Expect.equal scalarField "values" "Field 'values' cannot be expanded"
                    Expect.equal typeName "Composite" "Type name is correct"

                | otherResults -> failwithf "Unexpected %A" otherResults

            | otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Nullable Input object can be used as query variable" {
            let schema = Introspection.fromSchemaDefinition """
                input LoginCredentials {
                    username: String!
                    password: String!
                }

                type LoginResult {
                    token: String
                    success: Boolean!
                }

                type Query {
                    login(credentials: LoginCredentials): LoginResult!
                }
            """

            let query = Query.parse """
                query ($credentials: LoginCredentials!) {
                    login(credentials: $credentials) {
                        token
                        success
                    }

                    optionalLogin: login(credentials: null) {
                        token
                        success
                    }

                    anotherLogin: login {
                        token
                        success
                    }
                }
            """

            match schema, query with
            | Ok schema, Ok query ->
                match Query.validate query schema with
                | ValidationResult.Success -> ()
                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "NonNull Input object can be used as query variable" {
            let schema = Introspection.fromSchemaDefinition """
                input LoginCredentials {
                    username: String!
                    password: String!
                }

                type LoginResult {
                    token: String
                    success: Boolean!
                }

                type Query {
                    login(credentials: LoginCredentials!): LoginResult!
                }
            """

            let query = Query.parse """
                query ($credentials: LoginCredentials!) {
                    login(credentials: $credentials) {
                        token
                        success
                    }
                }
            """

            match schema, query with
            | Ok schema, Ok query ->
                match Query.validate query schema with
                | ValidationResult.Success -> ()
                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Reading variables in queries works" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser (username: String!) : User
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query ($input: String!) {
                    findUser(username: $input) {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.Success -> ()
                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Detecting unknown input variables work" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser: User
                }

                type User {
                    username: String!
                    email: String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query ($input: Whatever!) {
                    findUser {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.QueryErrors [ QueryError.UnknownInputVariable (name, typeName) ]  ->
                    Expect.equal name "input" "Variable input name is detected"
                    Expect.equal typeName "Whatever" "Type name is detected"

                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Input types can be converted into F# unions" {
            let schema = Introspection.fromSchemaDefinition """
                scalar DateTimeOffset

                enum Role { Admin, Customer }

                input LoginCredentials {
                    username: String!
                    password: String!
                    rememberMe: Boolean
                    createdAt: DateTimeOffset
                    roles: [Role!]!
                }

                type Query {
                    sorting(input: LoginCredentials): String!
                }

                schema {
                    query: Query
                }
            """

            match schema with
            | Error error -> failwith error
            | Ok schema ->
                let generated =
                    let globalTypes = CodeGen.createGlobalTypes schema
                    let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
namespace rec Test

[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type Role =
    | [<CompiledName "Admin">] Admin
    | [<CompiledName "Customer">] Customer

type LoginCredentials =
    { username: string
      password: string
      rememberMe: Option<bool>
      createdAt: Option<System.DateTimeOffset>
      roles: list<Role> }
"""
                let trimmedGenerated = trimContentEnd generated
                let trimmedExpected = trimContentEnd expected

                Expect.equal trimmedGenerated trimmedExpected "The code is generated correctly"
        }


        test "Next tick generates the correct next typeName" {
            let names = ResizeArray [
                "Chair"
                "Table"
            ]

            Expect.equal (CodeGen.nextTick "Table" names) "Table1" "Increment starts with 1"

            let conflictingName =  ResizeArray [
                "Chair"
                "Table"
                "Table1"
                "Table2"
                "Table3"
            ]

            Expect.equal (CodeGen.nextTick "Table" conflictingName) "Table4" "Next tick is counted"
        }

        test "Enum types can be converted into F# unions" {
            let schema = Introspection.fromSchemaDefinition """
                enum Sort {
                    ASCENDING,
                    descending
                }

                type Query {
                    sorting: Sort
                }

                schema {
                    query: Query
                }
            """



            match schema with
            | Error error -> failwith error
            | Ok schema ->
                let generated =
                    let globalTypes = CodeGen.createGlobalTypes schema
                    let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
namespace rec Test

[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type Sort =
    | [<CompiledName "ASCENDING">] Ascending
    | [<CompiledName "descending">] Descending
"""

                Expect.equal (trimContentEnd generated) (trimContentEnd expected) "The code is generated correctly"
        }

        test "Enum cases with 'Tags' is escaped" {
            let schema = Introspection.fromSchemaDefinition """
                enum Sort {
                    Tags,
                    Other
                }

                type Query {
                    sorting: Sort
                }

                schema {
                    query: Query
                }
            """

            match schema with
            | Error error -> failwith error
            | Ok schema ->
                let generated =
                    let globalTypes = CodeGen.createGlobalTypes schema
                    let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
namespace rec Test

[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type Sort =
    | [<CompiledName "Tags">] TAGS
    | [<CompiledName "Other">] Other
"""

                Expect.equal (trimContentEnd generated) (trimContentEnd expected) "The code is generated correctly"
        }        

        test "Enum types can be converted into F# unions maintaining text casing" {
            let schema = Introspection.fromSchemaDefinition """
                enum Sort {
                    ascendingOne,
                    DescendingTwo,
                    oneTwoThree,
                    oneTwo123,
                    SHA512,
                    sha256
                }

                type Query {
                    sorting: Sort
                }

                schema {
                    query: Query
                }
            """
            match schema with
            | Error error -> failwith error
            | Ok schema ->

            let generated =
                let globalTypes = CodeGen.createGlobalTypes schema
                let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

            let expected = """
namespace rec Test

[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type Sort =
    | [<CompiledName "ascendingOne">] AscendingOne
    | [<CompiledName "DescendingTwo">] DescendingTwo
    | [<CompiledName "oneTwoThree">] OneTwoThree
    | [<CompiledName "oneTwo123">] OneTwo123
    | [<CompiledName "SHA512">] Sha512
    | [<CompiledName "sha256">] Sha256
"""

            Expect.equal (trimContentEnd generated) (trimContentEnd expected) "The code is generated correctly"
        }

        test "Long enum types can be converted into F# unions" {
            let schema = Introspection.fromSchemaDefinition """
                enum Sort {
                    transport_distance,
                    transport_duration,
                    stationary_location_significance_day_part_duration,
                    stationary_location_significance_day_part_day_count
                }

                type Query {
                    sorting: Sort
                }

                schema {
                    query: Query
                }
            """
            match schema with
            | Error error -> failwith error
            | Ok schema ->

            let generated =
                let globalTypes = CodeGen.createGlobalTypes schema
                let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

            let expected = """
namespace rec Test

[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type Sort =
    | [<CompiledName "transport_distance">] TransportDistance
    | [<CompiledName "transport_duration">] TransportDuration
    | [<CompiledName "stationary_location_significance_day_part_duration">] StationaryLocationSignificanceDayPartDuration
    | [<CompiledName "stationary_location_significance_day_part_day_count">] StationaryLocationSignificanceDayPartDayCount
"""

            Expect.equal (trimContentEnd generated) (trimContentEnd expected) "The code is generated correctly"
        }

        test "Query types can be generated from schema" {
            let schema = Introspection.fromSchemaDefinition """
                enum Sort {
                    ASCENDING,
                    descending
                }

                type User {
                    username: String!
                    email: String!
                }

                type Query {
                    sorting: Sort
                    name: String!
                    age: Int
                    currentUser: User!
                    optionalUser: User
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query QueryName {
                    sorting
                    firstName: name
                    age
                    currentUser {
                        email
                    }
                    nextUser: currentUser {
                        email
                        username
                    }
                    optionalUser {
                        username
                    }
                }
            """

            match schema, query with
            | Ok schema, Ok query ->

                let name =
                    Query.findOperationName query
                    |> Option.defaultValue "DefaultQueryName"
                    |> CodeGen.normalizeName

                let generated =
                    let skipTypeName = false
                    let queryTypes = CodeGen.generateTypes "Root" query schema skipTypeName
                    let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
[<RequireQualifiedAccess>]
module rec Test.QueryName

type User = { email: string }
type NextUser = { email: string; username: string }
type OptionalUser = { username: string }

type Root =
    { sorting: Option<Sort>
      firstName: string
      age: Option<int>
      currentUser: User
      nextUser: NextUser
      optionalUser: Option<OptionalUser> }
"""

                Expect.equal (trimContentEnd generated) (trimContentEnd expected) "The code is generated correctly"

            | otherwise -> failwithf "%A" otherwise
        }


        test "Object types cannot be used input variables" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser (username: String!) : User
                    hello: String
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query ($input: User!) {
                    hello
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.QueryErrors [ QueryError.UnknownInputVariable (name, typeName) ]  ->
                    Expect.equal name "input" "Variable input name is detected"
                    Expect.equal typeName "User" "Type name is detected"

                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Unknwown field arguments can be detected" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser (username: String!) : User
                    hello: String
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query {
                    hello
                    findUser(usernam: "Whatsup?") {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.QueryErrors
                    [  QueryError.UnknownFieldArgument (name, field, typeName);
                       QueryError.MissingRequiredArgument ("username", "findUser", "Query") ] ->

                    Expect.equal name "usernam" "Variable name is detected"
                    Expect.equal field "findUser" "Field is detected"
                    Expect.equal typeName "Query" "Type name is detected"

                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Missing required field arguments can be detected" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser (username: String!) : User
                    hello: String
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query {
                    hello
                    findUser {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.QueryErrors [ QueryError.MissingRequiredArgument (name, field, typeName) ]  ->
                    Expect.equal name "username" "Variable name is detected"
                    Expect.equal field "findUser" "Field is detected"
                    Expect.equal typeName "Query" "Type name is detected"

                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Non-required field arguments don't give errors" {
            let schema = Introspection.fromSchemaDefinition """
                type Query {
                    findUser (username: String) : User
                    hello: String
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query {
                    hello
                    findUser {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.Success -> ()
                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Enums can be used as reference variables" {
            let schema = Introspection.fromSchemaDefinition """
                enum Interval {
                  Raw
                  Day
                  Month
                }

                type Query {
                    findUser (interval: Interval!) : User
                }

                type User {
                    username : String!
                    email : String!
                }

                schema {
                    query: Query
                }
            """

            let query = Query.parse """
                query ($interval: Interval!) {
                    findUser(interval: $interval) {
                        username
                        email
                    }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.Success -> ()
                | otherResults -> failwithf "Unexpected %A" otherResults

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Unknown input fields can be detected" {
            let schema = Introspection.fromSchemaDefinition """
                input Credentials {
                    username: String!
                    password: String!
                }

                type Mutation {
                    login (credentials: Credentials!) : String!
                }

                type Query {
                    dummy: String
                }

                schema {
                    mutation: Mutation
                    query: Query
                }
            """

            let query = Query.parse """
                mutation  {
                    login(credentials: { username: "", password: "", whatever: "" })
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                match Query.validate query schema with
                | ValidationResult.Success -> failwithf "expected to fail"
                | otherResults -> ()

            |  otherResults -> failwithf "Unexpected %A" otherResults
        }

        test "Input variables with required ID can be used properly" {
            let schema = Introspection.fromSchemaDefinition """
                type Foo {
                  id: ID!
                  name: String!
                }

                type Query {
                  foo(id: ID!): Foo
                  bar(id: ID): Foo
                }
            """

            let query = Query.parse """
                query($id: ID!) {
                  foo(id: $id) {
                    id
                    name
                  }

                  bar(id: $id) {
                    id
                    name
                  }
                }
            """

            match query, schema with
            | Ok query, Ok schema ->
                let result = Query.validate query schema
                Expect.equal result ValidationResult.Success "Should succeed"

            |  otherResults ->
                failwithf "Unexpected %A" otherResults
        }

        test "Field arguments can be parsed" {
            let query = Query.parse """
                query ($input: String!) {
                    findUser(
                        name: $input,
                        include: false,
                        limit: 10,
                        whatsUp: "value",
                        nested: { username: "hello" },
                        listStuff: [1,2,3,4,5],
                        interval: Raw
                    ) {
                        username
                        email
                    }
                }
            """

            match query with
            | Error error -> failwith error
            | Ok document -> Expect.equal true true "Query was parsed"
        }
    ]

let snowflakeTests = testList "Snowflaqe" [
    queryParsing
    SampleHasuraSchema.hasuraTests
    SampleGitHubSchema.githubTests
    SampleCraftSchema.tests
    SamplePostgraphile.tests
    SampleSentiantSchema.tests
]

[<EntryPoint>]
let main argv = runTests defaultConfig snowflakeTests
