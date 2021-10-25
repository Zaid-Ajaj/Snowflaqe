module SampleHasuraSchema

open System
open Expecto
open Snowflaqe
open FSharp.Data.LiteralProviders

let [<Literal>] firstSchema = TextFile<"./HasuraSchema.json">.Text
let [<Literal>] typesFileName = "Types.fs"

let query = """
query GetCurrentOrderQuery($userId: Int!, $deliveryDate: timestamptz!) {
  userOrders(
    where: { userId: { _eq: $userId }, deliveryDate: { _gte: $deliveryDate } }
  ) {
    status
    id
    deliveryDate
    userOrderDetails {
      amount
      id
      price
      status
    }
  }
}
"""

let queryWithOptionalTimestamptz = """
query GetCurrentOrderQuery($userId: Int!, $deliveryDate: timestamptz) {
  userOrders(
    where: { userId: { _eq: $userId }, deliveryDate: { _gte: $deliveryDate } }
  ) {
    status
    id
    deliveryDate
    userOrderDetails {
      amount
      id
      price
      status
    }
  }
}
"""

let queryWithCustomType = """
query GetCurrentOrderQuery($userId: Int!, $deliveryDate: myCustomType!) {
  userOrders(
    where: { userId: { _eq: $userId }, deliveryDate: { _gte: $deliveryDate } }
  ) {
    status
    id
    deliveryDate
    userOrderDetails {
      amount
      id
      price
      status
    }
  }
}
"""
let hasuraTests = testList "Hasura" [
    test "Timestamptz is converted to DateTimeOffset" {
        let parsedSchema = Schema.parse firstSchema
        match parsedSchema with
        | Error error -> failwithf "Failed to parse the hasura schema:\n%s" error
        | Ok schema ->
            let parsedQuery = Query.parse query
            match parsedQuery with
            | Error error -> failwithf "Failed to parse query: %s" error
            | Ok query ->
                let name =
                    Query.findOperationName query
                    |> Option.defaultValue "DefaultQueryName"
                    |> CodeGen.normalizeName

                let generated =
                    let queryTypes = CodeGen.generateTypes "Root" "ErrorType" query schema
                    let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
[<RequireQualifiedAccess>]
module rec Test.GetCurrentOrderQuery

type InputVariables =
    { userId: int
      deliveryDate: System.DateTimeOffset }

/// An array relationship
type userOrderDetails =
    { amount: string
      id: int
      price: float
      status: string }

/// fetch data from the table: "userOrders"
type userOrders =
    { status: string
      id: int
      deliveryDate: System.DateTimeOffset
      /// An array relationship
      userOrderDetails: list<userOrderDetails> }

/// query root
type Root =
    { /// fetch data from the table: "userOrders"
      userOrders: list<userOrders> }

"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code is correct"
    }

    test "Optional timestamptz is converted to Option<DateTimeOffset>" {
        let parsedSchema = Schema.parse firstSchema
        match parsedSchema with
        | Error error -> failwithf "Failed to parse the hasura schema:\n%s" error
        | Ok schema ->
            let parsedQuery = Query.parse queryWithOptionalTimestamptz
            match parsedQuery with
            | Error error -> failwithf "Failed to parse query: %s" error
            | Ok query ->
                let name =
                    Query.findOperationName query
                    |> Option.defaultValue "DefaultQueryName"
                    |> CodeGen.normalizeName

                let generated =
                    let queryTypes = CodeGen.generateTypes "Root" "ErrorType" query schema
                    let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
[<RequireQualifiedAccess>]
module rec Test.GetCurrentOrderQuery

type InputVariables =
    { userId: int
      deliveryDate: Option<System.DateTimeOffset> }

/// An array relationship
type userOrderDetails =
    { amount: string
      id: int
      price: float
      status: string }

/// fetch data from the table: "userOrders"
type userOrders =
    { status: string
      id: int
      deliveryDate: System.DateTimeOffset
      /// An array relationship
      userOrderDetails: list<userOrderDetails> }

/// query root
type Root =
    { /// fetch data from the table: "userOrders"
      userOrders: list<userOrders> }

"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code correct"
    }

    test "Custom types map to strings in query variables" {
        let parsedSchema = Schema.parse firstSchema
        match parsedSchema with
        | Error error -> failwithf "Failed to parse the hasura schema:\n%s" error
        | Ok schema ->
            let parsedQuery = Query.parse queryWithCustomType
            match parsedQuery with
            | Error error -> failwithf "Failed to parse query: %s" error
            | Ok query ->
                let name =
                    Query.findOperationName query
                    |> Option.defaultValue "DefaultQueryName"
                    |> CodeGen.normalizeName

                let generated =
                    let queryTypes = CodeGen.generateTypes "Root" "ErrorType" query schema
                    let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                    let file = CodeGen.createFile typesFileName [ ns ]
                    CodeGen.formatAst file typesFileName

                let expected = """
[<RequireQualifiedAccess>]
module rec Test.GetCurrentOrderQuery

type InputVariables = { userId: int; deliveryDate: string }

/// An array relationship
type userOrderDetails =
    { amount: string
      id: int
      price: float
      status: string }

/// fetch data from the table: "userOrders"
type userOrders =
    { status: string
      id: int
      deliveryDate: System.DateTimeOffset
      /// An array relationship
      userOrderDetails: list<userOrderDetails> }

/// query root
type Root =
    { /// fetch data from the table: "userOrders"
      userOrders: list<userOrders> }
"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code correct"
    }
]
