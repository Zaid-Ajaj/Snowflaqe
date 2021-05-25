module SamplePostgraphile

open System.IO
open Expecto
open Snowflaqe
open Snowflaqe.Types

let [<Literal>] typesFileName = "Types.fs"

let schemaPath = Utilities.path [ Utilities.tests; "PostgraphileSchema.json" ]
let schema = File.ReadAllText schemaPath
let query = """
query {
  allObjects(condition: { objectId:55, objectTypeId:2 }) {
    totalCount
    nodes {
      name
      path
      aggregatedMeterReadingsByObjectId {
        nodes {
          timestamp
          value
        }
      }
      meterByObjectId {
        objectId
        ean
        importCode
        meterFunction
        meterTypeByMeterTypeId{
          description
        }
    }
    objectTypeByObjectTypeId {
        objectTypeId
        description
      }
    }
  }
}
"""
let tests = testList "Postgraphile" [
    test "Schema can be parsed" {
        let parsedSchema = Schema.parse schema
        match parsedSchema with
        | Error error -> failwith error
        | Ok schema -> ()
    }

    test "Query can be generated" {
        let parsedSchema = Schema.parse schema
        let parsedQuery = Query.parse query
        match parsedSchema, parsedQuery with
        | Error error, _ -> failwith error
        | _, Error error -> failwith error
        | Ok schema, Ok query ->
            match Query.validate query schema  with
            | ValidationResult.Success  ->
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
module rec Test.DefaultQueryName

type AggregatedMeterReading =
    { timestamp: System.DateTimeOffset
      value: Option<decimal> }

type AggregatedMeterReadingsConnection =
    { nodes: list<Option<AggregatedMeterReading>> }

type MeterType = { description: string }
type Meter =
    { objectId: int
      ean: Option<string>
      importCode: Option<string>
      meterFunction: int
      meterTypeByMeterTypeId: Option<MeterType> }

type ObjectType =
    { objectTypeId: int
      description: string }

type Object =
    { name: string
      path: string
      aggregatedMeterReadingsByObjectId: AggregatedMeterReadingsConnection
      meterByObjectId: Option<Meter>
      objectTypeByObjectTypeId: Option<ObjectType> }

type ObjectsConnection =
    { totalCount: int
      nodes: list<Option<Object>> }

type Root =
    { allObjects: Option<ObjectsConnection> }
"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code is correct"

            | otherResults ->
                failwithf "Unexpected error occured while validating Postgraphile query:\n%A" otherResults
    }
]
