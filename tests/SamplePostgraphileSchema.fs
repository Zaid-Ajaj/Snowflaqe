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

/// A list of `AggregatedMeterReading` objects.
type AggregatedMeterReading =
    { timestamp: System.DateTimeOffset
      value: Option<decimal> }

/// Reads and enables pagination through a set of `AggregatedMeterReading`.
type AggregatedMeterReadingsConnection =
    { nodes: list<Option<AggregatedMeterReading>> }

/// Reads a single `MeterType` that is related to this `Meter`.
type MeterType = { description: string }
/// Reads a single `Meter` that is related to this `Object`.
/// Reads a single `Meter` that is related to this `Object`.
type Meter =
    { objectId: int
      ean: Option<string>
      importCode: Option<string>
      /// Reads a single `MeterType` that is related to this `Meter`.
      meterFunction: int
      meterTypeByMeterTypeId: Option<MeterType> }
/// Reads a single `ObjectType` that is related to this `Object`.

type ObjectType =
    { objectTypeId: int
      description: string }
/// A list of `Object` objects.

type Object =
    { name: string
      /// Reads and enables pagination through a set of `AggregatedMeterReading`.
      path: string
      /// Reads a single `Meter` that is related to this `Object`.
      aggregatedMeterReadingsByObjectId: AggregatedMeterReadingsConnection
      /// Reads a single `ObjectType` that is related to this `Object`.
      meterByObjectId: Option<Meter>
      objectTypeByObjectTypeId: Option<ObjectType> }
/// Reads and enables pagination through a set of `Object`.

    { /// The count of *all* `Object` you could get from the connection.
      totalCount: int
      /// A list of `Object` objects.
    { totalCount: int
      nodes: list<Option<Object>> }
/// The root query type which gives access points into the data universe.

    { /// Reads and enables pagination through a set of `Object`.
      allObjects: Option<ObjectsConnection> }
    { allObjects: Option<ObjectsConnection> }
"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code is correct"

            | otherResults ->
                failwithf "Unexpected error occured while validating Postgraphile query:\n%A" otherResults
    }
]
