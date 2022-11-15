module SamplePostgraphile

open System
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
                    let skipTypeName = false
                    let queryTypes = CodeGen.generateTypes "Root" query schema skipTypeName
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
    {
      /// A list of `AggregatedMeterReading` objects.
      nodes: list<Option<AggregatedMeterReading>>
    }

/// Reads a single `MeterType` that is related to this `Meter`.
type MeterType = { description: string }

/// Reads a single `Meter` that is related to this `Object`.
type Meter =
    {
      objectId: int
      ean: Option<string>
      importCode: Option<string>
      meterFunction: int
      /// Reads a single `MeterType` that is related to this `Meter`.
      meterTypeByMeterTypeId: Option<MeterType>
    }

/// Reads a single `ObjectType` that is related to this `Object`.
type ObjectType =
    { objectTypeId: int
      description: string }

/// A list of `Object` objects.
type Object =
    {
      name: string
      path: string
      /// Reads and enables pagination through a set of `AggregatedMeterReading`.
      aggregatedMeterReadingsByObjectId: AggregatedMeterReadingsConnection
      /// Reads a single `Meter` that is related to this `Object`.
      meterByObjectId: Option<Meter>
      /// Reads a single `ObjectType` that is related to this `Object`.
      objectTypeByObjectTypeId: Option<ObjectType>
    }

/// Reads and enables pagination through a set of `Object`.
type ObjectsConnection =
    {
      /// The count of *all* `Object` you could get from the connection.
      totalCount: int
      /// A list of `Object` objects.
      nodes: list<Option<Object>>
    }

/// The root query type which gives access points into the data universe.
type Root =
    {
      /// Reads and enables pagination through a set of `Object`.
      allObjects: Option<ObjectsConnection>
    }
"""
                Expect.equal (Utilities.trimContentEnd generated) (Utilities.trimContentEnd expected) "The generated code is correct"

            | otherResults ->
                failwithf "Unexpected error occured while validating Postgraphile query:\n%A" otherResults
    }
]
