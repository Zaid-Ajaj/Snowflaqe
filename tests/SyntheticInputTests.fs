module SampleSyntheticInputSchema

open System
open Expecto
open Snowflaqe
open FSharp.Data.LiteralProviders

let [<Literal>] firstSchema = TextFile<"./SyntheticInput.json">.Text
let [<Literal>] typesFileName = "Types.fs"

let tests = testList "Hasura" [
    test "Input types with conflicting fields have a [<CLIMutable>] attribute" {
        let schema = Schema.parse firstSchema

        match schema with
        | Error error -> failwith error
        | Ok schema ->
            let generated =
                let normalizeEnumCases = true
                let globalTypes = CodeGen.createGlobalTypes schema normalizeEnumCases
                let ns = CodeGen.createNamespace [ "Test" ] globalTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

            let expected = """
namespace rec Test

/// An input object with conflicting fields
[<CLIMutable>]
type SyntheticInput =
    { anotherType: Option<int>
      AnotherType: Option<int> }
"""
            let trimmedGenerated = Utilities.trimContentEnd generated
            let trimmedExpected = Utilities.trimContentEnd expected

            Expect.equal trimmedGenerated trimmedExpected "The code is generated correctly"
        }
]