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
      AnotherType: Option<int>
      ``of``: Option<int>
      ``break``: Option<int>
      ``checked``: Option<int>
      ``component``: Option<int>
      ``const``: Option<int>
      ``constraint``: Option<int>
      ``continue``: Option<int>
      ``event``: Option<int>
      ``external``: Option<int>
      ``include``: Option<int>
      ``mixin``: Option<int>
      ``parallel``: Option<int>
      ``process``: Option<int>
      ``protected``: Option<int>
      ``pure``: Option<int>
      ``sealed``: Option<int>
      ``tailcall``: Option<int>
      ``trait``: Option<int>
      ``virtual``: Option<int> }
"""
            let trimmedGenerated = Utilities.trimContentEnd generated
            let trimmedExpected = Utilities.trimContentEnd expected

            Expect.equal trimmedGenerated trimmedExpected "The code is generated correctly"
        }
]
