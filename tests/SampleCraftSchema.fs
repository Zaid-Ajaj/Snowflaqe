module SampleCraftSchema

open System
open Expecto
open Snowflaqe
open FSharp.Data.LiteralProviders
open Snowflaqe.Types

let [<Literal>] craftSchema = TextFile<"./CraftSchema.json">.Text

let validQuery = """
query HomeConfig {
  HomeConfig: entries(site: "stephenHamiltonNew" type: ["Home"]){
    __typename
    ...on Home_Home_Entry {
      __typename
      HeroLink
  	  Grid {
        ... on Grid_row_BlockType {
          __typename
          typeHandle
        	items {
            url
            width
            height
            id
          }
        }
      }
    }
  }
  Sections: entries(site: "stephenHamiltonNew", type: "PortfolioSection") {
    __typename
    ... on PortfolioSection_portfolioSection_Entry {
      __typename
      siteTitle
      siteSubtitle
      Grid {
        ... on Grid_row_BlockType {
          __typename
          items {
            id
            width
            height
            url
          }
        }
      }
    }
  }
}
"""

let tests = testList "Craft schema" [
    test "query is valid" {
        match Query.parse validQuery, Schema.parse craftSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal ValidationResult.Success result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }

    test "generates valid code" {
        let schema = Schema.parse craftSchema
        let query = Query.parse validQuery

        match schema, query with
        | Ok schema, Ok query ->

            let name =
                Query.findOperationName query
                |> Option.defaultValue "DefaultQueryName"
                |> CodeGen.normalizeName

            let generated =
                let queryTypes = CodeGen.generateTypes name "ErrorType" query schema
                let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                let file = CodeGen.createFile "Types.fs" [ ns ]
                CodeGen.formatAst file

            Expect.isNotEmpty generated "The code is generated correctly"

        | otherwise -> failwithf "%A" otherwise
    }
]