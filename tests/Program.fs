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
            | Ok (GraphqlDocument.Query parsedQuery) -> Expect.equal parsedQuery.name None "This query has no name defined"
            | Ok (_) -> failwith "Unexpected results"
            | Error error -> failwith error
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
]


let snowflakeTests = testList "Snowflake" [ queryParsing ]

[<EntryPoint>]
let main argv = runTests defaultConfig snowflakeTests