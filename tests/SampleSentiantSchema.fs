module SampleSentiantSchema

open System.IO
open Expecto
open Snowflaqe
open Snowflaqe.Types

let schemaPath = Utilities.path [ Utilities.tests; "SentiantSchema.json" ]
let schema = File.ReadAllText schemaPath
let query = """
query ($user_id: String!) {
    user(id: $user_id) {
        id
        __typename
        event_history(from: "2021-04-10", to: "2021-04-13", mode: car, type: Transport) {
            __typename
            ... on Transport {
                __typename
                mode
                distance
                event_id
                behavior_scores {
                    __typename
                    ... on CarBehaviorScores {
                        __typename
                        handheld_calling
                        hard_events
                        legal_v2
                    }
                }
            }
        }
    }
}
"""

let tests = testList "Sentiant schema" [
    test "query is valid" {
        match Query.parse query, Schema.parse schema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal ValidationResult.Success result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }
]