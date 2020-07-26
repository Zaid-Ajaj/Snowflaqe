module SampleGithubSchema

open System
open Expecto
open Snowflaqe
open FSharp.Data.LiteralProviders
open Snowflaqe.Types

let [<Literal>] githubSchema = TextFile<"./GithubSchema.json">.Text

let queryWithUnion = """
query GithubSearch {
  search(query:"Snowflaqe", type:REPOSITORY, first:10) {
    nodes {
      ... on Repository {
        __typename
        name
        nameWithOwner
      }
      ... on App {
        __typename
        id
        name
      }

      ... on Issue {
        __typename
        id
        title
      }
    }
  }
}
"""

let validQuery = """
query GetPullRequests($org: String!) {
  organization(login: $org) {
    name
    url
    repositories(first: 1) {
      nodes {
        name
        pullRequests(first: 1, states: MERGED) {
          nodes {
            number
            title
            url
            body
            author {
              login
              url
            }
            mergedBy {
              login
              resourcePath
            }
            reviews(last: 10, states: APPROVED) {
              nodes {
                author {
                  avatarUrl
                  login
                  ... on Bot {
                    __typename
                    id
                    login
                  }
                  ... on User {
                    __typename
                    id
                    bio
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
"""

let githubTests = testList "Github tests" [
    test "Schema can be parsed" {
        match Schema.parse githubSchema with
        | Error error -> failwith error
        | Ok schema ->
            let actor = Schema.findTypeByName "Actor" schema
            Expect.isSome actor "Actor should be parsed"
    }

    test "Query can be parsed" {
        match Query.parse validQuery with
        | Error error -> failwith error
        | Ok query -> ()
    }

    test "Query can be parsed and inline fragments can be found" {
        match Query.parse validQuery with
        | Error error -> failwith error
        | Ok query ->
            let fragments = Query.findInlineFragments query.nodes
            Expect.equal (List.length fragments) 2 "There are two inline fragments"
    }

    test "Query can be parsed and fragments are not expanded" {
        match Query.parse validQuery with
        | Error error -> failwith error
        | Ok query ->
            let expandedNodes = Query.expandFragments query.nodes [ ]
            let fragments = Query.findInlineFragments expandedNodes
            Expect.equal 2 (List.length fragments) "There are two inline fragments"
    }

    test "Query can be validated" {
        match Query.parse validQuery, Schema.parse githubSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal ValidationResult.Success result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }

    test "Query can be validated when an an interface doesn't have subtype selections" {
        let simplifiedValidQuery = """
            query GetPullRequests($org: String!) {
              organization(login: $org) {
                name
                url
                repositories(first: 1) {
                  nodes {
                    name
                    pullRequests(first: 1, states: MERGED) {
                      nodes {
                        number
                        title
                        url
                        body
                        author {
                          login
                          url
                        }
                        mergedBy {
                          login
                          resourcePath
                        }
                        reviews(last: 10, states: APPROVED) {
                          nodes {
                            author {
                              avatarUrl
                              login
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
        """

        match Query.parse simplifiedValidQuery, Schema.parse githubSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal ValidationResult.Success result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }

    test "Unknown subtype can be detected" {
        let simplifiedValidQuery = """
            query GetPullRequests($org: String!) {
              organization(login: $org) {
                name
                url
                repositories(first: 1) {
                  nodes {
                    name
                    pullRequests(first: 1, states: MERGED) {
                      nodes {
                        number
                        title
                        url
                        body
                        author {
                          login
                          url
                        }
                        mergedBy {
                          login
                          resourcePath
                        }
                        reviews(last: 10, states: APPROVED) {
                          nodes {
                            author {
                              avatarUrl
                              login
                              ... on WeirdSubType {
                                __typename
                                fieldz
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
        """

        match Query.parse simplifiedValidQuery, Schema.parse githubSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal (ValidationResult.QueryErrors [QueryError.UnknownSubType("Actor", "WeirdSubType", "author")]) result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }


    test "Unknown fields on subtypes can be detected" {
        let simplifiedValidQuery = """
            query GetPullRequests($org: String!) {
              organization(login: $org) {
                name
                url
                repositories(first: 1) {
                  nodes {
                    name
                    pullRequests(first: 1, states: MERGED) {
                      nodes {
                        number
                        title
                        url
                        body
                        author {
                          login
                          url
                        }
                        mergedBy {
                          login
                          resourcePath
                        }
                        reviews(last: 10, states: APPROVED) {
                          nodes {
                            author {
                              avatarUrl
                              login
                              ... on User {
                                __typename
                                fieldz
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
        """

        match Query.parse simplifiedValidQuery, Schema.parse githubSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            match result with
            | ValidationResult.QueryErrors [ QueryError.UnknownField("fieldz", "author", "User") ] -> ()
            | validation -> failwithf "Unexpected %A" validation

        | _ ->
            failwith "Unexpected"
    }

    test "Query types can be generated from schema" {
        let schema = Schema.parse githubSchema

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

            // printfn "%s" generated

            Expect.isNotEmpty generated "The code is generated correctly"

        | otherwise -> failwithf "%A" otherwise
    }

    test "Query types with union can be generated from schema" {
        let schema = Schema.parse githubSchema

        let query = Query.parse queryWithUnion

        match schema, query with
        | Ok schema, Ok query ->

            let name =
                Query.findOperationName query
                |> Option.defaultValue "GithubSearch"
                |> CodeGen.normalizeName

            let generated =
                let queryTypes = CodeGen.generateTypes name "ErrorType" query schema
                let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                let file = CodeGen.createFile "Types.fs" [ ns ]
                CodeGen.formatAst file

            // printfn "%s" generated

            Expect.isNotEmpty generated "The code is generated correctly"

        | otherwise ->
            failwithf "%A" otherwise
    }
]