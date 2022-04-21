module SampleGithubSchema

open FSharp.Data.LiteralProviders
open Expecto
open Snowflaqe
open Snowflaqe.Types

let [<Literal>] githubSchema = TextFile<"./GitHubSchema.json">.Text
let [<Literal>] typesFileName = "Types.fs"

let queryWithUnion = """
query GitHubSearch {
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

let queryWithUnionMissingTypeName = """
query GitHubSearch {
  search(query:"Snowflaqe", type:REPOSITORY, first:10) {
    nodes {
      ... on Repository {
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
                  __typename
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

let githubTests = testList "GitHub tests" [
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
            let fragments = Query.findNestedInlineFragments query.nodes
            Expect.equal (List.length fragments) 2 "There are two inline fragments"
    }

    test "Query can be parsed and fragments are not expanded" {
        match Query.parse validQuery with
        | Error error -> failwith error
        | Ok query ->
            let expandedNodes = Query.expandFragments query.nodes [ ]
            let fragments = Query.findNestedInlineFragments expandedNodes
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
                              __typename
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

    test "Missing type name from union fields can be detected" {
      match Query.parse queryWithUnionMissingTypeName, Schema.parse githubSchema with
      | Ok query, Ok schema ->
          let result = Query.validate query schema
          Expect.equal result (ValidationResult.QueryErrors [QueryError.MissingTypeNameField("SearchResultItem", "Repository", "nodes")])  "Validation should succeed"
      | _ ->
          failwith "Unexpected"
    }

    test "Detect invalid inline fragments on fields" {
      let queryWithInvalidFragment = """
        query GetPullRequests($org: String!) {
          organization(login: $org) {
            name
            url
            repositories(first: 100) {
              nodes {
                name
                pullRequests(first: 100, states: OPEN) {
                  nodes {
                    number
                    title
                    url
                    body
                    author {
                      login
                    }
                    reviews(last: 10, states: APPROVED) {
                      nodes {
                        author {
                          login
                        }
                        ... on User {
                          __typename
                          bio
                          id
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

      match Query.parse queryWithInvalidFragment, Schema.parse githubSchema with
      | Ok query, Ok schema ->
          let result = Query.validate query schema
          Expect.equal (ValidationResult.QueryErrors [QueryError.InvalidInlineFragment("PullRequestReview", "User", "nodes")]) result "Validation should succeed"
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
                              __typename
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
                let skipTypeName = false
                let queryTypes = CodeGen.generateTypes name query schema skipTypeName
                let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

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
                |> Option.defaultValue "GitHubSearch"
                |> CodeGen.normalizeName

            let generated =
                let skipTypeName = false
                let queryTypes = CodeGen.generateTypes name query schema skipTypeName
                let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

            // printfn "%s" generated

            Expect.isNotEmpty generated "The code is generated correctly"

        | otherwise ->
            failwithf "%A" otherwise
    }
]
