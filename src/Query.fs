module Snowflake.Query

open Snowflake.Types
open GraphQLParser.AST
open GraphQLParser

let rec readNode (node: ASTNode) = 
    match node.Kind with 
    | ASTNodeKind.Name -> 
        let nameNode = unbox<GraphQLName> node
        Some (GraphqlNode.Name(nameNode, nameNode.Location))

    | ASTNodeKind.SelectionSet -> 
        let selectionSet = unbox<GraphQLSelectionSet> node
        Some (GraphqlNode.SelectionSet (readSelections selectionSet))

    | ASTNodeKind.Field -> 
        let field = unbox<GraphQLFieldSelection> node
        let fieldSelection = {
            name = field.Name.Value
            arguments = listOrNone field.Arguments
            selectionSet = if isNull field.SelectionSet then None else Some (readSelections field.SelectionSet) 
            directives = listOrNone field.Directives
            location = field.Location
        }

        Some (GraphqlNode.Field fieldSelection)
    | _ -> 
        None 

and readSelections (selectionSet: GraphQLSelectionSet) : SelectionSet = {
    location = selectionSet.Location
    nodes = List.choose readNode (List.ofSeq selectionSet.Selections)
}

let parseDocument (document: GraphQLDocument) : Result<GraphqlDocument, string> = 
    if document.Definitions.Count > 0 then 
        match document.Definitions.[0].Kind with 
        | ASTNodeKind.OperationDefinition -> 
            let operation = unbox<GraphQLOperationDefinition> document.Definitions.[0]
            match operation.Operation with 
            | OperationType.Query -> 
                let name = 
                    if isNull operation.Name 
                    then None 
                    else Option.ofObj operation.Name.Value
                
                let query = GraphqlDocument.Query {
                    name = name 
                    directives = listOrNone operation.Directives 
                    variables = listOrNone operation.VariableDefinitions
                    selectionSet = readSelections operation.SelectionSet 
                }

                Ok query
            | OperationType.Mutation -> 
                let name = 
                    if isNull operation.Name 
                    then None 
                    else Option.ofObj operation.Name.Value

                let mutation = GraphqlDocument.Mutation {
                    name = name 
                    directives = listOrNone operation.Directives 
                    variables = listOrNone operation.VariableDefinitions
                    selectionSet = readSelections operation.SelectionSet
                }

                Ok mutation

            | otherwise -> Error "Subscription type not supported"
        | _ ->
            Error "Expected the first to be an operation. Either starting with query { ... } or mutation { ... }" 
    else 
        Error "This document is empty"

let private lexer = Lexer()
let private parser = Parser(lexer)

let parse (content: string) = 
    try parseDocument (parser.Parse(Source content))
    with ex -> Error ex.Message