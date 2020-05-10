[<RequireQualifiedAccess>]
module Snowflaqe.Query

open Snowflaqe.Types
open GraphQLParser.AST
open GraphQLParser

let rec readVariableType (variableType: GraphQLType) = 
    match variableType.Kind with 
    | ASTNodeKind.NamedType -> 
        let namedType = unbox<GraphQLNamedType> variableType
        GraphqlVariableType.Ref namedType.Name.Value
    
    | ASTNodeKind.NonNullType -> 
        let nonNullType = unbox<GraphQLNonNullType> variableType
        GraphqlVariableType.NonNull (readVariableType nonNullType.Type)

    | ASTNodeKind.ListType -> 
        let listType = unbox<GraphQLListType> variableType
        GraphqlVariableType.List (readVariableType listType.Type)
    
    | astNodeType -> 
        failwithf "Unexpected ASTNodeType '%s' encountered when reading variable type" (astNodeType.ToString())

let readVariables (variables: seq<GraphQLVariableDefinition>) : GraphqlVariable list = 
    if isNull variables then 
        [ ]
    else 
        [
            for variable in variables ->
                {
                    name = variable.Variable.Name.Value
                    variableType = readVariableType variable.Type
                }
        ]

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
        let fieldSelection : GraphqlFieldSelection = {
            name = field.Name.Value
            arguments = listOrNone field.Arguments
            selectionSet = if isNull field.SelectionSet then None else Some (readSelections field.SelectionSet)
            directives = listOrNone field.Directives
            location = field.Location
        }

        Some (GraphqlNode.Field fieldSelection)

    | ASTNodeKind.FragmentSpread -> 
        let fragmentSpread = unbox<GraphQLFragmentSpread> node 
        Some (GraphqlNode.FragmentSpread fragmentSpread)

    | ASTNodeKind.FragmentDefinition ->     
        let fragmentDef = unbox<GraphQLFragmentDefinition> node 
        let def : GraphqlFragmentDefinition = {
            name = fragmentDef.Name.Value 
            selectionSet = if isNull fragmentDef.SelectionSet then None else Some (readSelections fragmentDef.SelectionSet)
            typeDef = fragmentDef.TypeCondition.Name.Value
            directives = listOrNone fragmentDef.Directives
            location = fragmentDef.Location
        }

        Some (GraphqlNode.FragmentDefinition def)

    | ASTNodeKind.OperationDefinition ->
        let operation = unbox<GraphQLOperationDefinition> node
        match operation.Operation with
        | OperationType.Query ->
            let name =
                if isNull operation.Name
                then None
                else Option.ofObj operation.Name.Value

            let query = GraphqlNode.Query {
                name = name
                directives = listOrNone operation.Directives
                variables = readVariables operation.VariableDefinitions
                selectionSet = readSelections operation.SelectionSet
            }

            Some query

        | OperationType.Mutation ->
            let name =
                if isNull operation.Name
                then None
                else Option.ofObj operation.Name.Value

            let mutation = GraphqlNode.Mutation {
                name = name
                directives = listOrNone operation.Directives
                variables = readVariables operation.VariableDefinitions
                selectionSet = readSelections operation.SelectionSet
            }

            Some mutation

        | _ -> 
            None
    | _ ->
        None

and readSelections (selectionSet: GraphQLSelectionSet) : SelectionSet = {
    location = selectionSet.Location
    nodes = List.choose readNode (List.ofSeq selectionSet.Selections)
}

let private lexer = Lexer()
let private parser = Parser(lexer)

let parse (content: string) : Result<GraphqlDocument, string> =
    try 
        let ast = parser.Parse(Source content)
        Ok { nodes = List.choose readNode (List.ofSeq ast.Definitions) }
    with 
    | ex -> Error ex.Message

/// Find the root operation of the document whether it is the root query or the root mutation
let findOperation (document: GraphqlDocument) = 
    document.nodes
    |> List.tryFind (function 
        | GraphqlNode.Query _ -> true
        | GraphqlNode.Mutation _ -> true
        | _ -> false)
    |> function 
        | Some (GraphqlNode.Query query) -> Some (GraphqlOperation.Query query)
        | Some (GraphqlNode.Mutation mutation) -> Some (GraphqlOperation.Mutation mutation)
        | _ -> None 

let rec expandFragments (nodes: GraphqlNode list) (fragments: GraphqlFragmentDefinition list) : GraphqlNode list = 
    nodes
    |> List.collect (function 
        | GraphqlNode.FragmentSpread spread -> 
            fragments
            |> List.tryFind (fun fragment -> fragment.name = spread.Name.Value)
            |> function 
                | None -> [ GraphqlNode.FragmentSpread spread ]
                | Some fragment -> 
                    match fragment.selectionSet with
                    | None -> [ ]
                    | Some selectionSet -> expandFragments selectionSet.nodes fragments

        | GraphqlNode.Field field -> 
            [ 
                match field.selectionSet with 
                | None -> GraphqlNode.Field field
                | Some selectionSet -> 
                    let modifiedNodes = expandFragments selectionSet.nodes fragments
                    let modifiedSelections = { selectionSet with nodes = modifiedNodes }
                    GraphqlNode.Field { field with selectionSet = Some modifiedSelections }
            ]

        | GraphqlNode.SelectionSet selectionSet -> 
            [
                let modifiedNodes = expandFragments selectionSet.nodes fragments
                GraphqlNode.SelectionSet { selectionSet with nodes = modifiedNodes }
            ]

        | anyOtherNode -> [ anyOtherNode ])

let expandDocumentFragments (document: GraphqlDocument) : GraphqlDocument = 
    let findFragmentDefinition = function 
        | GraphqlNode.FragmentDefinition definition -> Some definition
        | _ -> None 
    
    let fragments = List.choose findFragmentDefinition document.nodes

    let transformNode = function 
        | GraphqlNode.Query query -> 
            let modifiedSelections = { query.selectionSet with nodes = expandFragments query.selectionSet.nodes fragments }
            GraphqlNode.Query { query with selectionSet = modifiedSelections }
    
        | GraphqlNode.Mutation mutation -> 
            let modifiedSelections = { mutation.selectionSet with nodes = expandFragments mutation.selectionSet.nodes fragments }
            GraphqlNode.Mutation { mutation with selectionSet = modifiedSelections }

        | anythingElse -> anythingElse

    { document with nodes = List.map transformNode document.nodes }

let rec fieldCanExpand (field: GraphqlFieldType) = 
    match field with 
    | GraphqlFieldType.ObjectRef _ -> true 
    | GraphqlFieldType.EnumRef _ -> false 
    | GraphqlFieldType.Scalar _ -> false 
    | GraphqlFieldType.InputObjectRef _ -> false 
    | GraphqlFieldType.List innerField -> fieldCanExpand innerField
    | GraphqlFieldType.NonNull innerField -> fieldCanExpand innerField

let rec findFieldType (field: GraphqlFieldType) (schema: GraphqlSchema) = 
    match field with 
    | GraphqlFieldType.ObjectRef refType -> 
        schema.types 
        |> List.tryFind (function 
            | GraphqlType.Object objectDef -> objectDef.name = refType
            | _ -> false)
        |> function 
            | Some (GraphqlType.Object objectDef) -> Some objectDef
            | _ -> None

    | GraphqlFieldType.EnumRef _ -> None  
    | GraphqlFieldType.Scalar _ -> None 
    | GraphqlFieldType.InputObjectRef _ -> None 
    | GraphqlFieldType.List innerField -> findFieldType innerField schema
    | GraphqlFieldType.NonNull innerField -> findFieldType innerField schema

let rec validateFields (selection: SelectionSet) (graphqlType: GraphqlObject) (schema: GraphqlSchema) = 
    let fields = selection.nodes |> List.choose (function | GraphqlNode.Field field -> Some field | _ -> None)
    let unknownFields = 
        fields
        |> List.filter (fun field -> 
            match graphqlType.fields |> List.tryFind (fun fieldType -> fieldType.fieldName = field.name) with 
            | None -> true 
            | Some field -> false)

    [
        for field in unknownFields 
            do yield QueryError.UnknownField (field.name, graphqlType.name)
        
        for selectedField in fields do  
            yield! 
                graphqlType.fields 
                |> List.tryFind (fun fieldType -> fieldType.fieldName = selectedField.name)
                |> function 
                    | None -> [ ]
                    | Some graphqlField -> 
                        match selectedField.selectionSet with 
                        | Some selection when not (fieldCanExpand graphqlField.fieldType) -> 
                             [ QueryError.ExpandedScalarField (selectedField.name, graphqlType.name) ]
                        | Some selection -> 
                            match findFieldType graphqlField.fieldType schema with 
                            | None -> [ ]
                            | Some possibleExpansion -> validateFields selection possibleExpansion schema
                                
                        | _ ->
                            [ ]
    ]

/// Validates a document against the schema
let validate (document: GraphqlDocument) (schema: GraphqlSchema) : ValidationResult =
    match findOperation (expandDocumentFragments document) with 
    | None -> ValidationResult.NoQueryOrMutationProvided
    | Some (GraphqlOperation.Query query) -> 
        match Schema.findQuery schema with 
        | None -> ValidationResult.SchemaDoesNotHaveQueryType
        | Some queryType ->
            match validateFields query.selectionSet queryType schema with 
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors

    | Some (GraphqlOperation.Mutation mutation) ->  
        match Schema.findQuery schema with 
        | None -> ValidationResult.SchemaDoesNotHaveMutationType
        | Some mutationType -> 
            match validateFields mutation.selectionSet mutationType schema with 
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors