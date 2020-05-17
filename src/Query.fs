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
                    variableName = variable.Variable.Name.Value
                    variableType = readVariableType variable.Type
                }
        ]

let rec readArgumentValue (argValue: GraphQLValue) : FieldArgumentValue option =
    match argValue.Kind with
    | ASTNodeKind.Variable ->
        let variable = unbox<GraphQLVariable> argValue
        Some (FieldArgumentValue.Variable variable.Name.Value)

    | ASTNodeKind.BooleanValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.Boolean (if value.Value = "true" then true else false))

    | ASTNodeKind.NullValue ->
        Some FieldArgumentValue.Null

    | ASTNodeKind.StringValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.String value.Value)

    | ASTNodeKind.IntValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.Int (int value.Value))

    | ASTNodeKind.ListValue ->
        let value = unbox<GraphQLListValue> argValue
        let values = List.choose readArgumentValue (List.ofSeq value.Values)
        Some (FieldArgumentValue.List values)

    | ASTNodeKind.ObjectValue ->
        let object = unbox<GraphQLObjectValue> argValue
        let fields = if isNull object.Fields then [ ] else List.ofSeq object.Fields

        fields
        |> List.ofSeq
        |> List.choose (fun field ->
            match readArgumentValue field.Value with
            | None -> None
            | Some fieldValue -> Some (field.Name.Value, fieldValue))
        |> FieldArgumentValue.Object
        |> Some

    | _ ->
        None

let readArguments (args: seq<GraphQLArgument>) : GraphqlFieldArgument list =
    if isNull args then
        [ ]
    else
        [
            for arg in args do
                match readArgumentValue arg.Value with
                | None -> ()
                | Some argumentValue -> { name = arg.Name.Value; value = argumentValue }
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
            arguments = if isNull field.Arguments then [ ] else readArguments field.Arguments
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
            name = if isNull fragmentDef.Name then "" else fragmentDef.Name.Value
            selectionSet = if isNull fragmentDef.SelectionSet then None else Some (readSelections fragmentDef.SelectionSet)
            typeDef = if isNull fragmentDef.TypeCondition then None else Some fragmentDef.TypeCondition.Name.Value
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

let rec validateFields (parentField: string) (selection: SelectionSet) (graphqlType: GraphqlObject) (schema: GraphqlSchema) =
    let fields = selection.nodes |> List.choose (function | GraphqlNode.Field field -> Some field | _ -> None)
    let unknownFields =
        fields
        |> List.filter (fun field ->
            match graphqlType.fields |> List.tryFind (fun fieldType -> fieldType.fieldName = field.name) with
            | None -> true
            | Some field -> false)

    [
        for field in unknownFields
            do yield QueryError.UnknownField (field.name, parentField, graphqlType.name)

        for selectedField in fields do
            yield!
                graphqlType.fields
                |> List.tryFind (fun fieldType -> fieldType.fieldName = selectedField.name)
                |> function
                    | None -> [ ]
                    | Some graphqlField ->
                        let selectionErrors = 
                            match selectedField.selectionSet with
                            | Some selection when not (fieldCanExpand graphqlField.fieldType) ->
                                 [ QueryError.ExpandedScalarField (selectedField.name, parentField, graphqlType.name) ]
                            | Some selection ->
                                match findFieldType graphqlField.fieldType schema with
                                | None -> [ ]
                                | Some possibleExpansion -> validateFields selectedField.name selection possibleExpansion schema

                            | _ ->
                                [ ]

                        let allowedArguments  = List.map fst graphqlField.args
                        let unknownArguments = 
                            selectedField.arguments 
                            |> List.filter (fun arg -> not (List.contains arg.name allowedArguments))
                            |> List.map (fun arg -> QueryError.UnknownFieldArgument(arg.name, selectedField.name, graphqlType.name))

                        let missingRequiredArguments = 
                            let requiredArgs = 
                                graphqlField.args
                                |> List.filter (fun (argName, argType) -> 
                                    match argType with 
                                    | GraphqlFieldType.NonNull _ -> true 
                                    | _ -> false)
                                |> List.map (fun (argName, argType) -> argName) 

                            let selectedArgs = 
                                selectedField.arguments
                                |> List.map (fun arg -> arg.name)

                            [ 
                                for requiredArg in requiredArgs do 
                                    if not (List.contains requiredArg selectedArgs)
                                    then yield QueryError.MissingRequiredArgument(requiredArg, selectedField.name, graphqlType.name)
                            ]

                        let allErrors = [
                            yield! selectionErrors
                            yield! unknownArguments
                            yield! missingRequiredArguments
                        ]

                        allErrors
    ]

let rec variableName = function
    | GraphqlVariableType.Ref variable -> variable
    | GraphqlVariableType.NonNull variable -> variableName variable
    | GraphqlVariableType.List variable -> variableName variable

let validateInputVariables (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    [
        for variable in variables do
            let name = variableName variable.variableType
            if [ "Int"; "String"; "Float"; "Boolean"; "ID" ] |> List.contains name then
                ()
            else
                yield!
                    schema.types
                    |> List.tryFind (function
                        | GraphqlType.Scalar (GraphqlScalar.Custom customScalar) -> customScalar = name
                        | GraphqlType.Enum enumType -> enumType.name = name
                        | GraphqlType.InputObject inputType -> inputType.name = name
                        | _ -> false)
                    |> function
                        | None ->
                            [ QueryError.UnknownInputVariable(variable.variableName, name) ]
                        | Some _ ->
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
            let fieldErrors = validateFields "query" query.selectionSet queryType schema
            let inputVariableErrors = validateInputVariables query.variables schema
            let queryErrors = [ yield! fieldErrors; yield! inputVariableErrors ]
            match queryErrors with
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors

    | Some (GraphqlOperation.Mutation mutation) ->
        match Schema.findQuery schema with
        | None -> ValidationResult.SchemaDoesNotHaveMutationType
        | Some mutationType ->
            let fieldErrors = validateFields "mutation" mutation.selectionSet mutationType schema
            let inputVariableErrors = validateInputVariables mutation.variables schema
            let queryErrors = [ yield! fieldErrors; yield! inputVariableErrors ]
            match queryErrors with
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors