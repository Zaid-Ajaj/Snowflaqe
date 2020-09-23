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
        Some (FieldArgumentValue.Boolean (value.Value = "true"))

    | ASTNodeKind.NullValue ->
        Some FieldArgumentValue.Null

    | ASTNodeKind.StringValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.String value.Value)

    | ASTNodeKind.IntValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.Int (int value.Value))

    | ASTNodeKind.EnumValue ->
        let value = unbox<GraphQLScalarValue> argValue
        Some (FieldArgumentValue.EnumCase value.Value)

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
            alias = if isNull field.Alias then None else Some field.Alias.Value
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

    | ASTNodeKind.InlineFragment ->
        let inlineFragment = unbox<GraphQLInlineFragment> node
        Some (GraphqlNode.InlineFragment {
            typeCondition = inlineFragment.TypeCondition.Name.Value
            selection = readSelections inlineFragment.SelectionSet
        })

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

/// Find the root operation of the document whether it is the root query or the root mutation
let findOperationName (document: GraphqlDocument) =
    document.nodes
    |> List.tryFind (function
        | GraphqlNode.Query _ -> true
        | GraphqlNode.Mutation _ -> true
        | _ -> false)
    |> function
        | Some (GraphqlNode.Query query) -> query.name
        | Some (GraphqlNode.Mutation mutation) -> mutation.name
        | _ -> None


let rec findInlineFragments (nodes: GraphqlNode list) =
    nodes
    |> List.collect (function
        | GraphqlNode.InlineFragment fragment -> [ fragment ]
        | GraphqlNode.SelectionSet set -> findInlineFragments set.nodes
        | GraphqlNode.Query query -> findInlineFragments query.selectionSet.nodes
        | GraphqlNode.Mutation mutation -> findInlineFragments mutation.selectionSet.nodes
        | GraphqlNode.Field field ->
            match field.selectionSet with
            | None -> [ ]
            | Some selection -> findInlineFragments selection.nodes
        | anythingElse ->
            [ ]
    )

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

        | GraphqlNode.InlineFragment inlineFragment ->
            let modifiedNodes = expandFragments inlineFragment.selection.nodes fragments
            let modifiedSelection = { inlineFragment.selection with nodes = modifiedNodes  }
            let modifiedFragment = { inlineFragment with selection = modifiedSelection }
            [ GraphqlNode.InlineFragment modifiedFragment ]

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
    | GraphqlFieldType.InterfaceRef _ -> true
    | GraphqlFieldType.UnionRef _ -> true
    | GraphqlFieldType.EnumRef _ -> false
    | GraphqlFieldType.Scalar _ -> false
    | GraphqlFieldType.InputObjectRef _ -> false
    | GraphqlFieldType.List innerField -> fieldCanExpand innerField
    | GraphqlFieldType.NonNull innerField -> fieldCanExpand innerField

let rec findFieldType (field: GraphqlFieldType) (schema: GraphqlSchema) =
    match field with
    | GraphqlFieldType.ObjectRef refType -> Schema.findTypeByName refType schema
    | GraphqlFieldType.InterfaceRef refType -> Schema.findTypeByName refType schema
    | GraphqlFieldType.UnionRef refType -> Schema.findTypeByName refType schema
    | GraphqlFieldType.EnumRef _ -> None
    | GraphqlFieldType.Scalar _ -> None
    | GraphqlFieldType.InputObjectRef _ -> None
    | GraphqlFieldType.List innerField -> findFieldType innerField schema
    | GraphqlFieldType.NonNull innerField -> findFieldType innerField schema

let rec formatVariableType = function
    | GraphqlVariableType.Ref name -> name
    | GraphqlVariableType.NonNull variable -> sprintf "%s!" (formatVariableType variable)
    | GraphqlVariableType.List variable -> sprintf "[%s]" (formatVariableType variable)

let rec formatFieldArgumentType = function
    | GraphqlFieldType.Scalar scalar ->
        match scalar with
        | GraphqlScalar.String -> "String"
        | GraphqlScalar.Int -> "Int"
        | GraphqlScalar.Boolean -> "Boolean"
        | GraphqlScalar.Float -> "Float"
        | GraphqlScalar.ID -> "ID"
        | GraphqlScalar.Custom customScalar -> customScalar

    | GraphqlFieldType.NonNull fieldType -> sprintf "%s!" (formatFieldArgumentType fieldType)
    | GraphqlFieldType.EnumRef enumName -> enumName
    | GraphqlFieldType.List fieldType -> sprintf "[%s]" (formatFieldArgumentType fieldType)
    | GraphqlFieldType.InputObjectRef objectName -> objectName
    | GraphqlFieldType.ObjectRef objectName -> objectName
    | GraphqlFieldType.InterfaceRef interfaceRef -> interfaceRef
    | GraphqlFieldType.UnionRef unionRef -> unionRef

let rec validateFieldArgument (fieldName:string) (argument: GraphqlFieldArgument) (argumentType: GraphqlFieldType) (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    match argumentType with
    | GraphqlFieldType.Scalar scalar ->
        match argument.value with
        | FieldArgumentValue.String _ when scalar = GraphqlScalar.String || scalar = GraphqlScalar.ID -> [ ]
        | FieldArgumentValue.Int _ when scalar = GraphqlScalar.Int -> [ ]
        | FieldArgumentValue.Boolean _  when scalar = GraphqlScalar.Boolean -> [ ]
        | FieldArgumentValue.Float _  when scalar = GraphqlScalar.Float -> [ ]
        | FieldArgumentValue.Null -> [ ]
        | FieldArgumentValue.EnumCase enumCase ->  [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, sprintf "Enum(%s)" enumCase) ]
        | FieldArgumentValue.List _ ->  [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "List") ]
        | FieldArgumentValue.Object fields -> [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Object") ]
        | FieldArgumentValue.Variable variableName ->
            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)

            match foundVariable with
            | None -> [ QueryError.UsedNonDeclaredVariable(fieldName, argument.name, variableName) ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.Ref "ID" when scalar = GraphqlScalar.ID -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "ID") when scalar = GraphqlScalar.ID -> [ ]
                | GraphqlVariableType.Ref "String" when scalar = GraphqlScalar.String || scalar = GraphqlScalar.ID -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "String") when scalar = GraphqlScalar.String || scalar = GraphqlScalar.ID -> [ ]
                | GraphqlVariableType.Ref "Int" when scalar = GraphqlScalar.Int -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Int") when scalar = GraphqlScalar.Int -> [ ]
                | GraphqlVariableType.Ref "Float" when scalar = GraphqlScalar.Float -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Float") when scalar = GraphqlScalar.Float -> [ ]
                | GraphqlVariableType.Ref "Boolean" when scalar = GraphqlScalar.Boolean -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Boolean") when scalar = GraphqlScalar.Boolean -> [ ]
                | GraphqlVariableType.Ref refName when scalar = GraphqlScalar.Custom refName -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref refName) when scalar = GraphqlScalar.Custom refName -> [ ]
                | otherVariableType ->
                    [
                        QueryError.ArgumentAndVariableTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType , variableName, formatVariableType otherVariableType)
                    ]
        | other ->
                [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | GraphqlFieldType.NonNull (GraphqlFieldType.Scalar scalar) ->
        match argument.value with
        | FieldArgumentValue.String _ when scalar = GraphqlScalar.String || scalar = GraphqlScalar.ID -> [ ]
        | FieldArgumentValue.Int _ when scalar = GraphqlScalar.Int -> [ ]
        | FieldArgumentValue.Boolean _  when scalar = GraphqlScalar.Boolean -> [ ]
        | FieldArgumentValue.Float _  when scalar = GraphqlScalar.Float -> [ ]
        | FieldArgumentValue.Null ->
            [
                QueryError.NullUsedForNonNullableType(fieldName, argument.name, formatFieldArgumentType argumentType)
            ]
        | FieldArgumentValue.EnumCase enumCase ->
            [
                QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, sprintf "Enum(%s)" enumCase)
            ]
        | FieldArgumentValue.List _ ->
            [
                QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "List")
            ]
        | FieldArgumentValue.Object fields ->
            [
                QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Object")
            ]
        | FieldArgumentValue.Variable variableName ->
            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)

            match foundVariable with
            | None -> [ QueryError.UsedNonDeclaredVariable(fieldName, argument.name, variableName) ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "ID") when scalar = GraphqlScalar.ID -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "String") when scalar = GraphqlScalar.String || scalar = GraphqlScalar.ID ->  [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Int") when scalar = GraphqlScalar.Int -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Float") when scalar = GraphqlScalar.Float ->  [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref "Boolean") when scalar = GraphqlScalar.Boolean -> [ ]
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref refName) when scalar = GraphqlScalar.Custom refName -> [ ]
                | otherVariableType ->
                    [
                        QueryError.ArgumentAndVariableTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType , variableName, formatVariableType otherVariableType)
                    ]

        | _ ->  [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | GraphqlFieldType.EnumRef enumTypeName ->
        match argument.value with
        | FieldArgumentValue.Null -> [ ]
        | FieldArgumentValue.EnumCase caseName ->
            let foundEnumType =
                schema.types
                |> List.choose (function
                    | GraphqlType.Enum enumType -> if enumType.name = enumTypeName then Some enumType else None
                    | _ -> None)
                |> List.tryHead

            match foundEnumType with
            | None -> [ ]
            | Some enumType ->
                enumType.values
                |> List.tryFind(fun enumCase -> enumCase.name = caseName)
                |> function
                    | None -> [ QueryError.UnknownEnumCase(fieldName, argument.name, formatFieldArgumentType argumentType, caseName) ]
                    | Some _ -> [ ]

        | FieldArgumentValue.Variable variableName ->

            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)

            match foundVariable with
            | None -> [ QueryError.UsedNonDeclaredVariable(fieldName, argument.name, variableName) ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref enumTypeName') when enumTypeName' = enumTypeName -> [ ]
                | GraphqlVariableType.Ref enumTypeName' when enumTypeName' = enumTypeName -> [ ]
                | otherVariableType -> [ QueryError.ArgumentAndVariableTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType , variableName, formatVariableType otherVariableType) ]

        | _ -> [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | GraphqlFieldType.NonNull (GraphqlFieldType.EnumRef enumTypeName) ->
        match argument.value with
        | FieldArgumentValue.EnumCase caseName ->
            let foundEnumType =
                schema.types
                |> List.choose (function
                    | GraphqlType.Enum enumType -> if enumType.name = enumTypeName then Some enumType else None
                    | _ -> None)
                |> List.tryHead

            match foundEnumType with
            | None -> [ ]
            | Some enumType ->
                enumType.values
                |> List.tryFind(fun enumCase -> enumCase.name = caseName)
                |> function
                    | None -> [ QueryError.UnknownEnumCase(fieldName, argument.name, formatFieldArgumentType argumentType, caseName) ]
                    | Some _ -> [ ]

        | FieldArgumentValue.Variable variableName ->

            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)

            match foundVariable with
            | None -> [ QueryError.UsedNonDeclaredVariable(fieldName, argument.name, variableName) ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.NonNull (GraphqlVariableType.Ref enumTypeName') when enumTypeName' = enumTypeName -> [ ]
                | otherVariableType -> [ QueryError.ArgumentAndVariableTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType , variableName, formatVariableType otherVariableType) ]


        | _ -> [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | GraphqlFieldType.InputObjectRef objectRef ->
        match argument.value with
        | FieldArgumentValue.Null -> [ ]
        | FieldArgumentValue.Object fields ->
            let foundObjectType =
                schema.types
                |> List.tryPick (function
                    | GraphqlType.InputObject objectDef when objectDef.name = objectRef -> Some objectDef
                    | _ -> None)

            match foundObjectType with
            | None -> [ ]
            | Some objectType ->
                [
                    // check provided fields
                    for (field, fieldValue) in fields do
                        let objectField =
                            objectType.fields
                            |> List.tryFind (fun f -> f.fieldName = field)

                        match objectField with
                        | None -> yield QueryError.UnknownInputObjectField(objectType.name, field)
                        | Some objectField ->
                            let inputArgument : GraphqlFieldArgument = {
                                name = field
                                value = fieldValue
                            }

                            yield! validateFieldArgument argument.name inputArgument objectField.fieldType variables schema

                    // check missing fields that should be provided
                    for objectField in objectType.fields do
                        match objectField.fieldType with
                        | GraphqlFieldType.NonNull _ ->
                            let foundField =
                                fields
                                |> List.tryFind(fun (field, _) -> field = objectField.fieldName)

                            match foundField with
                            | None ->
                                yield QueryError.MissingRequiredFieldFromInputObject(argument.name, objectType.name, objectField.fieldName)
                            | Some _ ->
                                ()
                        | _ ->
                            yield! [ ]
                ]
        | FieldArgumentValue.Variable variableName ->
            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)
            match foundVariable with
            | None ->
                [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.NonNull(GraphqlVariableType.Ref refName) when refName = objectRef ->
                    [ ]
                | GraphqlVariableType.Ref refName when refName = objectRef ->
                    [ ]
                | _ ->
                    [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]
        | _ ->
            [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | GraphqlFieldType.NonNull (GraphqlFieldType.InputObjectRef objectRef) ->
        match argument.value with
        | FieldArgumentValue.Object fields ->
            let foundObjectType =
                schema.types
                |> List.tryPick (function
                    | GraphqlType.InputObject objectDef when objectDef.name = objectRef -> Some objectDef
                    | _ -> None)

            match foundObjectType with
            | None -> [ ]
            | Some objectType ->
                [
                    // check provided fields
                    for (field, fieldValue) in fields do
                        let objectField =
                            objectType.fields
                            |> List.tryFind (fun f -> f.fieldName = field)

                        match objectField with
                        | None -> yield QueryError.UnknownInputObjectField(objectType.name, field)
                        | Some objectField ->
                            let inputArgument : GraphqlFieldArgument = {
                                name = field
                                value = fieldValue
                            }

                            yield! validateFieldArgument argument.name inputArgument objectField.fieldType variables schema

                    // check missing fields that should be provided
                    for objectField in objectType.fields do
                        match objectField.fieldType with
                        | GraphqlFieldType.NonNull _ ->
                            let foundField =
                                fields
                                |> List.tryFind(fun (field, _) -> field = objectField.fieldName)

                            match foundField with
                            | None ->
                                yield QueryError.MissingRequiredFieldFromInputObject(argument.name, objectType.name, objectField.fieldName)
                            | Some _ ->
                                ()
                        | _ ->
                            yield! [ ]
                ]
        | FieldArgumentValue.Variable variableName ->
            let foundVariable =
                variables
                |> List.tryFind (fun var -> var.variableName = variableName)
            match foundVariable with
            | None ->
                [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]
            | Some variable ->
                match variable.variableType with
                | GraphqlVariableType.NonNull(GraphqlVariableType.Ref refName) when refName = objectRef ->
                    [ ]
                | GraphqlVariableType.NonNull(GraphqlVariableType.Ref refName) ->
                    [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, refName) ]
                | _ ->
                    [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]
        | _ ->
            [ QueryError.ArgumentTypeMismatch(fieldName, argument.name, formatFieldArgumentType argumentType, "Unknown") ]

    | _ -> [ ]

let containsTypeName (set: SelectionSet) =
    set.nodes
    |> List.choose (function
        | GraphqlNode.Field field -> Some field.name
        | _ -> None)
    |> List.contains "__typename"

let rec validateUnion (unionCase: string option) (parentField: string) (selection: SelectionSet)  (graphqlType: GraphqlUnion) (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    let fragments = findInlineFragments selection.nodes

    let missingTypeNameErrors =
        [
            for fragment in fragments do
                if not (containsTypeName fragment.selection)
                then yield QueryError.MissingTypeNameField(graphqlType.name, fragment.typeCondition, parentField)
        ]

    let unknownSubtypeErrors =
        fragments
        |> List.filter (fun fragment -> not (List.contains fragment.typeCondition graphqlType.possibleTypes))
        |> List.map (fun fragment -> QueryError.UnknownSubType(graphqlType.name, fragment.typeCondition, parentField))

    let subTypeErrors =
        fragments
        |> List.filter (fun fragment -> List.contains fragment.typeCondition graphqlType.possibleTypes)
        |> List.collect (fun fragment ->
            match Schema.findTypeByName fragment.typeCondition schema with
            | Some (GraphqlType.Object objectDef) ->
                validateFields parentField fragment.selection objectDef variables schema
            | Some (GraphqlType.Interface interfaceDef) ->
                validateInterface parentField fragment.selection interfaceDef variables schema
            | Some (GraphqlType.Union unionDef) ->
                if not (containsTypeName fragment.selection)
                then [ QueryError.MissingTypeNameField(fragment.typeCondition, graphqlType.name, parentField) ]
                else validateUnion (Some fragment.typeCondition) parentField fragment.selection unionDef variables schema
            | _ ->
                [ ]
        )

    missingTypeNameErrors
    |> List.append subTypeErrors
    |> List.append unknownSubtypeErrors

and validateInterface (parentField: string) (selection: SelectionSet) (graphqlType: GraphqlInterface) (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    let fragments = findInlineFragments selection.nodes

    // handle interface fields as if it was an object
    // change the interface into an object definition
    let graphqlObject = {
        name = graphqlType.name
        description = graphqlType.description
        fields = graphqlType.fields
    }

    let missingTypeNameErrors =
        if not (containsTypeName selection) && not (List.isEmpty fragments)
        then [ QueryError.MissingTypeNameOnInterface(graphqlType.name, parentField) ]
        else [ ]

    let baseFieldErrors = validateFields parentField selection graphqlObject variables schema

    let unknownSubtypeErrors =
        fragments
        |> List.filter (fun fragment -> not (List.contains fragment.typeCondition graphqlType.possibleTypes))
        |> List.map (fun fragment -> QueryError.UnknownSubType(graphqlType.name, fragment.typeCondition, parentField))

    let subTypeErrors =
        fragments
        |> List.filter (fun fragment -> List.contains fragment.typeCondition graphqlType.possibleTypes)
        |> List.collect (fun fragment ->
            match Schema.findTypeByName fragment.typeCondition schema with
            | Some (GraphqlType.Object objectDef) ->
                if not (containsTypeName fragment.selection)
                then [ QueryError.MissingTypeNameField(fragment.typeCondition, graphqlType.name, parentField) ]
                else validateFields parentField fragment.selection objectDef variables schema
            | Some (GraphqlType.Interface interfaceDef) ->
                validateInterface parentField fragment.selection interfaceDef variables schema
            | Some (GraphqlType.Union unionDef) ->
                if not (containsTypeName fragment.selection)
                then [ QueryError.MissingTypeNameField(fragment.typeCondition, graphqlType.name, parentField) ]
                else validateUnion (Some fragment.typeCondition) parentField fragment.selection unionDef variables schema
            | _ ->
                [ ]
        )

    let allErrors =
        missingTypeNameErrors
        |> List.append baseFieldErrors
        |> List.append unknownSubtypeErrors
        |> List.append subTypeErrors

    allErrors

and validateFields (parentField: string) (selection: SelectionSet) (graphqlType: GraphqlObject) (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    let fields =
        selection.nodes
        |> List.choose (function
            | GraphqlNode.Field field -> Some field
            | _ -> None)

    let invalidInlineFragments =
        match Schema.findTypeByName graphqlType.name schema with
        | Some (GraphqlType.Object objectDef) ->
            // only when the input is an actual Object, validate the inline fragments
            selection.nodes
            |> List.choose (function
                | GraphqlNode.InlineFragment fragment -> Some fragment
                | _ -> None)
        | _ ->
            [ ]

    let unknownFields =
        fields
        |> List.filter (fun field ->
            match graphqlType.fields |> List.tryFind (fun fieldType -> fieldType.fieldName = field.name) with
            | None -> true
            | Some field -> false)

    [
        for fragment in invalidInlineFragments
            do yield QueryError.InvalidInlineFragment(graphqlType.name, fragment.typeCondition, parentField)

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
                                | Some (GraphqlType.Object possibleExpansion) ->
                                    validateFields selectedField.name selection possibleExpansion variables schema
                                | Some (GraphqlType.Interface interfaceExpansion) ->
                                    validateInterface selectedField.name selection interfaceExpansion variables schema
                                | Some (GraphqlType.Union unionExpansion) ->
                                    validateUnion None selectedField.name selection unionExpansion variables schema
                                | _ ->
                                    [ ]
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

                        let typeMismatchArguments =
                            let allowedArguments =
                                selectedField.arguments
                                |> List.filter (fun arg -> List.contains arg.name allowedArguments)

                            [
                                for arg in allowedArguments do
                                    let argType =
                                        graphqlField.args
                                        |> List.tryFind (fun (fieldArgName, fieldArgType) -> fieldArgName = arg.name)
                                        |> Option.map snd

                                    match argType with
                                    | None -> ()
                                    | Some argType -> yield! validateFieldArgument selectedField.name arg argType variables schema
                            ]


                        let allErrors = [
                            yield! selectionErrors
                            yield! unknownArguments
                            yield! missingRequiredArguments
                            yield! typeMismatchArguments
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
                yield! [ ]
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
            let fieldErrors = validateFields "query" query.selectionSet queryType query.variables schema
            let inputVariableErrors = validateInputVariables query.variables schema
            let queryErrors = [ yield! fieldErrors; yield! inputVariableErrors ]
            match queryErrors with
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors

    | Some (GraphqlOperation.Mutation mutation) ->
        match Schema.findMutation schema with
        | None -> ValidationResult.SchemaDoesNotHaveMutationType
        | Some mutationType ->
            let fieldErrors = validateFields "mutation" mutation.selectionSet mutationType mutation.variables schema
            let inputVariableErrors = validateInputVariables mutation.variables schema
            let queryErrors = [ yield! fieldErrors; yield! inputVariableErrors ]
            match queryErrors with
            | [ ] -> ValidationResult.Success
            | errors -> ValidationResult.QueryErrors errors