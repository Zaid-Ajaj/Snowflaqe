module rec Snowflaqe.Types

open GraphQLParser.AST

[<RequireQualifiedAccess>]
type OutputTarget =
    | Fable
    | FSharp
    | Shared

[<RequireQualifiedAccess>]
type AsyncReturnType =
    | Async
    | Task

[<RequireQualifiedAccess>]
type GraphqlScalar =
    | ID
    | Int
    | String
    | Float
    | Boolean
    | Custom of name:string

    with
        member scalar.IsCustomType() =
            match scalar with
            | GraphqlScalar.Custom _ -> true
            | _ -> false

type GraphqlEnumValue =  {
    name : string
    description : string option
    deprecated : bool
}

type GraphqlEnum = {
    name : string
    description : string option
    values : GraphqlEnumValue list
}

[<RequireQualifiedAccess>]
type GraphqlFieldType =
    | Scalar of fieldType:GraphqlScalar
    | ObjectRef of referencedObject:string
    | InputObjectRef of referencedObject:string
    | EnumRef of referencedEnum:string
    | InterfaceRef of referencedInterface:string
    | UnionRef of referencedUnion:string
    | NonNull of GraphqlFieldType
    | List of GraphqlFieldType

type GraphqlField = {
    fieldName : string
    fieldType : GraphqlFieldType
    description : string option
    args :  (string * GraphqlFieldType) list
}

[<RequireQualifiedAccess>]
type GraphqlVariableType =
    | Ref of referencedType:string
    | NonNull of GraphqlVariableType
    | List of GraphqlVariableType

type GraphqlVariable = {
    variableName: string
    variableType: GraphqlVariableType
}

type GraphqlInputField = {
    fieldName : string
    description : string option
    deprecated : bool
    fieldType : GraphqlFieldType
}

type GraphqlInputObject = {
    name: string
    description: string option
    fields : GraphqlInputField  list
}

type GraphqlObject = {
    name: string
    description: string option
    fields : GraphqlField  list
}

type GraphqlInterface = {
    name: string
    description: string option
    fields : GraphqlField  list
    possibleTypes: string list
}

type GraphqlUnion = {
    name: string
    description: string option
    possibleTypes: string list
}

[<RequireQualifiedAccess>]
type GraphqlType =
    | Scalar of GraphqlScalar
    | Object of GraphqlObject
    | InputObject of GraphqlInputObject
    | Enum of GraphqlEnum
    | Interface of GraphqlInterface
    | Union of GraphqlUnion

type GraphqlSchema = {
    types : GraphqlType list
    queryType : string option
    mutationType : string option
}

type SelectionSet = {
    nodes : GraphqlNode list
    location : GraphQLLocation
}

[<RequireQualifiedAccess>]
type FieldArgumentValue =
    | Variable of string
    | Null
    | Int of int
    | String of string
    | Boolean of bool
    | Float of float
    | EnumCase of string
    | List of FieldArgumentValue list
    | Object of (string * FieldArgumentValue) list

type GraphqlFieldArgument = {
    name: string
    value: FieldArgumentValue
}

type GraphqlFieldSelection = {
    alias: string option
    name : string
    selectionSet : SelectionSet option
    directives: GraphQLDirective list
    arguments : GraphqlFieldArgument list
    location : GraphQLLocation
}

type GraphqlFragmentDefinition = {
    name : string
    typeDef : string option
    selectionSet : SelectionSet option
    directives: GraphQLDirective list
    location : GraphQLLocation
}

type GraphqlInlineFragment = {
    typeCondition: string
    selection: SelectionSet
}

[<RequireQualifiedAccess>]
type GraphqlNode =
    | Name of GraphQLName * GraphQLLocation
    | Field of GraphqlFieldSelection
    | SelectionSet of SelectionSet
    | FragmentSpread of GraphQLFragmentSpread
    | FragmentDefinition of GraphqlFragmentDefinition
    | InlineFragment of GraphqlInlineFragment
    | Query of GraphqlQuery
    | Mutation of GraphqlMutation

type GraphqlQuery = {
    name : string option
    directives : GraphQLDirective list
    variables : GraphqlVariable list
    selectionSet : SelectionSet
}

type GraphqlMutation = {
    name : string option
    directives : GraphQLDirective list
    variables : GraphqlVariable list
    selectionSet : SelectionSet
}

[<RequireQualifiedAccess>]
type GraphqlOperation =
    | Query of GraphqlQuery
    | Mutation of GraphqlMutation

type GraphqlDocument = {
    nodes : GraphqlNode list
}

[<RequireQualifiedAccess>]
type QueryError =
    | UnknownField of fieldName:string * parentField:string * typeName:string
    | ExpandedScalarField of fieldName:string * parentField:string * typeName:string
    | UnknownInputVariable of variableName:string * variableType:string
    | UnknownFieldArgument of argumentName:string * parentField:string * typeName:string
    | MissingRequiredArgument of argumentName:string * parentField:string * typeName:string
    | UsedNonDeclaredVariable of fieldName:string * argumentName:string * variableName:string
    | NullableVariableUsedWithNonNullableArgument of fieldName:string * argumentName:string * variableName:string
    | NullUsedForNonNullableType of fieldName:string * argumentName:string * argumentType:string
    | ArgumentTypeMismatch of fieldName:string * argumentName:string * argumentType: string * providedType:string
    | ArgumentAndVariableTypeMismatch of fieldName:string * argumentName:string * argumentType:string * variableName:string * variableType:string
    | UnknownEnumCase of fieldName:string * argumentName:string * argumentType:string * enumCase:string
    | UnknownInputObjectField of inputObjectName:string * unknownFieldName:string
    | MissingRequiredFieldFromInputObject of inputObjectName:string * objectType:string * requiredFieldName:string
    | UnknownSubType of interfaceName:string * subTypeName: string * parentField: string
    | MissingTypeNameOnInterface of interfaceName:string * parentField:string
    | MissingTypeNameField of interfaceName:string * subTypeName: string * parentField: string
    | InvalidInlineFragment of objectType:string * fragmentName:string * parentField: string

[<RequireQualifiedAccess>]
type ValidationResult =
    | NoQueryOrMutationProvided
    | SchemaDoesNotHaveQueryType
    | SchemaDoesNotHaveMutationType
    | QueryErrors of QueryError list
    | Success