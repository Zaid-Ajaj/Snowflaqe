module rec Snowflaqe.Types

open GraphQLParser.AST

[<RequireQualifiedAccess>]
type GraphqlScalar =
    | ID
    | Int
    | String
    | Float
    | Boolean
    | Custom of name:string

type GraphqlEnumValue =  {
    name : string
    description : string option
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
    | NonNull of GraphqlFieldType
    | List of GraphqlFieldType

type GraphqlField = {
    fieldName : string
    fieldType : GraphqlFieldType
    args :  (string * GraphqlFieldType) list
}

[<RequireQualifiedAccess>]
type GraphqlVariableType = 
    | Ref of referencedType:string
    | NonNull of GraphqlVariableType
    | List of GraphqlVariableType

type GraphqlVariable = {
    name: string 
    variableType: GraphqlVariableType
}

type GraphqlInputField = {
    fieldName : string
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

[<RequireQualifiedAccess>]
type GraphqlType =
    | Scalar of GraphqlScalar
    | Object of GraphqlObject
    | InputObject of GraphqlInputObject
    | Enum of GraphqlEnum

type GraphqlSchema = { 
    types : GraphqlType list
    queryType : string option 
    mutationType : string option
}

type SelectionSet = {
    nodes : GraphqlNode list
    location : GraphQLLocation
}

type GraphqlFieldSelection = {
    name : string
    selectionSet : SelectionSet option
    directives: GraphQLDirective list
    arguments : GraphQLArgument list
    location : GraphQLLocation
}

type GraphqlFragmentDefinition = {
    name : string 
    typeDef : string option
    selectionSet : SelectionSet option 
    directives: GraphQLDirective list
    location : GraphQLLocation
}

[<RequireQualifiedAccess>]
type GraphqlNode =
    | Name of GraphQLName * GraphQLLocation
    | Field of GraphqlFieldSelection
    | SelectionSet of SelectionSet
    | FragmentSpread of GraphQLFragmentSpread
    | FragmentDefinition of GraphqlFragmentDefinition
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
    | UnknownField of fieldName:string * typeName:string
    | ExpandedScalarField of fieldName:string * typeName:string
    | UnknownInputVariable of variableName:string * variableType:string

[<RequireQualifiedAccess>]
type ValidationResult = 
    | NoQueryOrMutationProvided
    | SchemaDoesNotHaveQueryType
    | SchemaDoesNotHaveMutationType
    | QueryErrors of QueryError list
    | Success