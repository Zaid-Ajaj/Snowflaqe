module rec Snowflake.Types

open GraphQL
open GraphQLParser.AST

[<RequireQualifiedAccess>]
type GraphqlScalar =
    | Int
    | String
    | Float
    | Bool
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
    | EnumRef of referencedEnum:string
    | NonNull of GraphqlFieldType
    | List of GraphqlFieldType

type GraphqlField = {
    fieldName : string
    fieldType : GraphqlFieldType
    args :  (string * GraphqlFieldType) list
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
    | Enum of GraphqlEnum

type GraphqlSchema = { types : GraphqlType list }

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
    typeDef : string
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
    variables : GraphQLVariableDefinition list
    selectionSet : SelectionSet
}

type GraphqlMutation = {
    name : string option
    directives : GraphQLDirective list
    variables : GraphQLVariableDefinition list
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
type ValidationResult = 
    | NoQueryOrMutationProvided
    | Success