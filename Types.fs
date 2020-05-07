module Snowflake.Types

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