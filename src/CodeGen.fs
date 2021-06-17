[<RequireQualifiedAccess>]
module Snowflaqe.CodeGen

open System
open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions
open FsAst
open Fantomas
open Snowflaqe.Types
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.XmlDoc
open Newtonsoft.Json.Linq
open GraphQLParser.AST
open LinqToXmlExtensions
open System.Xml
open System.Xml.Linq
open StringBuffer
open Fantomas.FormatConfig
open System.Text

let compiledName (name: string) = SynAttribute.Create("CompiledName", name)

let capitalize (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

let normalizeName (unionCase: string) =
    if not(unionCase.Contains "_") then
        capitalize unionCase
    else
        unionCase.Split [| '_'; '-' |]
        |> Array.filter String.isNotNullOrEmpty
        |> Array.map capitalize
        |> String.concat ""

let capitalizeEnum (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1)).ToLowerInvariant()

let normalizeEnumName (unionCase: string) =

    let allUppercase =
        unionCase
        |> Seq.filter Char.IsLetter
        |> Seq.forall Char.IsUpper

    let allLowecase =
        unionCase
        |> Seq.filter Char.IsLetter
        |> Seq.forall Char.IsLower

    if (allUppercase || allLowecase) && not (unionCase.Contains "_") then
        capitalizeEnum unionCase
    elif not(unionCase.Contains "_") then
        // enumValue -> EnumValue
        // EnumValue -> EnumValue
        let firstNonUpperparts =
            unionCase
            |> Seq.takeWhile (fun token -> not (Char.IsUpper(token)))
            |> Seq.map string
            |> String.concat ""
            |> capitalizeEnum

        let splitByUpperCase =
            Regex.Matches(unionCase, @"([A-Z][a-z]+([0-9]?)+)")
            |> Seq.cast<Match>
            |> Seq.map (fun matched -> capitalizeEnum matched.Value)
            |> String.concat ""

        firstNonUpperparts + splitByUpperCase
    else
        // ENUM_VALUE -> EnumValue
        unionCase.Split [| '_'; '-' |]
        |> Array.filter String.isNotNullOrEmpty
        |> Array.map capitalizeEnum
        |> String.concat ""

let normalizeModuleName (name: string) =
    name.Replace(" ", "").Split [| '_'; '-' |]
    |> Array.filter String.isNotNullOrEmpty
    |> Array.map capitalize
    |> String.concat ""

type SynAttribute with
    static member Create(idents: string list) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Unit, range0)
           Range = range0
           Target = None
           TypeName = LongIdentWithDots(List.map Ident.Create idents, [ ])
        }

let createEnumType (enumType: GraphqlEnum) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [
            SynAttributeList.Create [
                SynAttribute.Create [ "Fable"; "Core"; "StringEnum" ]
                SynAttribute.RequireQualifiedAccess()
            ]
        ]

        Id = [ Ident.Create enumType.name ]
        XmlDoc = PreXmlDoc.Create enumType.description
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let values = enumType.values |> List.filter (fun enumValue -> not enumValue.deprecated)

    let enumRepresentation = SynTypeDefnSimpleReprUnionRcd.Create([
        for value in values ->
            let attrs = [ SynAttributeList.Create(compiledName value.name) ]
            let docs = PreXmlDoc.Create value.description
            SynUnionCase.UnionCase(attrs, Ident.Create (normalizeEnumName value.name), SynUnionCaseType.UnionCaseFields [], docs, None, range0)
    ])

    let simpleType = SynTypeDefnSimpleReprRcd.Union(enumRepresentation)
    SynModuleDecl.CreateSimpleType(info, simpleType)

let optionOfSystemDot id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "System"; inner ]) ]

let listOfSystemDot id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "list" ]) [ (LongIdentWithDots.Create [ "System"; inner ]) ]

let systemDot id inner =
    SynFieldRcd.Create(id, LongIdentWithDots([ Ident.Create "System"; Ident.Create inner ], []))


type SynType with
    static member Create(name: string) = SynType.CreateLongIdent name

    static member Option(inner) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Dictionary(key, value) =
        SynType.App(
            typeName=SynType.LongIdent(LongIdentWithDots.Create [ "System"; "Collections"; "Generic"; "Dictionary" ]),
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Option(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member List(inner) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member List(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member DateTimeOffset() =
        SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTimeOffset" ])

    static member DateTime() =
        SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTime" ])

    static member Int() =
        SynType.Create "int"

    static member String() =
        SynType.Create "string"

    static member Bool() =
        SynType.Create "bool"

    static member Float() =
        SynType.Create "float"

    static member Decimal() =
        SynType.Create "decimal"

type SynFieldRcd with
    static member Create(name: string, fieldType: SynType) =
        {
            Access = None
            Attributes = [ ]
            Id = Some (Ident.Create name)
            IsMutable = false
            IsStatic = false
            Range = range0
            Type = fieldType
            XmlDoc= PreXmlDoc.Empty
        }

    static member Create(name: string, fieldType: string) =
        {
            Access = None
            Attributes = [ ]
            Id = Some (Ident.Create name)
            IsMutable = false
            IsStatic = false
            Range = range0
            Type = SynType.Create fieldType
            XmlDoc= PreXmlDoc.Empty
        }

let rec createFSharpType (name: string option) (graphqlType: GraphqlFieldType) =
    match graphqlType with
    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar scalar) ->
        match scalar with
        | GraphqlScalar.Int -> SynType.Int()
        | GraphqlScalar.String -> SynType.String()
        | GraphqlScalar.Boolean -> SynType.Bool()
        | GraphqlScalar.Float -> SynType.Float()
        | GraphqlScalar.ID -> SynType.String()
        | GraphqlScalar.Custom "Decimal" -> SynType.Decimal()
        | GraphqlScalar.Custom "Numeric" -> SynType.Decimal()
        | GraphqlScalar.Custom "BigFloat" -> SynType.Decimal()
        | GraphqlScalar.Custom "DateTimeOffset" -> SynType.DateTimeOffset()
        | GraphqlScalar.Custom "Datetime" -> SynType.DateTimeOffset()
        | GraphqlScalar.Custom "timestamptz" -> SynType.DateTimeOffset()
        | GraphqlScalar.Custom "DateTime" -> SynType.DateTime()
        | GraphqlScalar.Custom "Date" -> SynType.DateTime()
        | GraphqlScalar.Custom custom -> SynType.String()

    | GraphqlFieldType.NonNull(GraphqlFieldType.List innerType) ->
        let innerFSharpType = createFSharpType name innerType
        SynType.List(innerFSharpType)

    | GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumType) ->
        SynType.Create enumType

    | GraphqlFieldType.NonNull(GraphqlFieldType.InputObjectRef objectRef) ->
        SynType.Create objectRef

    | GraphqlFieldType.NonNull(GraphqlFieldType.ObjectRef objectRef) ->
        SynType.Create (Option.defaultValue objectRef name)

    | GraphqlFieldType.NonNull(GraphqlFieldType.InterfaceRef interfaceRef) ->
        SynType.Create (Option.defaultValue interfaceRef name)

    | GraphqlFieldType.NonNull(GraphqlFieldType.UnionRef unionRef) ->
        SynType.Create (Option.defaultValue unionRef name)

    | GraphqlFieldType.Scalar scalar ->
        let innerFSharpType =
            match scalar with
            | GraphqlScalar.Int -> SynType.Int()
            | GraphqlScalar.String -> SynType.String()
            | GraphqlScalar.Boolean -> SynType.Bool()
            | GraphqlScalar.Float -> SynType.Float()
            | GraphqlScalar.ID -> SynType.String()
            | GraphqlScalar.Custom "Decimal" -> SynType.Decimal()
            | GraphqlScalar.Custom "Numeric" -> SynType.Decimal()
            | GraphqlScalar.Custom "BigFloat" -> SynType.Decimal()
            | GraphqlScalar.Custom "Datetime" -> SynType.DateTimeOffset()
            | GraphqlScalar.Custom "DateTimeOffset" -> SynType.DateTimeOffset()
            | GraphqlScalar.Custom "timestamptz" -> SynType.DateTimeOffset()
            | GraphqlScalar.Custom "DateTime" -> SynType.DateTime()
            | GraphqlScalar.Custom "Date" -> SynType.String()
            | GraphqlScalar.Custom custom -> SynType.String()

        SynType.Option(innerFSharpType)

    | GraphqlFieldType.List innerType ->
        let innerFSharpType = createFSharpType name innerType
        SynType.Option(SynType.List(innerFSharpType))

    | GraphqlFieldType.EnumRef enumType ->
        SynType.Option(SynType.Create enumType)

    | GraphqlFieldType.InputObjectRef objectRef ->
        SynType.Option(SynType.Create objectRef)

    | GraphqlFieldType.ObjectRef objectRef ->
        SynType.Option(SynType.Create (Option.defaultValue objectRef name))

    | GraphqlFieldType.InterfaceRef interfaceRef ->
        SynType.Option(SynType.Create (Option.defaultValue interfaceRef name))

    | GraphqlFieldType.UnionRef unionRef ->
        SynType.Option(SynType.Create (Option.defaultValue unionRef name))

    | GraphqlFieldType.NonNull(inner) ->
        createFSharpType name inner

let createInputRecord (input: GraphqlInputObject) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create input.name ]
        XmlDoc = PreXmlDoc.Create input.description
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let fields = input.fields |> List.filter (fun field -> not field.deprecated)

    let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
        for field in fields ->
            let recordFieldType = createFSharpType None field.fieldType
            let recordField = SynFieldRcd.Create(field.fieldName, recordFieldType)
            { recordField with XmlDoc = PreXmlDoc.Create field.description }
    ]

    let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation

    SynModuleDecl.CreateSimpleType(info, simpleType)



let createGlobalTypes (schema: GraphqlSchema) =
    let enums =
        schema.types
        |> List.choose (function
            | GraphqlType.Enum enumType when not (enumType.name.StartsWith "__")  -> Some enumType
            | _ -> None)
        |> List.map createEnumType

    let inputs =
        schema.types
        |> List.choose (function
            | GraphqlType.InputObject objectDefn when not (objectDefn.name.StartsWith "__") -> Some objectDefn
            | _ -> None)
        |> List.map createInputRecord

    List.append enums inputs


let nextTick (name: string) (visited: ResizeArray<string>) =
    if not (visited.Contains name) then
        name
    else
    visited
    |> Seq.toList
    |> List.filter (fun visitedName -> visitedName.StartsWith name)
    |> List.map (fun visitedName -> visitedName.Replace(name, ""))
    |> List.choose(fun rest ->
        match Int32.TryParse rest with
        | true, n -> Some n
        | _ -> None)
    |> function
        | [ ] -> name + "1"
        | ns -> name + (string (List.max ns + 1))

let findNextTypeName fieldName objectName (selections: string list) (visitedTypes: ResizeArray<string>) =
    let nestedSelectionType =
        selections
        |> List.map normalizeName
        |> String.concat "And"

    if not (visitedTypes.Contains objectName) then
        objectName
    elif not (visitedTypes.Contains (normalizeName fieldName)) then
        normalizeName fieldName
    elif not (visitedTypes.Contains nestedSelectionType) && selections.Length <= 3 && selections.Length > 1 then
        nestedSelectionType
    elif not (visitedTypes.Contains (normalizeName fieldName + "From" + objectName)) then
        normalizeName fieldName + "From" + objectName
    else
        nextTick (normalizeName fieldName + "From" + objectName) visitedTypes

let rec extractTypeName = function
    | GraphqlFieldType.Scalar scalar ->
        match scalar with
        | GraphqlScalar.Int -> "Int"
        | GraphqlScalar.Boolean -> "Boolean"
        | GraphqlScalar.String -> "String"
        | GraphqlScalar.Float -> "Float"
        | GraphqlScalar.ID -> "ID"
        | GraphqlScalar.Custom custom -> custom

    | GraphqlFieldType.ObjectRef objectRef -> objectRef
    | GraphqlFieldType.InterfaceRef interfaceRef -> interfaceRef
    | GraphqlFieldType.UnionRef unionRef -> unionRef
    | GraphqlFieldType.EnumRef enumRef -> enumRef
    | GraphqlFieldType.InputObjectRef objectRef -> objectRef

    | GraphqlFieldType.NonNull fieldType ->
        extractTypeName fieldType

    | GraphqlFieldType.List fieldType ->
        extractTypeName fieldType

let rec generateFields (typeName: string) (description: string option) (selections: SelectionSet) (schemaType: GraphqlObject) (schema: GraphqlSchema) (visitedTypes: ResizeArray<string>) (types: Dictionary<string,SynModuleDecl>)  =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create typeName ]
        XmlDoc = PreXmlDoc.Create description
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let selectedFields =
        selections.nodes
        |> List.choose (function
            | GraphqlNode.Field field -> Some field
            | _ -> None)

    let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
        for field in selectedFields do
            let fieldTypeInfo =
                schemaType.fields
                |> List.tryFind (fun fieldType' -> fieldType'.fieldName = field.name)

            match fieldTypeInfo with
            | None ->
                ()
            | Some fieldInfo when Query.fieldCanExpand fieldInfo.fieldType ->
                let fieldName = field.alias |> Option.defaultValue field.name
                let fieldTypeName = extractTypeName fieldInfo.fieldType
                let nestedFieldType = Schema.findTypeByName fieldTypeName schema

                match nestedFieldType, field.selectionSet with
                | Some (GraphqlType.Object objectDef), Some nestedSelectionSet ->
                    let nestedFields =
                        nestedSelectionSet.nodes
                        |> List.choose (function
                            | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                            | _ -> None)

                    let typeName = findNextTypeName fieldName fieldTypeName nestedFields visitedTypes

                    visitedTypes.Add(typeName)
                    let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                    types.Add(typeName, nestedType)
                    let recordField = SynFieldRcd.Create(fieldName, createFSharpType (Some typeName) fieldInfo.fieldType)
                    { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }

                | Some (GraphqlType.Interface interfaceDef), Some nestedSelectionSet ->
                    let inlineFragments = Query.findInlineFragments nestedSelectionSet.nodes
                    let interfaceFields =
                        nestedSelectionSet.nodes
                        |> List.choose (function
                            | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                            | _ -> None)

                    match inlineFragments, interfaceFields with
                    | [ ], nestedFields ->
                        // no inline fragments means to treat the interface def like another object def
                        let typeName = findNextTypeName fieldName fieldTypeName nestedFields visitedTypes
                        // do as if the interface was an object
                        let objectTypeDef = {
                            name = interfaceDef.name
                            description = interfaceDef.description
                            fields = interfaceDef.fields
                        }

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectTypeDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        let recordField = SynFieldRcd.Create(fieldName, createFSharpType (Some typeName) fieldInfo.fieldType)
                        { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }

                    // There were inline fragments without selection fields
                    // Create a discriminated union from it
                    | fragments, [ ] ->
                        let interfaceTypeName = fieldTypeName
                        let typeName = findNextTypeName fieldName interfaceTypeName [ ] visitedTypes
                        visitedTypes.Add(typeName)
                        let localUnionCases = Dictionary<string, string>()
                        for fragment in fragments do
                            match Schema.findTypeByName fragment.typeCondition schema with
                            | Some (GraphqlType.Object objectDef) ->
                                let caseName = findNextTypeName fieldName fragment.typeCondition [ ] visitedTypes
                                visitedTypes.Add caseName
                                let nestedType = generateFields objectDef.name objectDef.description fragment.selection objectDef schema visitedTypes types
                                types.Add(caseName, nestedType)
                                localUnionCases.Add(fragment.typeCondition, caseName)

                            | _ ->
                                ()

                        let interfaceUnions = SynTypeDefnSimpleReprUnionRcd.Create [
                            for pair in localUnionCases  ->
                                let unionCaseType = SynUnionCaseType.Create([ SynFieldRcd.Create(pair.Key.ToLowerInvariant(), pair.Value) ])
                                SynUnionCaseRcd.Create(Ident.Create (capitalize pair.Key), unionCaseType)
                        ]

                        let interfaceUnionsInfo : SynComponentInfoRcd = {
                            Access = None
                            Attributes = [
                                SynAttributeList.Create [
                                    SynAttribute.RequireQualifiedAccess()
                                ]
                            ]
                            Id = [ Ident.Create typeName ]
                            XmlDoc = PreXmlDoc.Create description
                            Parameters = [ ]
                            Constraints = [ ]
                            PreferPostfix = false
                            Range = range0
                        }

                        let simpleType = SynTypeDefnSimpleReprRcd.Union(interfaceUnions)
                        let unionType = SynModuleDecl.CreateSimpleType(interfaceUnionsInfo, simpleType)

                        types.Add(typeName, unionType)

                        let recordField = SynFieldRcd.Create(fieldName, createFSharpType (Some typeName) fieldInfo.fieldType)
                        { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }

                    // There were inline fragments AND selection fields
                    // Create a discriminated union from it including an extra case for the base fields
                    // all unions have the common base fields as well
                    | fragments, fields ->
                        let interfaceTypeName = interfaceDef.name

                        let interfaceFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> Some field
                                | _ -> None)

                        let includedImplementers =
                            fragments
                            |> List.map (fun fragment -> fragment.typeCondition)

                        let notIncludedImplementers =
                            interfaceDef.possibleTypes
                            |> List.filter (fun subType -> not (List.contains subType includedImplementers))

                        let extraInterfaceFragments = [
                            for subType in notIncludedImplementers ->
                                {
                                    typeCondition = subType
                                    selection = {
                                        location = GraphQLLocation()
                                        nodes = [ for field in interfaceFields -> GraphqlNode.Field field ]
                                    }
                                }
                        ]

                        let modifiedFragments = extraInterfaceFragments @ [
                            for fragment in fragments ->
                                let modifiedSelection =
                                    { fragment.selection
                                        with nodes = fragment.selection.nodes
                                                     |> List.append [ for field in interfaceFields -> GraphqlNode.Field field ]
                                                     |> List.distinctBy (function
                                                        | GraphqlNode.Field field -> (field.name, field.alias)
                                                        | _ -> Guid.NewGuid().ToString(), None) }

                                { fragment with selection = modifiedSelection }
                        ]

                        let typeName = findNextTypeName fieldName interfaceTypeName [ ] visitedTypes
                        visitedTypes.Add(typeName)
                        let localUnionCases = Dictionary<string, string>()
                        for fragment in modifiedFragments do
                            match Schema.findTypeByName fragment.typeCondition schema with
                            | Some (GraphqlType.Object objectDef) ->
                                let caseName = findNextTypeName fieldName fragment.typeCondition fields visitedTypes
                                visitedTypes.Add caseName
                                let nestedType = generateFields caseName objectDef.description fragment.selection objectDef schema visitedTypes types
                                types.Add(caseName, nestedType)
                                localUnionCases.Add(fragment.typeCondition, caseName)

                            | Some (GraphqlType.Interface interfaceDef) ->
                                let objectDef = {
                                    name = interfaceDef.name
                                    description = interfaceDef.description
                                    fields = interfaceDef.fields
                                }

                                let caseName = findNextTypeName fieldName fragment.typeCondition fields visitedTypes
                                visitedTypes.Add caseName
                                let nestedType = generateFields caseName objectDef.description fragment.selection objectDef schema visitedTypes types
                                types.Add(caseName, nestedType)
                                localUnionCases.Add(interfaceDef.name, caseName)
                            | _ ->
                                ()

                        let interfaceUnions = SynTypeDefnSimpleReprUnionRcd.Create [
                            for pair in localUnionCases  ->
                                let unionCaseType = SynUnionCaseType.Create([ SynFieldRcd.Create(pair.Key.ToLowerInvariant(), pair.Value) ])
                                SynUnionCaseRcd.Create(Ident.Create (capitalize pair.Key), unionCaseType)
                        ]

                        let interfaceUnionsInfo : SynComponentInfoRcd = {
                            Access = None
                            Attributes = [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess() ] ]
                            Id = [ Ident.Create typeName ]
                            XmlDoc = PreXmlDoc.Create description
                            Parameters = [ ]
                            Constraints = [ ]
                            PreferPostfix = false
                            Range = range0
                        }

                        let simpleType = SynTypeDefnSimpleReprRcd.Union(interfaceUnions)
                        let unionType = SynModuleDecl.CreateSimpleType(interfaceUnionsInfo, simpleType)

                        types.Add(typeName, unionType)

                        let recordField = SynFieldRcd.Create(fieldName, createFSharpType (Some typeName) fieldInfo.fieldType)
                        { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }

                | Some (GraphqlType.Union unionDef), Some nestedSelectionSet ->
                    let fragments = Query.findInlineFragments nestedSelectionSet.nodes
                    let typeName = findNextTypeName fieldName unionDef.name [ ] visitedTypes
                    visitedTypes.Add(typeName)
                    let localUnionCases = Dictionary<string, string>()
                    for fragment in fragments do
                        match Schema.findTypeByName fragment.typeCondition schema with
                        | Some (GraphqlType.Object objectDef) ->
                            let caseName = findNextTypeName fieldName fragment.typeCondition [ ] visitedTypes
                            visitedTypes.Add caseName
                            let nestedType = generateFields caseName objectDef.description fragment.selection objectDef schema visitedTypes types
                            types.Add(caseName, nestedType)
                            localUnionCases.Add(fragment.typeCondition, caseName)

                        | _ ->
                            ()

                    let interfaceUnions = SynTypeDefnSimpleReprUnionRcd.Create [
                        for pair in localUnionCases  ->
                            let unionCaseType = SynUnionCaseType.Create([ SynFieldRcd.Create(pair.Key.ToLowerInvariant(), pair.Value) ])
                            SynUnionCaseRcd.Create(Ident.Create (capitalize pair.Key), unionCaseType)
                    ]

                    let interfaceUnionsInfo : SynComponentInfoRcd = {
                        Access = None
                        Attributes = [
                            SynAttributeList.Create [
                                SynAttribute.RequireQualifiedAccess()
                            ]
                        ]
                        Id = [ Ident.Create typeName ]
                        XmlDoc = PreXmlDoc.Create description
                        Parameters = [ ]
                        Constraints = [ ]
                        PreferPostfix = false
                        Range = range0
                    }

                    let simpleType = SynTypeDefnSimpleReprRcd.Union(interfaceUnions)
                    let unionType = SynModuleDecl.CreateSimpleType(interfaceUnionsInfo, simpleType)

                    types.Add(typeName, unionType)

                    let recordField = SynFieldRcd.Create(fieldName, createFSharpType (Some typeName) fieldInfo.fieldType)
                    { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }

                | _ ->
                    ()

            | Some fieldInfo ->
                // a field that cannot expand which means it was a scalar
                let fieldName = field.alias |> Option.defaultValue field.name
                let recordFieldType = createFSharpType None fieldInfo.fieldType
                let recordField = SynFieldRcd.Create(fieldName, recordFieldType)
                { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }
    ]

    let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation
    SynModuleDecl.CreateSimpleType(info, simpleType)

let isCustomScalar (typeName: string) (schema: GraphqlSchema) =
    schema.types
    |> List.choose (function
        | GraphqlType.Scalar (GraphqlScalar.Custom name) -> Some name
        | _ -> None)
    |> List.contains typeName

let rec makeVariableType variableType  (schema: GraphqlSchema) =
    match variableType with
    | GraphqlVariableType.NonNull(GraphqlVariableType.Ref name) ->
        match name with
        | "Int" -> SynType.Int()
        | "String" -> SynType.String()
        | "Boolean" -> SynType.Bool()
        | "Float" -> SynType.Float()
        | "ID" -> SynType.String()
        | "Date" -> SynType.DateTime()
        | "DateTime"-> SynType.DateTime()
        | "Datetime" -> SynType.DateTimeOffset()
        | "DateTimeOffset" -> SynType.DateTimeOffset()
        | "Decimal" -> SynType.Decimal()
        | "Numeric" -> SynType.Decimal()
        | "BigFloat" -> SynType.Decimal()
        | "timestamptz" -> SynType.DateTimeOffset()
        | _ when isCustomScalar name schema -> SynType.String()
        | _ -> SynType.Create name

    | GraphqlVariableType.Ref name ->
        let variableType =
            match name with
            | "Int" -> SynType.Int()
            | "String" -> SynType.String()
            | "Boolean" -> SynType.Bool()
            | "Float" -> SynType.Float()
            | "ID" -> SynType.String()
            | "Date" -> SynType.DateTime()
            | "DateTime"-> SynType.DateTime()
            | "Datetime" -> SynType.DateTimeOffset()
            | "DateTimeOffset" -> SynType.DateTimeOffset()
            | "timestamptz" -> SynType.DateTimeOffset()
            | "Decimal" -> SynType.Decimal()
            | "Numeric" -> SynType.Decimal()
            | "BigFloat" -> SynType.Decimal()
            | _ when isCustomScalar name schema -> SynType.String()
            | _ -> SynType.Create name

        SynType.Option(variableType)
    | GraphqlVariableType.NonNull(GraphqlVariableType.List variableType) ->
        SynType.List(makeVariableType variableType schema)
    | GraphqlVariableType.List variableType ->
        SynType.Option(SynType.List(makeVariableType variableType schema))
    | GraphqlVariableType.NonNull variableType ->
        makeVariableType variableType schema

let generateInputVariablesType (variables: GraphqlVariable list) (schema: GraphqlSchema) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create "InputVariables" ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create([
        for variable in variables ->
            let variableType = makeVariableType variable.variableType schema
            SynFieldRcd.Create(variable.variableName, variableType)
    ])

    let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation
    SynModuleDecl.CreateSimpleType(info, simpleType)

let generateTypes (rootQueryName: string) (errorTypeName: string) (document: GraphqlDocument) (schema: GraphqlSchema) : SynModuleDecl list =
    match Query.findOperation (Query.expandDocumentFragments document) with
    | None -> [ ]
    | Some (GraphqlOperation.Query query) ->
        match Schema.findQuery schema with
        | None -> [ ]
        | Some queryType ->
            let inputTypes =
                schema.types
                |> List.choose (function
                    | GraphqlType.Enum enumRef -> Some enumRef.name
                    | GraphqlType.InputObject inputRef -> Some inputRef.name
                    | _ -> None)

            let visitedTypes = ResizeArray<string> [ yield! inputTypes; "InputVariables" ]
            let allTypes = Dictionary<string, SynModuleDecl>()
            let rootType = generateFields rootQueryName queryType.description query.selectionSet queryType schema visitedTypes allTypes
            [
                if query.variables.Length > 0 then yield generateInputVariablesType query.variables schema

                for typeName in allTypes.Keys do
                    yield allTypes.[typeName]

                yield rootType
            ]

    | Some (GraphqlOperation.Mutation mutation) ->
        match Schema.findMutation schema with
        | None -> [ ]
        | Some mutationType ->
            let inputTypes =
                schema.types
                |> List.choose (function
                    | GraphqlType.Enum enumRef -> Some enumRef.name
                    | GraphqlType.InputObject inputRef -> Some inputRef.name
                    | _ -> None)

            let visitedTypes = ResizeArray<string> [ yield! inputTypes; "InputVariables" ]
            let allTypes = Dictionary<string, SynModuleDecl>()
            let rootType = generateFields rootQueryName mutationType.description mutation.selectionSet mutationType schema visitedTypes allTypes
            [
                if mutation.variables.Length > 0 then yield generateInputVariablesType mutation.variables schema

                for typeName in allTypes.Keys do
                    yield allTypes.[typeName]

                yield rootType
            ]

let createNamespace (names: seq<string>) declarations =
    let nameParts =
        names
        |> Seq.collect (fun name ->
            if name.Contains "."
            then name.Split('.')
            else [| name |]
        )

    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ for name in nameParts -> Ident.Create name ], true, SynModuleOrNamespaceKind.DeclaredNamespace,declarations,  xmlDoc, [ ], None, range.Zero)

let createQualifiedModule (idens: seq<string>) declarations =
    let nameParts =
        idens
        |> Seq.collect (fun name ->
            if name.Contains "."
            then name.Split('.')
            else [| name |]
        )

    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ for ident in nameParts -> Ident.Create ident ], true, SynModuleOrNamespaceKind.NamedModule,declarations,  xmlDoc, [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess()  ]  ], None, range.Zero)

let createFile fileName modules =
    let qualfiedNameOfFile = QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create fileName)
    ParsedImplFileInput.ParsedImplFileInput(fileName, false, qualfiedNameOfFile, [], [], modules, (false, false))

let private formatConfig =
    { FormatConfig.FormatConfig.Default with
        EndOfLine = EndOfLineStyle.CRLF;
        NewlineBetweenTypeDefinitionAndMembers = true;
        StrictMode = true }

let formatAst file fileName =
    CodeFormatter.FormatASTAsync (ParsedInput.ImplFile file, fileName, [], Some (SourceOrigin.SourceString file.ToRcd.File), formatConfig)
    |> Async.RunSynchronously

let defaultErrorType() =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create "ErrorType" ]
        XmlDoc = PreXmlDoc.Create [ " The error returned by the GraphQL backend" ]
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let recordRepresentation =  SynTypeDefnSimpleReprRecordRcd.Create [
        SynFieldRcd.Create("message", SynType.String())
    ]

    let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation
    SynModuleDecl.CreateSimpleType(info, simpleType)

let readTypeSegment (input: string) =
    match input.ToLower() with
    | "string" -> SynType.String() |> Ok
    | "int" -> SynType.Int()|> Ok
    | "bool" -> SynType.Bool()|> Ok
    | "int64" -> SynType.Create("int64")|> Ok
    | "option<string>" -> SynType.Option(SynType.String()) |> Ok
    | "string option" -> SynType.Option(SynType.String()) |> Ok
    | "option<int>" -> SynType.Option(SynType.Int()) |> Ok
    | "int option" -> SynType.Option(SynType.Int()) |> Ok
    | "option<bool>" -> SynType.Option(SynType.Bool()) |> Ok
    | "bool option" -> SynType.Option(SynType.Bool()) |> Ok
    | "list<string>" -> SynType.List(SynType.String()) |> Ok
    | "string list" -> SynType.List(SynType.String()) |> Ok
    | "list<int>" -> SynType.List(SynType.Int()) |> Ok
    | "int list" -> SynType.List(SynType.Int()) |> Ok
    | "dictionary<string, string>" -> SynType.Dictionary(SynType.String(), SynType.String()) |> Ok
    | otherwise -> Error input

let parseErrorType (typeInfo: JObject) =
    match Seq.toList (typeInfo.Properties()) with
    | [ ] -> Error "Missing type name from the custom error type"
    | property :: _  ->
        if property.Value.Type <> JTokenType.Object then
            Error ($"Property {property.Name} must be an object containing the fields of the custom error")
        else
        let info : SynComponentInfoRcd = {
            Access = None
            Attributes = [ ]
            Id = [ Ident.Create property.Name ]
            XmlDoc = PreXmlDoc.Create [ " The error returned by the GraphQL backend" ]
            Parameters = [ ]
            Constraints = [ ]
            PreferPostfix = false
            Range = range0
        }

        let errorFields = unbox<JObject> property.Value
        let mutable errorMessage = None
        let fields = ResizeArray<SynFieldRcd>()
        for prop in errorFields.Properties() do
            if errorMessage.IsNone && prop.Value.Type = JTokenType.String then
                match readTypeSegment (string prop.Value) with
                | Error errorType -> errorMessage <- Some ($"Could not create field {property.Name} : {errorType} for the custom error type")
                | Ok fieldType -> fields.Add(SynFieldRcd.Create(prop.Name, fieldType))
            elif prop.Value.Type <> JTokenType.String then
                errorMessage <- Some ($"Custom error property '{prop.Name}' must be a string")
            else
                ()

        match errorMessage with
        | Some errorMsg -> Error errorMsg
        | None ->
            let recordRepresentation =  SynTypeDefnSimpleReprRecordRcd.Create (Seq.toList fields)
            let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation
            Ok (property.Name, SynModuleDecl.CreateSimpleType(info, simpleType))

let createDummyStringEnumAttribute() =
    """namespace Fable.Core

type StringEnumAttribute() =
    inherit System.Attribute()
    """

type DocumentGenerationData =
    { NugetPackageReferences: XElement seq
      Files: XElement seq
      CopyLocalLockFileAssemblies: bool option
      ContentItems: XElement seq
      ProjectReferences: XElement seq }

let generatePropsDocument
    { NugetPackageReferences = packageReferences
      Files = files
      CopyLocalLockFileAssemblies = copyLocalLockFileAssemblies
      ContentItems = contentItems
      ProjectReferences = projectReferences } =
    XDocument(
        XElement.ofStringName("Project",
            seq {
            MSBuildXElement.PropertyGroup(
                seq {
                    if copyLocalLockFileAssemblies.IsSome then
                        XElement.ofStringName("CopyLocalLockFileAssemblies", (copyLocalLockFileAssemblies.ToString().ToLower()))
                    XElement.ofStringName("GenerateDocumentationFile", true)
                    })
            if copyLocalLockFileAssemblies.IsSome then
                MSBuildXElement.PropertyGroup(copyLocalLockFileAssemblies.Value)
            if not (files |> Seq.isEmpty) then
                XElement.ofStringName("ItemGroup", files)
            if not (Seq.isEmpty contentItems) then
                XElement.ofStringName("ItemGroup", contentItems)
            if not (Seq.isEmpty packageReferences) then
                XElement.ofStringName("ItemGroup", packageReferences)
            if not (Seq.isEmpty projectReferences) then
                XElement.ofStringName("ItemGroup", projectReferences)
        }))

let generateProjectDocument
    { NugetPackageReferences = packageReferences
      Files = files
      CopyLocalLockFileAssemblies = copyLocalLockFileAssemblies
      ContentItems = contentItems
      ProjectReferences = projectReferences } =
    XDocument(
        XElement.ofStringName("Project",
            XAttribute.ofStringName("Sdk", "Microsoft.NET.Sdk"),
            seq {
            MSBuildXElement.PropertyGroup(
                seq {
                    XElement.ofStringName("TargetFramework", "netstandard2.0")
                    XElement.ofStringName("LangVersion", "latest")
                    if copyLocalLockFileAssemblies.IsSome then
                        XElement.ofStringName("CopyLocalLockFileAssemblies",
                            if copyLocalLockFileAssemblies.Value
                            then "true"
                            else "false"
                        )
                    XElement.ofStringName("GenerateDocumentationFile", true)
                })
            if not (Seq.isEmpty files) then
                XElement.ofStringName("ItemGroup", files)
            if not (Seq.isEmpty contentItems) then
                XElement.ofStringName("ItemGroup", contentItems)
            if not (Seq.isEmpty packageReferences) then
                XElement.ofStringName("ItemGroup", packageReferences)
            if not (Seq.isEmpty projectReferences) then
                XElement.ofStringName("ItemGroup", projectReferences)
        }))

let addLines (query: string) =
    query.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> "                " + line)
    |> String.concat Environment.NewLine

let private toPascalCase str =
    let firstChar = str |> Seq.head
    if Char.IsUpper(firstChar)
    then str
    else
        let sb = StringBuilder str
        sb.[0] <- Char.ToUpperInvariant(firstChar)
        sb.ToString()

let sampleClientMember query queryName hasVariables =
    let queryName = toPascalCase queryName
    let args = if hasVariables then "input: " + queryName + ".InputVariables" else ""
    let query = "\"\"\"\n" + addLines query + "\n            \"\"\""
    let body = if hasVariables then "{ query = query; variables = Some input }" else "{ query = query; variables = None }"
    $"""    member _.{queryName}({args}) =
        async {{
            let query = {query}
            let! response =
                Http.request url
                |> Http.method POST
                |> Http.headers [ Headers.contentType "application/json"; yield! headers ]
                |> Http.content (BodyContent.Text (Json.serialize {body}))
                |> Http.send

            match response.statusCode with
            | 200 ->
                let response = Json.parseNativeAs<GraphqlSuccessResponse<{queryName}.Query>> response.responseText
                return Ok response.data

            | errorStatus ->
                let response = Json.parseNativeAs<GraphqlErrorResponse> response.responseText
                return Error response.errors
        }}
"""

let asyncRequestBody serializer body =
    match serializer with
    | SerializerType.System ->
        $"""
            let! response =
                httpClient.PostAsJsonAsync(url, {body}, options)
                |> Async.AwaitTask
            let! responseContent = Async.AwaitTask(response.Content.ReadAsStringAsync())
    """
    | SerializerType.Newtonsoft ->
        $"""
            let inputJson = JsonConvert.SerializeObject({body}, settings)
            let! response =
                httpClient.PostAsync(url, new StringContent(inputJson, Encoding.UTF8, "application/json"))
                |> Async.AwaitTask
            let! responseContent = Async.AwaitTask(response.Content.ReadAsStringAsync())
    """
    

let taskRequestBody serializer body =
    match serializer with
    | SerializerType.System ->
        $"""
            let! response = httpClient.PostAsJsonAsync(url, {body}, options)
            let! responseContent = Async.AwaitTask(response.Content.ReadAsStringAsync())
    """
    | SerializerType.Newtonsoft ->
        $"""
            let inputJson = JsonConvert.SerializeObject({body}, settings)
            let! response = httpClient.PostAsync(url, new StringContent(inputJson, Encoding.UTF8, "application/json"))
            let! responseContent = Async.AwaitTask(response.Content.ReadAsStringAsync())
    """
    

let sampleFSharpNewtonsoftClientMember query queryName hasVariables useTasks =
    let queryName = toPascalCase queryName
    let args = if hasVariables then "input: " + queryName + ".InputVariables" else ""
    let builder = if useTasks then "task" else "async"
    let query = "\"\"\"\n" + addLines query + "\n            \"\"\""
    let body = if hasVariables then "{ query = query; variables = Some input }" else "{ query = query; variables = None }"
    let requestBody = if useTasks then taskRequestBody SerializerType.Newtonsoft body else asyncRequestBody SerializerType.Newtonsoft body
    let queryArgs = if hasVariables then " input" else "()"
    let syncMember =
        if useTasks
        then $"member this.{queryName}({args}) = Async.RunSynchronously(Async.AwaitTask(this.{queryName}Async{queryArgs}))"
        else $"member this.{queryName}({args}) = Async.RunSynchronously(this.{queryName}Async{queryArgs})"

    $"""
    member _.{queryName}Async({args}) =
        {builder} {{
            let query = {query}

            {requestBody}
            let responseJson = JsonConvert.DeserializeObject<JObject>(responseContent, settings)

            match response.IsSuccessStatusCode with
            | true ->
                let errorsReturned =
                    responseJson.ContainsKey "errors"
                    && responseJson.["errors"].Type = JTokenType.Array
                    && responseJson.["errors"].HasValues

                if errorsReturned then
                    let response = responseJson.ToObject<GraphqlErrorResponse>(JsonSerializer.Create(settings))
                    return Error response.errors
                else
                    let response = responseJson.ToObject<GraphqlSuccessResponse<{queryName}.Query>>(JsonSerializer.Create(settings))
                    return Ok response.data

            | errorStatus ->
                let response = responseJson.ToObject<GraphqlErrorResponse>(JsonSerializer.Create(settings))
                return Error response.errors
        }}

    {syncMember}
"""
let sampleFSharpSystemClientMember query queryName hasVariables useTasks =
    let queryName = toPascalCase queryName
    let args = if hasVariables then "input: " + queryName + ".InputVariables" else ""
    let builder = if useTasks then "task" else "async"
    let query = "\"\"\"\n" + addLines query + "\n            \"\"\""
    let body = if hasVariables then "{ query = query; variables = Some input }" else "{ query = query; variables = None }"
    let requestBody = if useTasks then taskRequestBody SerializerType.System body else asyncRequestBody SerializerType.System body
    let queryArgs = if hasVariables then " input" else "()"
    let syncMember =
        if useTasks
        then $"member this.{queryName}({args}) = Async.RunSynchronously(Async.AwaitTask(this.{queryName}Async{queryArgs}))"
        else $"member this.{queryName}({args}) = Async.RunSynchronously(this.{queryName}Async{queryArgs})"

    $"""
    member _.{queryName}Async({args}) =
        {builder} {{
            let query = {query}

            {requestBody}
            let responseJson = JsonSerializer.Deserialize<JsonElement>(responseContent, options)

            match response.IsSuccessStatusCode with
            | true ->
                let errorsReturned =
                    match responseJson.TryGetProperty ("errors") with
                    | true, value -> value.GetArrayLength() > 0
                    | false, _ -> false

                if errorsReturned then
                    let response = JsonSerializer.Deserialize<GraphqlErrorResponse> (responseContent, options)
                    return Error response.errors
                else
                    let response = JsonSerializer.Deserialize<GraphqlSuccessResponse<{queryName}.Query>> (responseContent, options)
                    return Ok response.data

            | errorStatus ->
                let response = JsonSerializer.Deserialize<GraphqlErrorResponse> (responseContent, options)
                return Error response.errors
        }}

    {syncMember}
"""

let inline sampleFSharpClientMember serializer query queryName hasVariables useTasks =
    match serializer with
    | SerializerType.System -> sampleFSharpSystemClientMember query queryName hasVariables useTasks
    | SerializerType.Newtonsoft -> sampleFSharpNewtonsoftClientMember query queryName hasVariables useTasks

let sampleFableGraphqlClient projectName clientName errorType members =
    $"""namespace {projectName}

open System
open System.Net.Http
open Fable.SimpleHttp
open Fable.SimpleJson

type GraphqlInput<'T> = {{ query: string; variables: Option<'T> }}
type GraphqlSuccessResponse<'T> = {{ data: 'T }}
type GraphqlErrorResponse = {{ errors: {errorType} list }}

type {clientName}(url: string, headers: Header list) =
    new(url: string) = {clientName}(url, [ ])
{members}"""

let private sampleFSharpSystemGraphqlClient projectName clientName errorType members useTasks =
    $"""namespace {projectName}

open System.Net.Http
open System.Text
open System.Text.Json
open System.Net.Http.Json
open System.Text.Json.Serialization
{if useTasks then "open FSharp.Control.Tasks" else ""}

type GraphqlInput<'T> = {{ query: string; variables: Option<'T> }}
type GraphqlSuccessResponse<'T> = {{ data: 'T }}
type GraphqlErrorResponse = {{ errors: {errorType} list }}

type {clientName}(url: string, httpClient: HttpClient, options: JsonSerializerOptions) =
    new(url: string, options: JsonSerializerOptions) = {clientName}(url, new HttpClient(), options)
    new(url: string, client: HttpClient) =
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        {clientName}(url, client, options)
    new(url: string) = {clientName}(url, new HttpClient())
{members}"""

let private sampleFSharpNewtonsoftGraphqlClient projectName clientName errorType members useTasks =
        $"""namespace {projectName}

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Fable.Remoting.Json
open System.Net.Http
open System.Net.Http.Json
open System.Text
{if useTasks then "open FSharp.Control.Tasks" else ""}

type GraphqlInput<'T> = {{ query: string; variables: Option<'T> }}
type GraphqlSuccessResponse<'T> = {{ data: 'T }}
type GraphqlErrorResponse = {{ errors: {errorType} list }}

type {clientName}(url: string, httpClient: HttpClient) =
    let fableJsonConverter = FableJsonConverter() :> JsonConverter
    let settings = JsonSerializerSettings(DateParseHandling=DateParseHandling.None, Converters = [| fableJsonConverter |])

    new(url: string) = {clientName}(url, new HttpClient())

    {members}"""

let sampleFSharpGraphqlClient projectName clientName errorType members serializer =
    match serializer with
    | SerializerType.System -> sampleFSharpSystemGraphqlClient projectName clientName errorType members
    | SerializerType.Newtonsoft -> sampleFSharpNewtonsoftGraphqlClient projectName clientName errorType members

let sampleFableGraphqlClientFsi projectName clientName =
        $"""namespace {projectName}

open System.Net.Http
open Fable.SimpleHttp
open Fable.SimpleJson

type {clientName} =
    class
        /// <summary>Creates {clientName} specifying <see href="T:HttpClient">HttpClient</see> instance</summary>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization <b>you must</b> add
        /// <see href="T:Fable.SimpleJson.FableJsonConverter">FableJsonConverter</see>
        /// from <a href="https://github.com/Zaid-Ajaj/Fable.SimpleJson">Fable.SimpleJson</a> NuGet package yourself
        /// </remarks>
        /// <param name="url">GraphQL endpoint URL</param>
        new: url: string * headers: Header list -> SpotifyGraphqlClient

        /// <summary>Creates {clientName}</summary>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization
        /// <see href="T:Fable.SimpleJson.FableJsonConverter">FableJsonConverter</see> is added
        /// from <a href="https://github.com/Zaid-Ajaj/Fable.SimpleJson">Fable.SimpleJson</a> NuGet package
        /// </remarks>
        new: url: string -> SpotifyGraphqlClient
    end
"""

let private sampleFSharpSystemGraphqlClientFsi projectName clientName =
    $"""namespace {projectName}

open System.Net.Http
open System.Text
open System.Text.Json

type {clientName} =
    class
        /// <summary>Creates {clientName} specifying <see href="T:HttpClient">HttpClient</see> instance</summary>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization <b>you must</b> add
        /// <see href="T:System.Text.Json.Serialization.JsonFSharpConverter">JsonFSharpConverter</see>
        /// from <a href="https://github.com/Tarmil/FSharp.SystemTextJson">FSharp.SystemTextJson</a> NuGet package yourself
        /// </remarks>
        /// <param name="url">GraphQL endpoint URL</param>
        new: url: string * client: HttpClient * options: JsonSerializerOptions -> SpotifyGraphqlClient

        /// <summary>
        /// Creates {clientName} specifying <see href="T:JsonSerializerOptions">JsonSerializerOptions</see>
        /// </summary>
        /// <param name="url">GraphQL endpoint URL</param>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization <b>you must</b> add
        /// <see href="T:System.Text.Json.Serialization.JsonFSharpConverter">JsonFSharpConverter</see>
        /// from <a href="https://github.com/Tarmil/FSharp.SystemTextJson">FSharp.SystemTextJson</a> NuGet package yourself
        /// </remarks>
        new: url: string * options: JsonSerializerOptions -> SpotifyGraphqlClient

        /// <summary>
        /// Creates {clientName} specifying <see href="T:HttpClient">HttpClient</see> instance
        /// </summary>
        /// <param name="url">GraphQL endpoint URL</param>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization
        /// <see href="T:System.Text.Json.Serialization.JsonFSharpConverter">JsonFSharpConverter</see> by default is added
        /// from <a href="https://github.com/Tarmil/FSharp.SystemTextJson">FSharp.SystemTextJson</a> NuGet package
        /// </remarks>
        new: url: string * client: HttpClient -> SpotifyGraphqlClient

        /// <summary>Creates {clientName}</summary>
        /// <param name="url">GraphQL endpoint URL</param>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization
        /// <see href="T:System.Text.Json.Serialization.JsonFSharpConverter">JsonFSharpConverter</see> by default is added
        /// from <a href="https://github.com/Tarmil/FSharp.SystemTextJson">FSharp.SystemTextJson</a> NuGet package
        /// </remarks>
        new: url: string -> SpotifyGraphqlClient
    end
"""

let private sampleFSharpNewtonsoftGraphqlClientFsi projectName clientName =
        $"""namespace {projectName}

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Fable.Remoting.Json
open System.Net.Http
open System.Text

type {clientName} =
    class
        /// <summary>Creates {clientName} specifying <see href="T:HttpClient">HttpClient</see> instance</summary>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization
        /// <see href="T:Fable.Remoting.Json.FableJsonConverter">FableJsonConverter</see> is added
        /// from <a href="https://github.com/Zaid-Ajaj/Fable.SimpleJson">Fable.SimpleJson</a> NuGet package
        /// </remarks>
        /// <param name="url">GraphQL endpoint URL</param>
        new: url: string * client: HttpClient -> SpotifyGraphqlClient

        /// <summary>Creates {clientName}</summary>
        /// <remarks>
        /// In order to enable all F# types serialization and deserealization
        /// <see href="T:Fable.SimpleJson.FableJsonConverter">FableJsonConverter</see> is added
        /// from <a href="https://github.com/Zaid-Ajaj/Fable.SimpleJson">Fable.SimpleJson</a> NuGet package
        /// </remarks>
        new: string -> SpotifyGraphqlClient
    end
"""

let sampleFSharpGraphqlClientFsi projectName clientName serializer =
    match serializer with
    | SerializerType.System -> sampleFSharpSystemGraphqlClientFsi projectName clientName
    | SerializerType.Newtonsoft -> sampleFSharpNewtonsoftGraphqlClientFsi projectName clientName
