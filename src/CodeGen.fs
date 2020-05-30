[<RequireQualifiedAccess>]
module Snowflaqe.CodeGen

open System
open System.Linq
open FsAst
open Fantomas
open Snowflaqe.Types
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.Range
open System.Collections.Generic

let compiledName (name: string) = SynAttribute.Create("CompiledName", name)

let capitalize (input: string) = input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

let normalizeName (unionCase: string) =
    if not(unionCase.Contains "_") then
        capitalize unionCase
    else
        unionCase.Split [| '_'; '-' |]
        |> Array.filter String.isNotNullOrEmpty
        |> Array.map capitalize
        |> String.concat ""

let capitalizeEnum (input: string) =
    input.First().ToString().ToUpper() + String.Join("", input.Skip(1)).ToLowerInvariant()
let normalizeEnumName (unionCase: string) =
    if not(unionCase.Contains "_") then
        capitalizeEnum unionCase
    else
        unionCase.Split [| '_'; '-' |]
        |> Array.filter String.isNotNullOrEmpty
        |> Array.map capitalizeEnum
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

let optionOf id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ inner ]) ]

let optionOfSystemDot id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ "System"; inner ]) ]

let listOfSystemDot id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "list" ]) [ (LongIdentWithDots.Create [ "System"; inner ]) ]

let listOf id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "list" ]) [ (LongIdentWithDots.Create [ inner ]) ]

let systemDot id inner =
    SynFieldRcd.Create(id, LongIdentWithDots([ Ident.Create "System"; Ident.Create inner ], []))

// TODO:
// f inner -> Option<list<inner>>
// g inner -> list<Option<inner>>
// h inner -> Option<list<Option<inner>>
// k inner -> list<list<inner>>

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

    let recordFieldType (field: GraphqlInputField) =
        match field.fieldType with
        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Int)) ->
            SynFieldRcd.CreateInt(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.Int) ->
            optionOf field.fieldName "int"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTimeOffset")) ->
            systemDot field.fieldName "DateTimeOffset"

        | GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTimeOffset") ->
            optionOfSystemDot field.fieldName "DateTimeOffset"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTime")) ->
            systemDot field.fieldName "DateTime"

        | GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTime") ->
            optionOfSystemDot field.fieldName "DateTime"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.String)) ->
            SynFieldRcd.CreateString(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.String) ->
            optionOf field.fieldName "string"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Boolean)) ->
            SynFieldRcd.CreateBool(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.Boolean) ->
            optionOf field.fieldName "bool"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "Decimal")) ->
            SynFieldRcd.Create(field.fieldName, LongIdentWithDots.Create [ "decimal" ])

        | GraphqlFieldType.Scalar (GraphqlScalar.Custom "Decimal") ->
            optionOf field.fieldName "decimal"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Float)) ->
            SynFieldRcd.CreateFloat(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.Float) ->
            optionOf field.fieldName "float"

        | GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumName) ->
            let synType = LongIdentWithDots.Create [ enumName ]
            SynFieldRcd.Create(field.fieldName, synType)

        | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumName))) ->
            listOf field.fieldName enumName

        | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.InputObjectRef inputRef))) ->
            listOf field.fieldName inputRef

        | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.Scalar scalar ))) ->
            match scalar with
            | GraphqlScalar.Int -> listOf field.fieldName "int"
            | GraphqlScalar.String -> listOf field.fieldName "string"
            | GraphqlScalar.Boolean -> listOf field.fieldName "bool"
            | GraphqlScalar.Float -> listOf field.fieldName "float"
            | GraphqlScalar.ID -> listOf field.fieldName "string"
            | GraphqlScalar.Custom "DateTimeOffset" -> listOfSystemDot field.fieldName "DateTimeOffset"
            | GraphqlScalar.Custom "DateTime" -> listOfSystemDot field.fieldName "DateTime"
            | GraphqlScalar.Custom "Decimal" -> listOf field.fieldName "decimal"
            | GraphqlScalar.Custom custom -> listOf field.fieldName custom

        | GraphqlFieldType.EnumRef enumName ->
            optionOf field.fieldName enumName

        | GraphqlFieldType.NonNull(GraphqlFieldType.InputObjectRef inputRef) ->
            let synType = LongIdentWithDots.Create [ inputRef ]
            SynFieldRcd.Create(field.fieldName, synType)

        | GraphqlFieldType.InputObjectRef inputRef ->
            optionOf field.fieldName inputRef

        | _ ->
            SynFieldRcd.CreateInt field.fieldName

    let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
        for field in fields ->
            let recordField = recordFieldType field
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

let objectFieldType (fieldName: string) (field: GraphqlField) =

    match field.fieldType with
    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Int)) ->
        SynFieldRcd.CreateInt(fieldName)

    | GraphqlFieldType.Scalar (GraphqlScalar.Int) ->
        optionOf fieldName "int"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTimeOffset")) ->
        systemDot fieldName "DateTimeOffset"

    | GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTimeOffset") ->
        optionOfSystemDot fieldName "DateTimeOffset"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTime")) ->
        systemDot fieldName "DateTime"

    | GraphqlFieldType.Scalar (GraphqlScalar.Custom "DateTime") ->
        optionOfSystemDot fieldName "DateTime"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.String)) ->
        SynFieldRcd.CreateString(fieldName)

    | GraphqlFieldType.Scalar (GraphqlScalar.String) ->
        optionOf fieldName "string"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Boolean)) ->
        SynFieldRcd.CreateBool(fieldName)

    | GraphqlFieldType.Scalar (GraphqlScalar.Boolean) ->
        optionOf fieldName "bool"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Float)) ->
        SynFieldRcd.CreateFloat(fieldName)

    | GraphqlFieldType.Scalar (GraphqlScalar.Float) ->
        optionOf fieldName "float"

    | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Custom "Decimal")) ->
        SynFieldRcd.Create(fieldName, LongIdentWithDots.Create [ "decimal" ])

    | GraphqlFieldType.Scalar (GraphqlScalar.Custom "Decimal") ->
        optionOf fieldName "decimal"

    | GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumName) ->
        let synType = LongIdentWithDots.Create [ enumName ]
        SynFieldRcd.Create(fieldName, synType)

    | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumName))) ->
        listOf fieldName enumName

    | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.InputObjectRef inputRef))) ->
        listOf fieldName inputRef

    | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.Scalar scalar ))) ->
        match scalar with
        | GraphqlScalar.Int -> listOf fieldName "int"
        | GraphqlScalar.String -> listOf fieldName "string"
        | GraphqlScalar.Boolean -> listOf fieldName "bool"
        | GraphqlScalar.Float -> listOf fieldName "float"
        | GraphqlScalar.ID -> listOf fieldName "string"
        | GraphqlScalar.Custom "DateTimeOffset" -> listOfSystemDot fieldName "DateTimeOffset"
        | GraphqlScalar.Custom "DateTime" -> listOfSystemDot fieldName "DateTime"
        | GraphqlScalar.Custom custom -> listOf fieldName custom

    | GraphqlFieldType.EnumRef enumName ->
        optionOf fieldName enumName

    | GraphqlFieldType.NonNull(GraphqlFieldType.InputObjectRef inputRef) ->
        let synType = LongIdentWithDots.Create [ inputRef ]
        SynFieldRcd.Create(fieldName, synType)

    | GraphqlFieldType.InputObjectRef inputRef ->
        optionOf fieldName inputRef

    | _ ->
        SynFieldRcd.CreateInt fieldName

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
    elif not (visitedTypes.Contains nestedSelectionType) && selections.Length <= 3 && selections.Length < 1 then
        nestedSelectionType
    elif not (visitedTypes.Contains (normalizeName fieldName + "From" + objectName)) then
        objectName + normalizeName fieldName
    else
        nextTick (normalizeName fieldName + "From" + objectName) visitedTypes

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
                match fieldInfo.fieldType with
                | GraphqlFieldType.ObjectRef objectName ->
                    let nestedFieldType =
                        schema.types
                        |> List.tryPick (function
                            | GraphqlType.Object objectDef when objectDef.name = objectName -> Some objectDef
                            | _ -> None)
                    match nestedFieldType, field.selectionSet with
                    | Some objectDef, Some nestedSelectionSet ->
                        let nestedFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                                | _ -> None)

                        let typeName = findNextTypeName fieldName objectName nestedFields visitedTypes

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        optionOf fieldName typeName
                    | _ ->
                        ()

                | GraphqlFieldType.List(GraphqlFieldType.ObjectRef objectName) ->
                    let nestedFieldType =
                        schema.types
                        |> List.tryPick (function
                            | GraphqlType.Object objectDef when objectDef.name = objectName -> Some objectDef
                            | _ -> None)
                    match nestedFieldType, field.selectionSet with
                    | Some objectDef, Some nestedSelectionSet ->
                        let nestedFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                                | _ -> None)

                        let typeName = findNextTypeName fieldName objectName nestedFields visitedTypes

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        SynFieldRcd.Create(fieldName, LongIdentWithDots([ Ident.Create typeName ], [ ]))
                    | _ ->
                        ()

                | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.ObjectRef objectName)) ->
                    let nestedFieldType =
                        schema.types
                        |> List.tryPick (function
                            | GraphqlType.Object objectDef when objectDef.name = objectName -> Some objectDef
                            | _ -> None)
                    match nestedFieldType, field.selectionSet with
                    | Some objectDef, Some nestedSelectionSet ->
                        let nestedFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                                | _ -> None)

                        let typeName = findNextTypeName fieldName objectName nestedFields visitedTypes

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        SynFieldRcd.Create(fieldName, LongIdentWithDots([ Ident.Create typeName ], [ ]))
                    | _ ->
                        ()

                | GraphqlFieldType.NonNull(GraphqlFieldType.List(GraphqlFieldType.NonNull(GraphqlFieldType.ObjectRef objectName))) ->
                    let nestedFieldType =
                        schema.types
                        |> List.tryPick (function
                            | GraphqlType.Object objectDef when objectDef.name = objectName -> Some objectDef
                            | _ -> None)
                    match nestedFieldType, field.selectionSet with
                    | Some objectDef, Some nestedSelectionSet ->
                        let nestedFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                                | _ -> None)

                        let typeName = findNextTypeName fieldName objectName nestedFields visitedTypes

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        listOf fieldName typeName
                    | _ ->
                        ()
                | GraphqlFieldType.NonNull(GraphqlFieldType.ObjectRef objectName) ->
                    let nestedFieldType =
                        schema.types
                        |> List.tryPick (function
                            | GraphqlType.Object objectDef when objectDef.name = objectName -> Some objectDef
                            | _ -> None)
                    match nestedFieldType, field.selectionSet with
                    | Some objectDef, Some nestedSelectionSet ->
                        let nestedFields =
                            nestedSelectionSet.nodes
                            |> List.choose (function
                                | GraphqlNode.Field field -> field.alias |> Option.defaultValue field.name |> Some
                                | _ -> None)

                        let typeName = findNextTypeName fieldName objectName nestedFields visitedTypes

                        visitedTypes.Add(typeName)
                        let nestedType = generateFields typeName fieldInfo.description nestedSelectionSet objectDef schema visitedTypes types
                        types.Add(typeName, nestedType)
                        SynFieldRcd.Create(fieldName, LongIdentWithDots([ Ident.Create typeName ], [ ]))
                    | _ ->
                        ()
                | _ ->
                    ()

            | Some fieldInfo ->
                let fieldName = field.alias |> Option.defaultValue field.name
                let recordField = objectFieldType fieldName fieldInfo
                { recordField with XmlDoc = PreXmlDoc.Create fieldInfo.description }
    ]

    let simpleType = SynTypeDefnSimpleReprRcd.Record recordRepresentation

    SynModuleDecl.CreateSimpleType(info, simpleType)

let generateTypes (rootQueryName: string) (document: GraphqlDocument) (schema: GraphqlSchema) : SynModuleDecl list =
    match Query.findOperation (Query.expandDocumentFragments document) with
    | None -> [ ]
    | Some (GraphqlOperation.Query query) ->
        match Schema.findQuery schema with
        | None -> [ ]
        | Some queryType ->
            let visitedTypes = ResizeArray<string>()
            let allTypes = Dictionary<string, SynModuleDecl>()
            let rootType = generateFields rootQueryName queryType.description query.selectionSet queryType schema visitedTypes allTypes
            [
                for typeName in allTypes.Keys do
                    yield allTypes.[typeName]

                yield rootType
            ]

    | Some (GraphqlOperation.Mutation mutation) ->
        match Schema.findMutation schema with
        | None -> [ ]
        | Some mutationType ->
            let visitedTypes = ResizeArray<string>()
            let allTypes = Dictionary<string, SynModuleDecl>()
            let rootType = generateFields rootQueryName mutationType.description mutation.selectionSet mutationType schema visitedTypes allTypes
            [
                for typeName in allTypes.Keys do
                    yield allTypes.[typeName]

                yield rootType
            ]

let createNamespace name declarations =
    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ Ident.Create name ], true, SynModuleOrNamespaceKind.DeclaredNamespace,declarations,  xmlDoc, [ ], None, range.Zero)

let createQualifiedModule idens declarations =
    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace(idens |> List.map Ident.Create, true, SynModuleOrNamespaceKind.NamedModule,declarations,  xmlDoc, [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess()  ]  ], None, range.Zero)

let createFile fileName modules =
    let qualfiedNameOfFile = QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create fileName)
    ParsedImplFileInput.ParsedImplFileInput(fileName, false, qualfiedNameOfFile, [], [], modules, (false, false))

let formatAst file =
    formatAst (ParsedInput.ImplFile file)
    |> Async.RunSynchronously
