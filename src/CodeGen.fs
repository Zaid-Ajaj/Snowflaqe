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

let compiledName (name: string) = SynAttribute.Create("CompiledName", name)

let capitalize (input: string) = input.First().ToString().ToUpper() + String.Join("", input.Skip(1)).ToLowerInvariant()

let normalizeEnumName (unionCase: string) =
    if not(unionCase.Contains "_") then
        capitalize unionCase
    else
        unionCase.Split [| '_' |]
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

let optionOf id inner =
    SynFieldRcd.CreateApp id (LongIdentWithDots.Create [ "Option" ]) [ (LongIdentWithDots.Create [ inner ]) ]
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

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.String)) ->
            SynFieldRcd.CreateString(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.String) ->
            optionOf field.fieldName "string"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Boolean)) ->
            SynFieldRcd.CreateBool(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.Boolean) ->
            optionOf field.fieldName "bool"

        | GraphqlFieldType.NonNull(GraphqlFieldType.Scalar (GraphqlScalar.Float)) ->
            SynFieldRcd.CreateFloat(field.fieldName)

        | GraphqlFieldType.Scalar (GraphqlScalar.Float) ->
            optionOf field.fieldName "float"

        | GraphqlFieldType.NonNull(GraphqlFieldType.EnumRef enumName) ->
            let synType = LongIdentWithDots.Create [ enumName ]
            SynFieldRcd.Create(field.fieldName, synType)

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

let createNamespace name declarations =
    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ Ident.Create name ], true, SynModuleOrNamespaceKind.DeclaredNamespace,declarations,  xmlDoc, [ ], None, range.Zero)

let createFile fileName modules =
    let qualfiedNameOfFile = QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create fileName)
    ParsedImplFileInput.ParsedImplFileInput(fileName, false, qualfiedNameOfFile, [], [], modules, (false, false))

let formatAst file =
    formatAst (ParsedInput.ImplFile file)
    |> Async.RunSynchronously
