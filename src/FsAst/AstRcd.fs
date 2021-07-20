[<AutoOpen>]
module FsAst.AstRcd

open System
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc

type range = FSharp.Compiler.Text.range

type ParsedImplFileInputRcd = {
    File: string
    IsScript: bool
    QualName: QualifiedNameOfFile
    Pragmas: ScopedPragma list
    HashDirectives: ParsedHashDirective list
    Modules: SynModuleOrNamespace list
    IsLastCompiland: bool
    IsExe: bool }
with
    member x.FromRcd =
        ParsedImplFileInput(x.File, x.IsScript, x.QualName, x.Pragmas, x.HashDirectives, x.Modules, (x.IsLastCompiland, x.IsExe))

type ParsedImplFileInput with
    member x.ToRcd =
        let (ParsedImplFileInput(file, isScript, qualName, pragmas, hashDirectives, modules, (isLastCompiland, isExe))) = x
        { File = file; IsScript = isScript; QualName = qualName; Pragmas = pragmas; HashDirectives = hashDirectives; Modules = modules; IsLastCompiland = isLastCompiland; IsExe = isExe }

type SynModuleOrNamespaceRcd = {
    Id: LongIdent
    IsRecursive: bool
    Kind: SynModuleOrNamespaceKind
    Declarations: SynModuleDecl list
    XmlDoc: PreXmlDoc
    Attributes: SynAttributes
    Access: SynAccess option
    Range: range }
with
    member x.FromRcd =
        SynModuleOrNamespace(x.Id, x.IsRecursive, x.Kind, x.Declarations, x.XmlDoc, x.Attributes, x.Access, x.Range)

type SynModuleOrNamespace with
    member x.ToRcd =
        let (SynModuleOrNamespace(id, isRecursive, kind, declarations, xmlDoc, attributes, access, range)) = x
        { Id = id; IsRecursive = isRecursive; Kind = kind; Declarations = declarations; XmlDoc = xmlDoc; Attributes = attributes; Access = access; Range = range }

type SynComponentInfoRcd = {
    Attributes: SynAttributes
    Parameters: SynTyparDecl list
    Constraints: SynTypeConstraint list
    Id: LongIdent
    XmlDoc: PreXmlDoc
    PreferPostfix: bool
    Access: SynAccess option
    Range: range }
with
    member x.FromRcd =
        ComponentInfo(x.Attributes, x.Parameters, x.Constraints, x.Id, x.XmlDoc, x.PreferPostfix, x.Access, x.Range)

type SynComponentInfo with
    member x.ToRcd =
        let (ComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)) = x
        { Attributes = attributes; Parameters = parameters; Constraints = constraints; Id = id; XmlDoc = xmldoc; PreferPostfix = preferPostfix; Access = access; Range = range }

type SynTypeDefnRcd = {
    Info: SynComponentInfoRcd
    Repr: SynTypeDefnRepr
    Members: SynMemberDefns
    Range: range }
with
    member x.FromRcd =
        TypeDefn(x.Info.FromRcd, x.Repr, x.Members, x.Range)

type SynTypeDefn with
    member x.ToRcd =
        let (TypeDefn(info, repr, members, range)) = x
        { Info = info.ToRcd; Repr = repr; Members = members; Range = range }

type SynTypeDefnReprObjectModelRcd = {
    Kind: SynTypeDefnKind
    Members: SynMemberDefns
    Range: range }
with
    member x.FromRcd =
        SynTypeDefnRepr.ObjectModel(x.Kind, x.Members, x.Range)

type SynTypeDefnReprSimpleRcd = {
    Repr: SynTypeDefnSimpleRepr
    Range: range }
with
    member x.FromRcd =
        SynTypeDefnRepr.Simple(x.Repr, x.Range)

[<RequireQualifiedAccess>]
type SynTypeDefnReprRcd =
    | ObjectModel of SynTypeDefnReprObjectModelRcd
    | Simple of SynTypeDefnReprSimpleRcd
with
    member x.FromRcd =
        match x with
        | ObjectModel om -> om.FromRcd
        | Simple s -> s.FromRcd

type SynTypeDefnRepr with
    member x.ToRcd =
        match x with
        | SynTypeDefnRepr.ObjectModel(kind, members, range) ->
            SynTypeDefnReprRcd.ObjectModel { Kind = kind; Members = members; Range = range }
        | SynTypeDefnRepr.Simple(repr, range) ->
            SynTypeDefnReprRcd.Simple { Repr = repr; Range = range }
        | SynTypeDefnRepr.Exception _ -> failwith "Not supported"

// TODO other SynPat cases
[<RequireQualifiedAccess>]
type SynPatRcd =
    /// <summary>Creates a pattern of a constant value for example a literal string pattern that can be used in a match case</summary>
    | Const of SynPatConstRcd
    /// <summary>Creates a wild '_' pattern</summary>
    | Wild of SynPatWildRcd
    | Named of SynPatNamedRcd
    /// <summary>Creates a typed pattern 'pattern : type'</summary>
    | Typed of SynPatTypedRcd
    | Attrib of SynPatAttribRcd
    /// <summary>An OR pattern 'leftPattern | rightPattern'</summary>
    | Or of SynPatOrRcd
    /// <summary>
    /// An AND pattern 'leftPattern AND rightPattern'
    /// </summary>
    | Ands  of SynPatAndsRcd
    | LongIdent of SynPatLongIdentRcd
    | Tuple of SynPatTupleRcd
    | Paren of SynPatParenRcd
    | ArrayOrList of SynPatArrayOrListRcd
    | Null of SynPatNullRcd
    /// <summary>Creates an optional pattern '?pat' used for example to create optional parameters on static functions etc.</summary>
    | OptionalVal of SynPatOptionalValRcd
    /// <summary>A type test pattern ':? typeName'</summary>
    | IsInstance of SynPatIsInstanceRcd
    | Record of SynPatRecordRcd
    | QuoteExpr of SynPatQuoteExprRcd
    | DeprecatedCharRange of SynPatDeprecatedCharRangeRcd
    | InstanceMember of SynPatInstanceMemberRcd
    | FromParseError of SynPatFromParseErrorRcd

and SynPatConstRcd = {
    Const: SynConst
    Range: range }

and SynPatWildRcd = {
    Range: range }

and SynPatNamedRcd = {
    Pattern: SynPatRcd
    Id: Ident
    IsThis: bool
    Access: SynAccess option
    Range: range }

and SynPatTypedRcd = {
    Pattern: SynPatRcd
    Type: SynType
    Range: range }

and SynPatOptionalValRcd = {
    Id: Ident
    Range: range
}

and SynPatOrRcd = {
    Left: SynPatRcd
    Right: SynPatRcd
    Range: range }

and SynPatAndsRcd = {
    Patterns: SynPatRcd list
    Range: range }

and SynPatArrayOrListRcd = {
    IsArray: bool
    Elements: SynPatRcd list
    Range: range }

and SynPatIsInstanceRcd = {
    Type : SynType
    Range : range }

and SynPatRecordRcd = {
    Fields : ((LongIdent * Ident) * SynPatRcd) list
    Range : range
}

and SynPatQuoteExprRcd = {
    Expr : SynExpr
    Range : range
}

and SynPatDeprecatedCharRangeRcd = {
    StartChar: char
    EndChar: char
    Range : range }

and SynPatInstanceMemberRcd = {
    ThisId: Ident
    MemberId: Ident
    ToolingId: Ident option
    Accessibility: SynAccess option
    Range: range }

and SynPatFromParseErrorRcd = {
    Pattern : SynPatRcd
    Range : range }

and SynPatAttribRcd = {
    Pattern: SynPatRcd
    Attributes: SynAttributes
    Range: range }

and SynPatLongIdentRcd = {
    Id: LongIdentWithDots
    ExtraId : Ident option
    TyparDecls : SynValTyparDecls option
    Args: SynArgPats
    Access: SynAccess option
    Range: range }

and SynPatTupleRcd = {
    Patterns: SynPatRcd list
    Range: range }

and SynPatParenRcd = {
    Pattern: SynPatRcd
    Range: range }

and SynPatNullRcd = {
    Range: range }

type SynPatRcd  with
    member x.FromRcd =
        match x with
        | Const c -> c.FromRcd
        | Wild w -> w.FromRcd
        | Named n -> n.FromRcd
        | Typed t -> t.FromRcd
        | Attrib a -> a.FromRcd
        | LongIdent u -> u.FromRcd
        | Tuple t -> t.FromRcd
        | Paren t -> t.FromRcd
        | Null n -> n.FromRcd
        | OptionalVal n -> n.FromRcd
        | Or n -> n.FromRcd
        | Ands n -> n.FromRcd
        | ArrayOrList n -> n.FromRcd
        | IsInstance n -> n.FromRcd
        | Record n -> n.FromRcd
        | QuoteExpr n -> n.FromRcd
        | DeprecatedCharRange n -> n.FromRcd
        | InstanceMember n -> n.FromRcd
        | FromParseError n -> n.FromRcd

and SynPatConstRcd with
    member x.FromRcd = SynPat.Const(x.Const, x.Range)
and SynPatWildRcd with
    member x.FromRcd = SynPat.Wild(x.Range)
and SynPatNamedRcd with
    member x.FromRcd = SynPat.Named(x.Pattern.FromRcd, x.Id, x.IsThis, x.Access, x.Range)
and SynPatTypedRcd with
    member x.FromRcd = SynPat.Typed(x.Pattern.FromRcd, x.Type, x.Range)
and SynPatAttribRcd with
    member x.FromRcd = SynPat.Attrib(x.Pattern.FromRcd, x.Attributes, x.Range)
and SynPatLongIdentRcd with
    member x.FromRcd = SynPat.LongIdent(x.Id, x.ExtraId, x.TyparDecls, x.Args, x.Access, x.Range)
and SynPatTupleRcd with
    member x.FromRcd = SynPat.Tuple(false, x.Patterns |> List.map (fun p -> p.FromRcd), x.Range)
and SynPatParenRcd with
    member x.FromRcd = SynPat.Paren(x.Pattern.FromRcd, x.Range)
and SynPatNullRcd with
    member x.FromRcd = SynPat.Null(x.Range)
and SynPatOptionalValRcd with
    member x.FromRcd = SynPat.OptionalVal(x.Id, x.Range)
and SynPatOrRcd with
    member x.FromRcd = SynPat.Or(x.Left.FromRcd, x.Right.FromRcd, x.Range)
and SynPatAndsRcd with
    member x.FromRcd = SynPat.Ands(x.Patterns |> List.map (fun pat -> pat.FromRcd), x.Range)
and SynPatArrayOrListRcd with
    member x.FromRcd = SynPat.ArrayOrList(x.IsArray, x.Elements |> List.map (fun pat -> pat.FromRcd), x.Range)
and SynPatIsInstanceRcd with
    member x.FromRcd = SynPat.IsInst(x.Type, x.Range)
and SynPatRecordRcd with
    member x.FromRcd =
        let fields = [ for ((idents, ident), pattern) in x.Fields -> ((idents, ident), pattern.FromRcd) ]
        SynPat.Record(fields, x.Range)
and SynPatQuoteExprRcd with
    member x.FromRcd = SynPat.QuoteExpr(x.Expr, x.Range)
and SynPatDeprecatedCharRangeRcd with
    member x.FromRcd = SynPat.DeprecatedCharRange(x.StartChar, x.EndChar, x.Range)
and SynPatInstanceMemberRcd with
    member x.FromRcd = SynPat.InstanceMember(x.ThisId, x.MemberId, x.ToolingId, x.Accessibility, x.Range)
and SynPatFromParseErrorRcd with
    member x.FromRcd = SynPat.FromParseError(x.Pattern.FromRcd, x.Range)
type SynPat with
    member x.ToRcd =
        match x with
        | SynPat.Const(cnst, range) ->
            SynPatRcd.Const { Const = cnst; Range = range }
        | SynPat.Wild range ->
            SynPatRcd.Wild { Range = range }
        | SynPat.Named(pattern, id, isThis, access, range) ->
            SynPatRcd.Named { Pattern = pattern.ToRcd; Id = id; IsThis = isThis; Access = access; Range = range }
        | SynPat.Typed(pattern, typ, range) ->
            SynPatRcd.Typed { Pattern = pattern.ToRcd; Type = typ; Range = range }
        | SynPat.Attrib(pattern, attributes, range) ->
            SynPatRcd.Attrib { Pattern = pattern.ToRcd; Attributes = attributes; Range = range }
        | SynPat.Or(left, right, range) ->
            SynPatRcd.Or { Left = left.ToRcd; Right = right.ToRcd; Range = range }
        | SynPat.Ands(patterns, range) ->
            SynPatRcd.Ands { Patterns = patterns |> List.map (fun pat -> pat.ToRcd); Range = range }
        | SynPat.LongIdent(id, extraId, typarDecls , args, access, range) ->
            SynPatRcd.LongIdent { Id = id; ExtraId = extraId; TyparDecls = typarDecls; Args = args; Access = access; Range = range }
        | SynPat.Tuple(_, patterns, range) ->
            SynPatRcd.Tuple { Patterns = patterns |> List.map (fun p -> p.ToRcd); Range = range }
        | SynPat.Paren(pattern, range) ->
            SynPatRcd.Paren { Pattern = pattern.ToRcd; Range = range }
        | SynPat.ArrayOrList(isArray, elementPatterns, range) ->
            SynPatRcd.ArrayOrList {
                IsArray = isArray
                Elements = elementPatterns |> List.map (fun p -> p.ToRcd)
                Range = range
            }
        | SynPat.Record(fields, range) ->
            SynPatRcd.Record {
                Fields = [
                    for ((idents, ident), pat) in fields ->
                        ((idents, ident), pat.ToRcd)
                ]

                Range = range
            }

        | SynPat.Null range ->
            SynPatRcd.Null { Range = range }
        | SynPat.OptionalVal (ident, range) ->
            SynPatRcd.OptionalVal { Id = ident; Range = range }
        | SynPat.IsInst(typeToTest, range) ->
            SynPatRcd.IsInstance {
                Type = typeToTest
                Range = range
            }

        | SynPat.QuoteExpr(expr, range) ->
            SynPatRcd.QuoteExpr {
                Expr = expr
                Range = range
            }

        | SynPat.DeprecatedCharRange(startChar, endChar, range) ->
            SynPatRcd.DeprecatedCharRange {
                StartChar = startChar
                EndChar = endChar
                Range = range
            }

        | SynPat.InstanceMember(thisId, memberId, toolingId, accessibility, range) ->
            SynPatRcd.InstanceMember {
                ThisId = thisId
                MemberId = memberId
                ToolingId = toolingId
                Accessibility = accessibility
                Range = range
            }

        | SynPat.FromParseError(pattern, range) ->
            SynPatRcd.FromParseError {
                Pattern = pattern.ToRcd
                Range = range
            }

type SynBindingReturnInfoRcd = {
    Type: SynType
    Range: range
    Attributes: SynAttributes
    }
with
    member x.FromRcd = SynBindingReturnInfo(x.Type, x.Range, x.Attributes)

type SynBindingReturnInfo with
    member x.ToRcd =
        let (SynBindingReturnInfo(typ, range, attributes)) = x
        { Type = typ; Range = range; Attributes = attributes }

type SynBindingRcd = {
    Access: SynAccess option
    Kind: SynBindingKind
    IsInline: bool
    IsMutable: bool
    Attributes: SynAttributes
    XmlDoc: PreXmlDoc
    ValData: SynValData
    Pattern: SynPatRcd
    ReturnInfo: SynBindingReturnInfoRcd option
    Expr: SynExpr
    Range: range
    Bind: DebugPointForBinding }
with
    member x.FromRcd =
        Binding(x.Access, x.Kind, x.IsInline, x.IsMutable, x.Attributes, x.XmlDoc, x.ValData, x.Pattern.FromRcd, x.ReturnInfo |> Option.map (fun ri -> ri.FromRcd), x.Expr, x.Range, x.Bind)

type SynBinding with
    member x.ToRcd =
        let (Binding(access, kind, isInline, isMutable, attrs, xmlDoc, info, pattern, returnInfo, rhsExpr, mBind, spBind)) = x
        { Access = access; Kind = kind; IsInline = isInline; IsMutable = isMutable; Attributes = attrs; XmlDoc = xmlDoc; ValData = info; Pattern = pattern.ToRcd; ReturnInfo = returnInfo |> Option.map (fun ri -> ri.ToRcd); Expr = rhsExpr; Range = mBind; Bind = spBind }

[<RequireQualifiedAccess>]
type SynTypeDefnSimpleReprRcd =
    | Union of SynTypeDefnSimpleReprUnionRcd
    | Enum of SynTypeDefnSimpleReprEnumRcd
    | Record of SynTypeDefnSimpleReprRecordRcd
    | General of SynTypeDefnSimpleReprGeneralRcd
    | LibraryOnlyILAssembly of SynTypeDefnSimpleReprLibraryOnlyILAssemblyRcd
    | TypeAbbrev of SynTypeDefnSimpleReprTypeAbbrevRcd
    | None of SynTypeDefnSimpleReprNoneRcd

and SynTypeDefnSimpleReprUnionRcd = {
    Access: SynAccess option
    Cases: SynUnionCase list
    Range: range }

and SynTypeDefnSimpleReprEnumRcd = {
    Cases: SynEnumCase list
    Range: range }

and  SynTypeDefnSimpleReprRecordRcd = {
    Access: SynAccess option
    Fields: SynField list
    Range: range }

and SynTypeDefnSimpleReprGeneralRcd = {
    Kind: SynTypeDefnKind
    // TODO incomplete
    // (SynType * range * Ident option) list
    // (SynValSig * MemberFlags) list
    // SynField list
    // bool
    // bool
    // SynSimplePat list option
    Range: range }

and SynTypeDefnSimpleReprLibraryOnlyILAssemblyRcd = {
    ILType: FSharp.Compiler.AbstractIL.IL.ILType
    Range: range }

and SynTypeDefnSimpleReprTypeAbbrevRcd = {
    ParseDetail: FSharp.Compiler.SyntaxTree.ParserDetail
    Type: SynType
    Range: range }

and SynTypeDefnSimpleReprNoneRcd = {
    Range: range }

type SynTypeDefnSimpleReprRcd with
    member x.FromRcd =
        match x with
        | Union u -> u.FromRcd
        | Enum e -> e.FromRcd
        | Record r -> r.FromRcd
        | General g -> g.FromRcd
        | LibraryOnlyILAssembly a -> a.FromRcd
        | TypeAbbrev a -> a.FromRcd
        | None n -> n.FromRcd
and SynTypeDefnSimpleReprUnionRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.Union(x.Access, x.Cases, x.Range)
and SynTypeDefnSimpleReprEnumRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.Enum(x.Cases, x.Range)
and SynTypeDefnSimpleReprRecordRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.Record(x.Access, x.Fields, x.Range)
and SynTypeDefnSimpleReprGeneralRcd with
    member x.FromRcd =  SynTypeDefnSimpleRepr.General(x.Kind, [], [], [], false, false, Option.None, x.Range) // TODO
and SynTypeDefnSimpleReprLibraryOnlyILAssemblyRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(x.ILType, x.Range)
and SynTypeDefnSimpleReprTypeAbbrevRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.TypeAbbrev(x.ParseDetail, x.Type, x.Range)
and SynTypeDefnSimpleReprNoneRcd with
    member x.FromRcd = SynTypeDefnSimpleRepr.None(x.Range)

type SynTypeDefnSimpleRepr with
    member x.ToRcd =
        match x with
        | SynTypeDefnSimpleRepr.Union(access, cases, range) ->
            SynTypeDefnSimpleReprRcd.Union { Access = access; Cases = cases; Range = range }
        | SynTypeDefnSimpleRepr.Enum(cases, range) ->
            SynTypeDefnSimpleReprRcd.Enum { Cases = cases; Range = range }
        | SynTypeDefnSimpleRepr.Record(access, fields, range) ->
            SynTypeDefnSimpleReprRcd.Record { Access = access; Fields = fields; Range = range }
        | SynTypeDefnSimpleRepr.General(kind, _, _, _, _ , _, _, range) -> // TODO
            SynTypeDefnSimpleReprRcd.General { Kind = kind; Range = range }
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(iltype, range) ->
            SynTypeDefnSimpleReprRcd.LibraryOnlyILAssembly { ILType = unbox iltype; Range = range }
        | SynTypeDefnSimpleRepr.TypeAbbrev(parseDetail, typ, range) ->
            SynTypeDefnSimpleReprRcd.TypeAbbrev { ParseDetail = parseDetail; Type = typ; Range = range }
        | SynTypeDefnSimpleRepr.None(range) ->
            SynTypeDefnSimpleReprRcd.None { Range = range }
        | SynTypeDefnSimpleRepr.Exception _ -> failwith "not supported"

type SynEnumCaseRcd = {
    Attributes: SynAttributes
    Id: Ident
    Constant: SynConst
    XmlDoc: PreXmlDoc
    Range: range }
with
    member x.FromRcd =
        SynEnumCase.EnumCase(x.Attributes, x.Id, x.Constant, x.XmlDoc, x.Range)

type SynEnumCase with
    member x.ToRcd =
        match x with
        | EnumCase(attributes, id, constant, xmlDoc, range) ->
            { Attributes = attributes; Id = id; Constant = constant; XmlDoc = xmlDoc; Range = range }

type XmlDoc with
    member x.Lines =
        x.GetElaboratedXmlLines()

type PreXmlDoc with
    member x.Lines: string []  = x.Lines

type SynUnionCaseRcd = {
    Attributes: SynAttributes
    Id: Ident
    Type: SynUnionCaseType
    XmlDoc: PreXmlDoc
    Access: SynAccess option
    Range: range }
with
    member x.FromRcd =
        SynUnionCase.UnionCase(x.Attributes, x.Id, x.Type, x.XmlDoc, x.Access, x.Range)
    member x.HasFields =
        match x.Type with
        | UnionCaseFields cases -> not cases.IsEmpty
        | _ -> false

type SynUnionCase with
    member x.ToRcd : SynUnionCaseRcd =
        match x with
        | SynUnionCase.UnionCase(attributes, id, typ, xmlDoc, access, range) ->
            { Attributes = attributes; Id = id; Type = typ; XmlDoc = xmlDoc; Access = access; Range = range }

type SynFieldRcd = {
    Attributes: SynAttributes
    IsStatic: bool
    Id: Ident option
    Type: SynType
    IsMutable: bool
    XmlDoc: PreXmlDoc
    Access: SynAccess option
    Range: range }
with
    member x.FromRcd =
        SynField.Field(x.Attributes, x.IsStatic, x.Id, x.Type, x.IsMutable, x.XmlDoc, x.Access, x.Range)

type SynField with
    member x.ToRcd: SynFieldRcd =
        match x with
        | SynField.Field(attributes, isstatic, id, typ, ismutable, xmlDoc, access, range) ->
             { Attributes = attributes
               IsStatic = isstatic
               Id = id
               Type = typ
               IsMutable = ismutable
               XmlDoc = xmlDoc
               Access = access
               Range = range }

