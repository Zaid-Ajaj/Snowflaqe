[<AutoOpen>]
module FsAst.AstRcd

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml


type range = FSharp.Compiler.Text.range
type ParsedImplFileInputRcd = {
    File: string
    IsScript: bool
    QualName: QualifiedNameOfFile
    Pragmas: ScopedPragma list
    HashDirectives: ParsedHashDirective list
    Modules: SynModuleOrNamespace list
    IsLastCompiland: bool
    IsExe: bool
    Trivia: ParsedImplFileInputTrivia }
with
    member x.FromRcd =
        ParsedImplFileInput(x.File, x.IsScript, x.QualName, x.Pragmas, x.HashDirectives, x.Modules, (x.IsLastCompiland, x.IsExe), x.Trivia)


type ParsedImplFileInput with
    member x.ToRcd =
        let (ParsedImplFileInput(file, isScript, qualName, pragmas, hashDirectives, modules, (isLastCompiland, isExe), trivia)) = x
        { File = file; IsScript = isScript; QualName = qualName; Pragmas = pragmas; HashDirectives = hashDirectives; Modules = modules; IsLastCompiland = isLastCompiland; IsExe = isExe; Trivia = trivia }

type SynModuleOrNamespaceRcd = {
    Id: LongIdent
    IsRecursive: bool
    Kind: SynModuleOrNamespaceKind
    Declarations: SynModuleDecl list
    XmlDoc: PreXmlDoc
    Attributes: SynAttributes
    Access: SynAccess option
    Range: range
    Trivia: SynModuleOrNamespaceTrivia }
with
    member x.FromRcd =
        SynModuleOrNamespace(x.Id, x.IsRecursive, x.Kind, x.Declarations, x.XmlDoc, x.Attributes, x.Access, x.Range, x.Trivia)

type SynModuleOrNamespace with
    member x.ToRcd =
        let (SynModuleOrNamespace(id, isRecursive, kind, declarations, xmlDoc, attributes, access, range, trivia)) = x
        { Id = id; IsRecursive = isRecursive; Kind = kind; Declarations = declarations; XmlDoc = xmlDoc; Attributes = attributes; Access = access; Range = range ; Trivia = trivia}

type SynComponentInfoRcd = {
    Attributes: SynAttributes
    Parameters: SynTyparDecls option
    Constraints: SynTypeConstraint list
    Id: LongIdent
    XmlDoc: PreXmlDoc
    PreferPostfix: bool
    Access: SynAccess option
    Range: range }
with
    member x.FromRcd =
        SynComponentInfo(x.Attributes, x.Parameters, x.Constraints, x.Id, x.XmlDoc, x.PreferPostfix, x.Access, x.Range)

type SynComponentInfo with
    member x.ToRcd =
        let (SynComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)) = x
        { Attributes = attributes; Parameters = parameters; Constraints = constraints; Id = id; XmlDoc = xmldoc; PreferPostfix = preferPostfix; Access = access; Range = range }

type SynTypeDefnRcd = {
    Info: SynComponentInfoRcd
    Repr: SynTypeDefnRepr
    Members: SynMemberDefns
    ImplicitConstructor: SynMemberDefn option
    Range: range
    Trivia: SynTypeDefnTrivia }
with
    member x.FromRcd =
        SynTypeDefn(x.Info.FromRcd, x.Repr, x.Members, x.ImplicitConstructor, x.Range, x.Trivia)

type SynTypeDefn with
    member x.ToRcd =
        let (SynTypeDefn(info, repr, members, implicitConstructor, range, trivia)) = x
        { Info = info.ToRcd; Repr = repr; Members = members; ImplicitConstructor = implicitConstructor; Range = range; Trivia = trivia }

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
    | As of SynPatAsRcd
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
    | ListCons of SynPatListConsRcd

and SynPatConstRcd = {
    Const: SynConst
    Range: range }

and SynPatWildRcd = {
    Range: range }

and SynPatNamedRcd = {
    Id: SynIdent
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
    Range: range
    Trivia: SynPatOrTrivia }

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
    Fields : ((LongIdent * Ident)* range * SynPatRcd) list
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
    Id: SynLongIdent
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

and SynPatAsRcd = {
    Left: SynPatRcd
    Right: SynPatRcd
    Range: range }

and SynPatNullRcd = {
    Range: range }
and SynPatListConsRcd = {
    LhsPat: SynPatRcd
    RhsPat: SynPatRcd
    Range: range
    Trivia: SynPatListConsTrivia }

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
        | As n -> n.FromRcd
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
        | ListCons n -> n.FromRcd

and SynPatConstRcd with
    member x.FromRcd = SynPat.Const(x.Const, x.Range)
and SynPatWildRcd with
    member x.FromRcd = SynPat.Wild(x.Range)
and SynPatNamedRcd with
    member x.FromRcd = SynPat.Named(x.Id, x.IsThis, x.Access, x.Range)
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
and SynPatAsRcd with
    member x.FromRcd = SynPat.As(x.Left.FromRcd, x.Right.FromRcd, x.Range)
and SynPatNullRcd with
    member x.FromRcd = SynPat.Null(x.Range)
and SynPatOptionalValRcd with
    member x.FromRcd = SynPat.OptionalVal(x.Id, x.Range)
and SynPatOrRcd with
    member x.FromRcd = SynPat.Or(x.Left.FromRcd, x.Right.FromRcd, x.Range, x.Trivia)
and SynPatAndsRcd with
    member x.FromRcd = SynPat.Ands(x.Patterns |> List.map (fun pat -> pat.FromRcd), x.Range)
and SynPatArrayOrListRcd with
    member x.FromRcd = SynPat.ArrayOrList(x.IsArray, x.Elements |> List.map (fun pat -> pat.FromRcd), x.Range)
and SynPatIsInstanceRcd with
    member x.FromRcd = SynPat.IsInst(x.Type, x.Range)
and SynPatRecordRcd with
    member x.FromRcd =
        let fields = [ for ((idents, ident), range, pattern) in x.Fields -> ((idents, ident), range, pattern.FromRcd) ]
        SynPat.Record(fields, x.Range)
and SynPatQuoteExprRcd with
    member x.FromRcd = SynPat.QuoteExpr(x.Expr, x.Range)
and SynPatDeprecatedCharRangeRcd with
    member x.FromRcd = SynPat.DeprecatedCharRange(x.StartChar, x.EndChar, x.Range)
and SynPatInstanceMemberRcd with
    member x.FromRcd = SynPat.InstanceMember(x.ThisId, x.MemberId, x.ToolingId, x.Accessibility, x.Range)
and SynPatFromParseErrorRcd with
    member x.FromRcd = SynPat.FromParseError(x.Pattern.FromRcd, x.Range)
and SynPatListConsRcd with
    member x.FromRcd = SynPat.ListCons(x.LhsPat.FromRcd, x.RhsPat.FromRcd, x.Range, x.Trivia)
        
type SynPat with
    member x.ToRcd =
        match x with
        | SynPat.Const(cnst, range) ->
            SynPatRcd.Const { Const = cnst; Range = range }
        | SynPat.Wild range ->
            SynPatRcd.Wild { Range = range }
        | SynPat.Named(id, isThis, access, range) ->
            SynPatRcd.Named { Id = id; IsThis = isThis; Access = access; Range = range }
        | SynPat.Typed(pattern, typ, range) ->
            SynPatRcd.Typed { Pattern = pattern.ToRcd; Type = typ; Range = range }
        | SynPat.Attrib(pattern, attributes, range) ->
            SynPatRcd.Attrib { Pattern = pattern.ToRcd; Attributes = attributes; Range = range }
        | SynPat.Or(left, right, range, trivia) ->
            SynPatRcd.Or { Left = left.ToRcd; Right = right.ToRcd; Range = range; Trivia = trivia }
        | SynPat.Ands(patterns, range) ->
            SynPatRcd.Ands { Patterns = patterns |> List.map (fun pat -> pat.ToRcd); Range = range }
        | SynPat.LongIdent(id, extraId, typarDecls , args, access, range) ->
            SynPatRcd.LongIdent { Id = id; ExtraId = extraId; TyparDecls = typarDecls; Args = args; Access = access; Range = range }
        | SynPat.Tuple(_, patterns, range) ->
            SynPatRcd.Tuple { Patterns = patterns |> List.map (fun p -> p.ToRcd); Range = range }
        | SynPat.Paren(pattern, range) ->
            SynPatRcd.Paren { Pattern = pattern.ToRcd; Range = range }
        | SynPat.As(left, right, range) ->
            SynPatRcd.As { Left = left.ToRcd; Right = right.ToRcd; Range = range }
        | SynPat.ArrayOrList(isArray, elementPatterns, range) ->
            SynPatRcd.ArrayOrList {
                IsArray = isArray
                Elements = elementPatterns |> List.map (fun p -> p.ToRcd)
                Range = range
            }
        | SynPat.Record(fields, range) ->
            SynPatRcd.Record {
                Fields = [
                    for ((idents, ident), fieldRange, pat) in fields ->
                        ((idents, ident), fieldRange, pat.ToRcd)
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
        | SynPat.ListCons(lhsPat, rhsPat, range, trivia) ->
            SynPatRcd.ListCons {
                LhsPat = lhsPat.ToRcd
                RhsPat = rhsPat.ToRcd
                Range = range
                Trivia = trivia
            }

type SynBindingReturnInfoRcd = {
    Type: SynType
    Range: range
    Attributes: SynAttributes
    Trivia: SynBindingReturnInfoTrivia
    }
with
    member x.FromRcd = SynBindingReturnInfo(x.Type, x.Range, x.Attributes, x.Trivia)

type SynBindingReturnInfo with
    member x.ToRcd =
        let (SynBindingReturnInfo(typ, range, attributes, trivia)) = x
        { Type = typ; Range = range; Attributes = attributes; Trivia = trivia }

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
    Bind: DebugPointAtBinding
    Trivia: SynBindingTrivia }
with
    member x.FromRcd =
        SynBinding(x.Access, x.Kind, x.IsInline, x.IsMutable, x.Attributes, x.XmlDoc, x.ValData, x.Pattern.FromRcd, x.ReturnInfo |> Option.map (fun ri -> ri.FromRcd), x.Expr, x.Range, x.Bind, x.Trivia)

type SynBinding with
    member x.ToRcd =
        let (SynBinding(access, kind, isInline, isMutable, attrs, xmlDoc, info, pattern, returnInfo, rhsExpr, mBind, spBind, trivia)) = x
        { Access = access; Kind = kind; IsInline = isInline; IsMutable = isMutable; Attributes = attrs; XmlDoc = xmlDoc; ValData = info; Pattern = pattern.ToRcd; ReturnInfo = returnInfo |> Option.map (fun ri -> ri.ToRcd); Expr = rhsExpr; Range = mBind; Bind = spBind; Trivia = trivia }

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
    ILType: obj // this type is FSharp.Compiler.AbstractIL.IL.ILType but is hidden to avoid the representation of AbstractIL being public
    Range: range }

and SynTypeDefnSimpleReprTypeAbbrevRcd = {
    ParseDetail: FSharp.Compiler.Syntax.ParserDetail
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
            SynTypeDefnSimpleReprRcd.LibraryOnlyILAssembly { ILType = iltype; Range = range }
        | SynTypeDefnSimpleRepr.TypeAbbrev(parseDetail, typ, range) ->
            SynTypeDefnSimpleReprRcd.TypeAbbrev { ParseDetail = parseDetail; Type = typ; Range = range }
        | SynTypeDefnSimpleRepr.None(range) ->
            SynTypeDefnSimpleReprRcd.None { Range = range }
        | SynTypeDefnSimpleRepr.Exception _ -> failwith "not supported"

type SynEnumCaseRcd = {
    Attributes: SynAttributes
    Id: SynIdent
    Constant: SynConst
    ValueRange: Range
    XmlDoc: PreXmlDoc
    Range: range
    Trivia: SynEnumCaseTrivia }
with
    member x.FromRcd =
        SynEnumCase(x.Attributes, x.Id, x.Constant, x.ValueRange, x.XmlDoc, x.Range, x.Trivia)

type SynEnumCase with
    member x.ToRcd =
        match x with
        | SynEnumCase(attributes, id, constant, valueRange, xmlDoc, range, trivia) ->
            { Attributes = attributes; Id = id;  Constant = constant; ValueRange = valueRange; XmlDoc = xmlDoc; Range = range; Trivia = trivia }

type XmlDoc with
    member x.Lines =
        x.GetElaboratedXmlLines()

type PreXmlDoc with
    member x.Lines: string []  =
        x.ToXmlDoc(false, None).Lines

type SynUnionCaseRcd = {
    Attributes: SynAttributes
    Id: SynIdent
    Type: SynUnionCaseKind
    XmlDoc: PreXmlDoc
    Access: SynAccess option
    Range: range
    Trivia: SynUnionCaseTrivia }
with
    member x.FromRcd =
        SynUnionCase(x.Attributes, x.Id, x.Type, x.XmlDoc, x.Access, x.Range, x.Trivia)
    member x.HasFields =
        match x.Type with
        | SynUnionCaseKind.Fields cases -> not cases.IsEmpty
        | _ -> false

type SynUnionCase with
    member x.ToRcd : SynUnionCaseRcd =
        match x with
        | SynUnionCase(attributes, id, typ, xmlDoc, access, range, trivia) ->
            { Attributes = attributes; Id = id; Type = typ; XmlDoc = xmlDoc; Access = access; Range = range; Trivia = trivia }

type SynFieldRcd = {
    Attributes: SynAttributes
    IsStatic: bool
    Id: Ident option
    Type: SynType
    IsMutable: bool
    XmlDoc: PreXmlDoc
    Access: SynAccess option
    Range: range
    Trivia: SynFieldTrivia }
with
    member x.FromRcd =
        SynField.SynField(x.Attributes, x.IsStatic, x.Id, x.Type, x.IsMutable, x.XmlDoc, x.Access, x.Range, x.Trivia)

type SynField with
    member x.ToRcd: SynFieldRcd =
        match x with
        | SynField.SynField(attributes, isstatic, id, typ, ismutable, xmlDoc, access, range, trivia) ->
             { Attributes = attributes
               IsStatic = isstatic
               Id = id
               Type = typ
               IsMutable = ismutable
               XmlDoc = xmlDoc
               Access = access
               Range = range
               Trivia = trivia }

