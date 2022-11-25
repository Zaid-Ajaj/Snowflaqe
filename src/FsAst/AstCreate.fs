[<AutoOpen>]
module FsAst.AstCreate
open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml

type range = FSharp.Compiler.Text.range
let range0 = range.Zero
let mkPos = FSharp.Compiler.Text.Position.mkPos
let mkRange = FSharp.Compiler.Text.Range.mkRange
type Ident with
    static member Create text =
        Ident(text, range.Zero)
    static member CreateLong (text: string) =
        text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

type SynIdent with
        static member Create (text: string) =
            SynIdent(Ident(text, range.Zero), None)
        static member Create (text, trivia) =
            SynIdent(Ident(text, range.Zero), trivia)
        static member CreateLong (text: string) =
            text.Split([|'.'|]) |> List.ofArray |> List.map SynIdent.Create
        member x.idRange =
            let (SynIdent (ident, _)) = x in ident.idRange
            member x.idText =
                let (SynIdent (ident, _)) = x in ident.idText

type SynLongIdent with
    static member Create texts =
        SynLongIdent(texts |> List.map Ident.Create, [], [])
    static member CreateString (text: string) =
        SynLongIdent(Ident.CreateLong text, [], [])
    static member CreateFromLongIdent (longIdent: LongIdent) =
        SynLongIdent(longIdent, [], [])

    member x.AsString =
        let sb = Text.StringBuilder()
        for i in 0 .. x.LongIdent.Length - 2 do
            sb.Append x.LongIdent.[i].idText |> ignore
            sb.Append '.' |> ignore
        sb.Append x.LongIdent.[x.LongIdent.Length-1].idText |> ignore
        sb.ToString()

type SynPatLongIdentRcd with
    static member Create (id, args, ?extraId, ?typarDecls, ?access) =
        { Id = id; ExtraId = extraId; TyparDecls = typarDecls ; Args = args; Access = access; Range = range.Zero }

type SynArgPats with
    static member Empty =
        SynArgPats.Pats[]

type SynPatRcd with
    static member CreateLongIdent (id, args: SynPatRcd list, ?access) =
        SynPatRcd.LongIdent ( {SynPatLongIdentRcd.Create(id, args |> List.map (fun a -> a.FromRcd) |> SynArgPats.Pats ) with Access = access } )
    static member CreateTuple patterns =
        SynPatRcd.Tuple { Patterns = patterns; Range = range.Zero }
    static member CreateParen pattern =
        SynPatRcd.Paren { Pattern = pattern; Range = range.Zero }
    static member CreateAttrib (pattern, attributes) =
        SynPatRcd.Attrib { Pattern = pattern; Attributes = attributes; Range = range.Zero }
    static member CreateTyped (pattern, typ) =
        SynPatRcd.Typed { Pattern = pattern; Type = typ; Range = range.Zero }
    static member CreateNamed id =
        SynPatRcd.Named { Id = id; IsThis = false; Access = None; Range = range.Zero }
    static member CreateWild =
        SynPatRcd.Wild { Range = range.Zero }

type QualifiedNameOfFile with
    static member Create name =
        QualifiedNameOfFile(Ident.Create name)

type SynMemberFlags with
    static member InstanceMember : SynMemberFlags =
        { GetterOrSetterIsCompilerGenerated = false; IsInstance = true; MemberKind = SynMemberKind.Member; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false }
    static member StaticMember =
        { SynMemberFlags.InstanceMember with IsInstance = false }

type SynConst with
    /// Creates a <see href="SynStringKind.Regular">Regular</see> string
    static member CreateString s =
        SynConst.String(s, SynStringKind.Regular, range.Zero)

type SynExpr with
    static member CreateConst cnst =
        SynExpr.Const(cnst, range.Zero)
    static member CreateConstString s =
        SynExpr.CreateConst (SynConst.CreateString s)
    static member CreateTyped (expr, typ) =
        SynExpr.Typed(expr, typ, range.Zero)
    static member CreateApp (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range.Zero)
    static member CreateAppInfix (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range.Zero)
    static member CreateIdent id =
        SynExpr.Ident(id)
    static member CreateIdentString id =
        SynExpr.Ident(Ident.Create id)
    static member CreateLongIdent (isOptional, id, altNameRefCell) =
        SynExpr.LongIdent(isOptional, id, altNameRefCell, range.Zero)
    static member CreateLongIdent id =
        SynExpr.CreateLongIdent(false, id, None)
    static member CreateParen expr =
        SynExpr.Paren(expr, range.Zero, None, range.Zero)
    static member CreateTuple list =
        SynExpr.Tuple(false, list, [], range.Zero)
    static member CreateParenedTuple list =
        SynExpr.CreateTuple list
        |> SynExpr.CreateParen
    static member CreateUnit =
        SynExpr.CreateConst SynConst.Unit
    static member CreateNull =
        SynExpr.Null(range.Zero)
    static member CreateRecord (fields: list<RecordFieldName * option<SynExpr>>) =
        let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range.Zero)
    static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates) =
        let fields = fieldUpdates |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range.Zero)
    static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates ) =
        let blockSep = (range.Zero, None) : BlockSeparator
        let copyInfo = Some (copyInfo, blockSep)
        SynExpr.Record (None, copyInfo, fieldUpdates, range.Zero)
    /// Creates:
    ///
    /// ```
    /// match matchExpr with
    /// | clause1
    /// | clause2
    /// ...
    /// | clauseN
    /// ```
    static member CreateMatch(matchExpr, clauses) =
        SynExpr.Match(DebugPointAtBinding.Yes range0, matchExpr, clauses, range0, { MatchKeyword = range.Zero; WithKeyword = range.Zero })
    /// Creates : `instanceAndMethod(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        SynExpr.CreateApp(valueExpr, args)
    /// Creates : `instanceAndMethod()`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent) =
        SynExpr.CreateInstanceMethodCall(instanceAndMethod, SynExpr.CreateUnit)
    /// Creates : `instanceAndMethod<type1, type2,... type}>(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, instanceMethodsGenericTypes, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range.Zero, instanceMethodsGenericTypes, [], None, range.Zero, range.Zero)
        SynExpr.CreateApp(valueExprWithType, args)
    /// Creates: expr1; expr2; ... exprN
    static member CreateSequential exprs =
        let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.SuppressBoth, false, expr1, expr2, range0)
        let rec inner exprs state =
            match state, exprs with
            | None, [] -> SynExpr.CreateConst SynConst.Unit
            | Some expr, [] -> expr
            | None, [single] -> single
            | None, [one;two] -> seqExpr one two
            | Some exp, [single] -> seqExpr exp single
            | None, head::shoulders::tail ->
                seqExpr head shoulders
                |> Some
                |> inner tail
            | Some expr, head::tail ->
                seqExpr expr head
                |> Some
                |> inner tail
        inner exprs None

type SynType with
    static member CreateApp (typ, args, ?isPostfix) =
        SynType.App(typ, None, args, [], None, (defaultArg isPostfix false), range.Zero)
    static member CreateLongIdent id =
        SynType.LongIdent(id)
    static member CreateLongIdent s =
        SynType.CreateLongIdent(SynLongIdent.CreateString s)
    static member CreateUnit =
        SynType.CreateLongIdent("unit")
    static member CreateFun (fieldTypeIn, fieldTypeOut) =
        SynType.Fun (fieldTypeIn, fieldTypeOut, range.Zero, { ArrowRange = range.Zero})

    static member Create(name: string) = SynType.CreateLongIdent name

    static member Option(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member ResizeArray(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "ResizeArray",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Set(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Set",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member NativePointer(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "nativeptr",
            typeArgs=[ inner ],
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

    static member Dictionary(key, value) =
        SynType.App(
            typeName=SynType.LongIdent(SynLongIdent.Create [ "System"; "Collections"; "Generic"; "Dictionary" ]),
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Map(key, value) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Map",
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member List(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=None,
            lessRange=None
        )

    static member Array(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "array",
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
        SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTimeOffset" ])

    static member DateTime() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTime" ])

    static member Guid() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "Guid" ])

    static member Int() =
        SynType.Create "int"

    static member UInt() =
        SynType.Create "uint"

    static member Int8() =
        SynType.Create "int8"

    static member UInt8() =
        SynType.Create "uint8"

    static member Int16() =
        SynType.Create "int16"

    static member UInt16() =
        SynType.Create "uint16"

    static member Int64() =
        SynType.Create "int64"

    static member UInt64() =
        SynType.Create "uint64"

    static member String() =
        SynType.Create "string"

    static member Bool() =
        SynType.Create "bool"

    static member Float() =
        SynType.Create "float"

    static member Float32() =
        SynType.Create "float32"

    static member Double() =
        SynType.Create "float"

    static member Decimal() =
        SynType.Create "decimal"

    static member Unit() =
        SynType.Create "unit"

    static member BigInt() =
        SynType.Create "bigint"

    static member Byte() =
        SynType.Create "byte"

    static member Char() =
        SynType.Create "char"

type SynArgInfo with
    static member Empty =
        SynArgInfo(SynAttributes.Empty, false, None)
    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)
    static member CreateIdString id =
        SynArgInfo.CreateId(Ident.Create id)

type SynPatRcd with
    static member CreateNull =
        SynPatRcd.Null { Range = range.Zero }

type SynValInfo with
    static member Empty =
        SynValInfo([], SynArgInfo.Empty)

type SynBindingReturnInfoRcd with
    static member Create typ =
        { Type = typ; Range = range.Zero; Attributes = []; Trivia = { ColonRange = None } }

type SynBindingRcd with
    static member Null =
        {   Access = None
            Kind = SynBindingKind.Normal
            IsInline = false
            IsMutable = false
            Attributes = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some SynMemberFlags.InstanceMember, SynValInfo.Empty, None)
            Pattern = SynPatRcd.CreateNull
            ReturnInfo = None
            Expr = SynExpr.Null range.Zero
            Range = range.Zero
            Bind = DebugPointAtBinding.NoneAtInvisible
            Trivia = SynBindingTrivia.Zero
        }
    static member Let =
        { SynBindingRcd.Null with
            ValData = SynValData(None, SynValInfo([], SynArgInfo.Empty), None)
            Expr = SynExpr.CreateTyped(SynExpr.CreateNull, SynType.CreateUnit)
        }

type SynComponentInfoRcd with
    static member Create id =
        {   Attributes = SynAttributes.Empty
            Parameters = None
            Constraints = []
            Id = id
            XmlDoc = PreXmlDoc.Empty
            PreferPostfix = false
            Access = None
            Range = range.Zero
        }

type SynMemberDefn with
    static member CreateImplicitCtor (ctorArgs) =
        SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, SynSimplePats.SimplePats(ctorArgs, range0), None, PreXmlDoc.Empty, range.Zero )
    static member CreateImplicitCtor() =
        SynMemberDefn.CreateImplicitCtor []

    /// <summary>
    /// Creates an instance member from a binding definition: [member {binding} = {expr}]
    /// where {binding} = {this.pattern args} and {expr} is the body of the binding
    /// </summary>
    static member CreateMember (binding:SynBindingRcd) =
        SynMemberDefn.Member(binding.FromRcd, range.Zero)

    /// <summary>
    /// Creates a member from a binding definition: [static member {binding} = {expr}]
    /// where {binding} = {pattern args} and {expr} is the body of the static binding
    /// </summary>
    static member CreateStaticMember(binding:SynBindingRcd) =
        let (SynValData(usedMemberFlags, valInfo, identifier)) = binding.ValData
        let staticMemberFlags: SynMemberFlags option = Some {
            GetterOrSetterIsCompilerGenerated = false
            // this means the member is static
            IsInstance = false;
            IsOverrideOrExplicitImpl = false
            IsDispatchSlot = false;
            IsFinal = false
            MemberKind = SynMemberKind.Member
        }
        let staticBinding = { binding with ValData = SynValData.SynValData(staticMemberFlags, valInfo, identifier) }
        SynMemberDefn.Member(staticBinding.FromRcd, range.Zero)

    /// <summary>
    /// Creates an instance member from a binding definition: [override {binding} = {expr}]
    /// where {binding} = {this.pattern args} and {expr} is the body of the static binding
    /// </summary>
    static member CreateOverrideMember(binding:SynBindingRcd) =
        let (SynValData(usedMemberFlags, valInfo, identifier)) = binding.ValData
        let overrideMemberFlags: SynMemberFlags option = Some {
            GetterOrSetterIsCompilerGenerated = false
            IsInstance = true;
            IsOverrideOrExplicitImpl = true
            IsDispatchSlot = false;
            IsFinal = false
            MemberKind = SynMemberKind.Member
        }
        let overrideBinding = { binding with ValData = SynValData.SynValData(overrideMemberFlags, valInfo, identifier) }
        SynMemberDefn.Member(overrideBinding.FromRcd, range.Zero)

    static member CreateInterface(interfaceType, members) =
        SynMemberDefn.Interface(interfaceType, None, members, range.Zero)

type SynTypeDefnReprObjectModelRcd with
    static member Create members =
        {   //Kind = SynTypeDefnKind.TyconClass
            Kind = SynTypeDefnKind.Unspecified
            Members = members
            Range = range.Zero
        }

type SynTypeDefnRcd with
    static member Create (info: SynComponentInfoRcd, members) =
        {   Info = info
            Repr = SynTypeDefnReprObjectModelRcd.Create(members).FromRcd
            Members = []
            ImplicitConstructor = None
            Range = range.Zero
            Trivia = SynTypeDefnTrivia.Zero
        }
    static member CreateSimple (info: SynComponentInfoRcd, simple: SynTypeDefnSimpleRepr, ?members) =
        {   Info = info
            Repr =  SynTypeDefnRepr.Simple(simple, range.Zero)
            Members = Option.defaultValue [] members
            ImplicitConstructor = None
            Range = range.Zero
            Trivia = { SynTypeDefnTrivia.Zero with LeadingKeyword = SynTypeDefnLeadingKeyword.Type range.Zero }
        }
// type Temp = SynModuleDelNestedModuleTrivia
type SynModuleDecl with
    static member CreateType (info, members) =
        SynModuleDecl.Types([SynTypeDefnRcd.Create(info, members).FromRcd], range.Zero)
    static member CreateSimpleType (info, simple: SynTypeDefnSimpleReprRcd, ?members) =
        SynModuleDecl.Types( [SynTypeDefnRcd.CreateSimple(info, simple.FromRcd, members = Option.defaultValue [] members).FromRcd], range.Zero)
    static member CreateOpen id =
        SynModuleDecl.Open(id, range.Zero)
    static member CreateOpen (fullNamespaceOrModuleName: string) =
        SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent.CreateFromLongIdent(Ident.CreateLong fullNamespaceOrModuleName), range.Zero), range.Zero)
    static member CreateHashDirective (directive, values) =
        SynModuleDecl.HashDirective (ParsedHashDirective (directive, values, range.Zero), range.Zero)
    static member CreateLet (bindings: SynBindingRcd list) =
        SynModuleDecl.Let(false, bindings |> List.map(fun b -> b.FromRcd), range.Zero)
    static member CreateAttribute(ident, expr, isProp, ?target) =
            { SynAttribute.TypeName = ident
              SynAttribute.ArgExpr = expr
              SynAttribute.Target = target
              SynAttribute.AppliesToGetterAndSetter = isProp
              SynAttribute.Range = range.Zero }
    static member CreateAttributes(attributes) =
        SynModuleDecl.Attributes(attributes, range.Zero)
    static member CreateNestedModule(info : SynComponentInfoRcd, members) =
        SynModuleDecl.NestedModule(info.FromRcd, false, members, false, range.Zero, { ModuleKeyword = None; EqualsRange = None})
    static member CreateTypes (types: SynTypeDefnRcd list) =
        SynModuleDecl.Types(types |> List.map (fun t -> t.FromRcd), range.Zero)

type SynModuleOrNamespaceRcd with
    static member CreateModule id =
        {   Id = id
            IsRecursive = false
            Kind = SynModuleOrNamespaceKind.NamedModule
            Declarations = []
            XmlDoc = PreXmlDoc.Empty
            Attributes = SynAttributes.Empty
            Access = None
            Range = range.Zero
            Trivia = { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
        }
    static member CreateNamespace id =
        { SynModuleOrNamespaceRcd.CreateModule id with
            Kind = SynModuleOrNamespaceKind.DeclaredNamespace
        }
    member x.AddDeclarations decls =
        { x with
            Declarations = List.append x.Declarations decls
        }
    member x.AddDeclaration decl =
        x.AddDeclarations [decl]

type ParsedImplFileInputRcd with
    static member CreateFs name =
        {   File = sprintf "%s.fs" name
            IsScript = false
            QualName = QualifiedNameOfFile.Create name
            Pragmas = []
            HashDirectives = []
            Modules = []
            IsLastCompiland = true
            IsExe = false
            Trivia = { CodeComments = []; ConditionalDirectives = [] }
        }
    member x.AddModules (modules: SynModuleOrNamespaceRcd list) =
        { x with
            Modules = List.append x.Modules (modules |> List.map (fun m -> m.FromRcd))
        }

    member x.AddModule mdl =
        x.AddModules [mdl]

type ParsedInput with
    static member CreateImplFile (implFile: ParsedImplFileInputRcd) =
        ParsedInput.ImplFile implFile.FromRcd

type SynTypeDefnSimpleReprEnumRcd with
    static member Create (cases: SynEnumCaseRcd list) =
        { Cases = cases |> List.map (fun c -> c.FromRcd)
          Range = range.Zero }

type SynTypeDefnSimpleReprRecordRcd with
    static member Create (fields: SynFieldRcd list) =
        { Access = None
          Fields = (fields |> List.map (fun f -> f.FromRcd))
          Range = range.Zero }

type SynTypeDefnSimpleReprUnionRcd with
    static member Create cases =
        { Access = None; Cases = cases; Range = range.Zero }

    static member Create (fields: SynUnionCaseRcd list) : SynTypeDefnSimpleReprUnionRcd=
        { Access = None
          Cases = fields |> List.map (fun f -> f.FromRcd)
          Range = range.Zero }

type SynUnionCaseRcd with
    static member Create(id, typ) : SynUnionCaseRcd =
        { Attributes = SynAttributes.Empty
          Id = id
          Type = typ
          XmlDoc = PreXmlDoc.Empty
          Access = None
          Range = range.Zero
          Trivia = { BarRange = None} }

type SynUnionCaseKind with
    static member Create(synFieldList : SynFieldRcd list) =
        SynUnionCaseKind.Fields(synFieldList |> List.map (fun sf -> sf.FromRcd))

type SynEnumCaseRcd with
    static member Create (id, cnst) =
        {   Attributes = SynAttributes.Empty
            Id = id
            Constant = cnst
            ValueRange = range.Zero
            XmlDoc = PreXmlDoc.Empty
            Range = range.Zero
            Trivia = { BarRange = None; EqualsRange = range.Zero }
        }

type SynFieldRcd with
    static member Create(id, typ, ?isMutable) : SynFieldRcd =
        let isMutable = defaultArg isMutable false
        {   Attributes = SynAttributes.Empty
            IsStatic = false
            Id = Some id
            Type = typ
            IsMutable = isMutable
            XmlDoc = PreXmlDoc.Empty
            Access = None
            Range = range.Zero
            Trivia = SynFieldTrivia.Zero
        }
    static member Create(id, typ) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent typ)
    static member CreateInt(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "int")
    static member CreateIntOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "int" ]) ]
    static member CreateString(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "string")
    static member CreateStringOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "string" ]) ]
    static member CreateFloat(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "float")
    static member CreateFloatOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "float" ]) ]
    static member CreateBool(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "bool")
    static member CreateBoolOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "bool" ]) ]
    static member CreateDecimal(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "decimal")
    static member CreateDecimalOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "decimal" ]) ]
    static member CreateOption(id, optional) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ optional ]) ]
    static member CreateApp id typ args =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateApp(SynType.CreateLongIdent typ, args |> List.map (SynType.CreateLongIdent)))

type SynAttributeList with
    static member Create(attrs): SynAttributeList =
        {
            Attributes = attrs
            Range = range0
        }

    static member Create(attr): SynAttributeList =
        {
            Attributes = [ attr ]
            Range = range0
        }

    static member Create([<ParamArray>] attrs): SynAttributeList =
        {
            Attributes = List.ofArray attrs
            Range = range0
        }

type SynAttribute with
    static member Create(name: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Unit, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ Ident.Create name ], [ ], [ ])
        }

    static member Create(name: string, argument: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ Ident.Create name ], [ ], [ ])
        }

    static member Create(name: string, argument: bool) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ Ident.Create name ], [ ], [ ])
        }

    static member Create(name: string, argument: int) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ Ident.Create name ], [ ], [ ])
        }

    static member Create(name: string, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ Ident.Create name ], [ ], [ ])
        }

    static member Create(name: Ident, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([ name ], [ ], [ ])
        }

    static member Create(name: Ident list, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent(name, [ ], [ ])
        }

    static member RequireQualifiedAccess() =
        SynAttribute.Create("RequireQualifiedAccess")
    
    static member CLIMutable() =
        SynAttribute.Create("CLIMutable")

    static member CompiledName(valueArg: string) =
        SynAttribute.Create("CompiledName", valueArg)

type PreXmlDoc with
    static member Create (lines: string list) =
        let lines = List.toArray lines
        let lineMaxIndex = Array.length lines - 1
        let s = mkPos 0 0
        let e = mkPos lineMaxIndex 0
        let containingRange = mkRange "" s e
        PreXmlDoc.Create(lines, containingRange)

    static member Create (docs: string option) =
        PreXmlDoc.Create [
            if docs.IsSome
            then docs.Value
        ]

    static member Create(docs: string) =
        PreXmlDoc.Create [
            if not (String.IsNullOrWhiteSpace docs)
            then docs
        ]

type SynSimplePat with
    static member CreateTyped(ident, ``type``) =
        let ssp = SynSimplePat.Id(ident, None, false, false, false, range.Zero)
        SynSimplePat.Typed(ssp, ``type``, range.Zero )

    static member CreateId(ident, ?altNameRefCell, ?isCompilerGenerated, ?isThis, ?isOptional) =
        SynSimplePat.Id(ident, altNameRefCell,
                        Option.defaultValue false isCompilerGenerated,
                        Option.defaultValue false isThis,
                        Option.defaultValue false isOptional,
                        range.Zero)

type SynSimplePats with
    static member Create(patterns) =
        SynSimplePats.SimplePats(patterns, range0)
