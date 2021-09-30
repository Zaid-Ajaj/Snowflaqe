[<AutoOpen>]
module Snowflaqe.LinqToXmlExtensions

open System
open System.Xml.Linq

type XAttribute with

    static member ofStringName (name: string, value: obj) =
        XAttribute(XName.Get(name), value)

type XElement with

    static member ofStringName (name: string, content: obj) =
        XElement(XName.Get(name), content)

    static member ofStringName (name: string, [<ParamArray>] content) =
        XElement(XName.Get(name), content)

type MSBuildXElement () =
    static member Compile(fileName: string) =
        XElement.ofStringName("Compile", XAttribute.ofStringName("Include", fileName))

    static member Import (projectPath: string) =
        XElement.ofStringName("Import",
            XAttribute.ofStringName("Project", projectPath))

    static member PackageReferenceInclude (include': string, version: string) =
        XElement.ofStringName("PackageReference",
            XAttribute.ofStringName("Include", include'),
            XAttribute.ofStringName("Version", version))

    static member PackageReferenceUpdate (update: string, version: string) =
        XElement.ofStringName("PackageReference",
            XAttribute.ofStringName("Update", update),
            XAttribute.ofStringName("Version", version))

    static member ProjectReference (include': string) =
        XElement.ofStringName("ProjectReference",
            XAttribute.ofStringName("Include", include'))

    static member PropertyGroup (copyLocalLockFileAssemblies: bool) =
        XElement.ofStringName("PropertyGroup",
            XElement.ofStringName("CopyLocalLockFileAssemblies",
                (copyLocalLockFileAssemblies.ToString().ToLower())))

    static member PropertyGroup ([<ParamArray>] content) =
        XElement.ofStringName("PropertyGroup", content)

type XDocument with

    member this.WriteTo (outputFileName: string) =
        System.IO.File.WriteAllText(outputFileName, this.ToString())

