namespace LinqToXmlExtensions

open System
open System.Xml.Linq

[<AbstractClass; Sealed>]
type XAttribute =

    static member ofStringName (name: string, value: obj) =
        XAttribute(XName.Get(name), value)

[<AbstractClass; Sealed>]
type XElement =

    static member ofStringName (name: string, content: obj) =
        XElement(XName.Get(name), content)

    static member ofStringName (name: string, [<ParamArray>] content) =
        XElement(XName.Get(name), content)

    static member Compile(fileName: string) =
        XElement.ofStringName("Compile", XAttribute.ofStringName("Include", fileName))

    static member PackageReference (include': string, version: string) =
        XElement.ofStringName("PackageReference",
            XAttribute.ofStringName("Include", include'),
            XAttribute.ofStringName("Version", version))

    static member ProjectReference (include': string) =
        XElement.ofStringName("ProjectReference",
            XAttribute.ofStringName("Include", include'))

