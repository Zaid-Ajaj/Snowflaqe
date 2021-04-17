[<AutoOpen>]
module Snowflaqe.LinqToXmlExtensions

open System
open System.Xml
open System.Xml.Linq

type XAttribute with

    static member ofStringName (name: string, value: obj) =
        XAttribute(XName.Get(name), value)

type XElement with

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

type XDocument with

    member this.WriteTo (outputFileName: string) =
        use writer = XmlWriter.Create(outputFileName)
        this.WriteTo(writer)

