namespace LinqToXmlExtensions

open System
open System.Xml.Linq

[<AbstractClass; Sealed>]
type XElement =

    static member ofStringName (name: string, content: obj) =
        XElement(XName.Get(name), content)

    static member ofStringName (name: string, [<ParamArray>] content) =
        XElement(XName.Get(name), content)

[<AbstractClass; Sealed>]
type XAttribute =

    static member ofStringName (name: string, value: obj) =
        XAttribute(XName.Get(name), value)
