[<AutoOpen>]
module Snowflaqe.LinqToXmlExtensions

open System
open System.Collections.Generic
open System.Xml
open System.Xml.Linq

type MSBuildTask =
    { Name : string
      FullName : string
      AssemblyFile : string
      Parameters : KeyValuePair<string, obj> seq }

type MSBuildTarget =
    { Name : string
      AfterTargets : string option
      BeforeTargets : string option
      Tasks : MSBuildTask seq }

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

    static member Task (task: MSBuildTask) =
        XElement.ofStringName(task.Name,
            task.Parameters |> Seq.map (fun p -> XAttribute.ofStringName(p.Key, p.Value)))

    static member Target (target: MSBuildTarget) =
        XElement.ofStringName("Target",
            seq {
                yield XAttribute.ofStringName("Name", target.Name) :> obj
                if target.AfterTargets.IsSome
                then yield XAttribute.ofStringName("AfterTargets", target.AfterTargets.Value) :> obj
                if target.BeforeTargets.IsSome
                then yield XAttribute.ofStringName("BeforeTargets", target.BeforeTargets.Value) :> obj
                yield! target.Tasks |> Seq.map (fun task -> MSBuildXElement.Task(task) :> obj)
            })

    static member UsingTask (taskName: string, assemblyFile: string) =
        XElement.ofStringName("UsingTask",
            XAttribute.ofStringName("TaskName", taskName),
            XAttribute.ofStringName("AssemblyFile", assemblyFile))

type XDocument with

    member this.WriteTo (outputFileName: string) =
        use writer = XmlWriter.Create(outputFileName)
        this.WriteTo(writer)

