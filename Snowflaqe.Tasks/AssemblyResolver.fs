namespace Snowflaqe.Tasks

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices

type AssemblyResolver() =

    /// <summary>
    /// Considers a path to load for satisfying an assembly ref and loads it
    /// if the file exists and version is sufficient.
    /// </summary>
    /// <param name="filePath">Path to consider for load</param>
    /// <param name="minimumVersion">Minimum version to consider</param>
    /// <param name="assembly">loaded assembly</param>
    /// <returns>true if assembly was loaded</returns>
    static member private Probe(filePath: string, minimumVersion: Version, [<Out>] assembly: Assembly byref) : bool =
        assembly <- null
        if File.Exists(filePath) then
            let name = AssemblyName.GetAssemblyName(filePath)
            if name.Version >= minimumVersion then
                assembly <- Assembly.Load(name)
                true
            else false
        else false

    static member private CurrentDomain_AssemblyResolve(sender: obj, args: ResolveEventArgs) : Assembly =
        // apply any existing policy
        let referenceName = new AssemblyName(AppDomain.CurrentDomain.ApplyPolicy(args.Name))

        let fileName = referenceName.Name + ".dll"
        let mutable assemblyPath =
            match args.RequestingAssembly with
            | null -> None
            | requestingAssembly -> Some requestingAssembly.Location
        let mutable probingPath = null
        let mutable assm: Assembly = null

        // look next to requesting assembly
        if assemblyPath.IsSome then
            probingPath <- Path.Combine(Path.GetDirectoryName(assemblyPath.Value), fileName)
            Debug.WriteLine($"Considering {probingPath} based on RequestingAssembly")
            match AssemblyResolver.Probe(probingPath, referenceName.Version) with
            | true, loadedAssm -> assm <- loadedAssm
            | false, _ ->  ()

        // look in AppDomain base directory
        probingPath <- Path.Combine(AppDomain.CurrentDomain.BaseDirectory, fileName)
        Debug.WriteLine($"Considering {probingPath} based on BaseDirectory")
        match AssemblyResolver.Probe(probingPath, referenceName.Version) with
        | true, loadedAssm -> assm <- loadedAssm
        | false, _ ->  ()

        // look next to the executing assembly
        match Assembly.GetExecutingAssembly().Location with
        | null -> ()
        | assemblyPath ->
            probingPath <- Path.Combine(Path.GetDirectoryName(assemblyPath), fileName)
            Debug.WriteLine($"Considering {probingPath} based on ExecutingAssembly")
            match AssemblyResolver.Probe(probingPath, referenceName.Version) with
            | true, loadedAssm -> assm <- loadedAssm
            | false, _ ->  ()

        // look in current directory
        Debug.WriteLine($"Considering {fileName}")
        match AssemblyResolver.Probe(fileName, referenceName.Version) with
        | true, loadedAssm -> assm <- loadedAssm
        | false, _ ->  ()

        assm

    static member public Enable() =
        //System.Diagnostics.Debugger.Launch() |> ignore
        AppDomain.CurrentDomain.add_AssemblyResolve(fun sender args -> AssemblyResolver.CurrentDomain_AssemblyResolve(sender, args))
