#if !NETCOREAPP2_0
module Snowflaqe.Tasks.AssemblyResolver

open System
open System.Diagnostics
open System.IO
open System.Reflection

module private ValueOption =
    let mapNone initializer =
        function
        | ValueSome value -> ValueSome value
        | ValueNone -> initializer ()

/// <summary>
/// Considers a path to load for satisfying an assembly ref and loads it
/// if the file exists and version is sufficient.
/// </summary>
/// <param name="filePath">Path to consider for load</param>
/// <param name="minimumVersion">Minimum version to consider</param>
/// <returns>loaded assembly voption</returns>
let private probe(filePath: string, minimumVersion: Version) : Assembly voption =
    if File.Exists(filePath) then
        let name = AssemblyName.GetAssemblyName(filePath)
        if name.Version >= minimumVersion
        then ValueSome (Assembly.Load(name))
        else ValueNone
    else ValueNone

let private loadFromCurrentDirectory fileName version =
    Debug.WriteLine($"Considering {fileName}")
    probe(fileName, version)

let private loadFromExecutingAssembly fileName version =
    match Assembly.GetExecutingAssembly().Location with
    | null -> ValueNone
    | assemblyPath ->
        let probingPath = Path.Combine(Path.GetDirectoryName(assemblyPath), fileName)
        Debug.WriteLine($"Considering {probingPath} based on ExecutingAssembly")
        probe(probingPath, version)

let private loadFromAppDomain fileName version =
    let probingPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, fileName)
    Debug.WriteLine($"Considering {probingPath} based on BaseDirectory")
    probe(probingPath, version)

let private loadFromRequestingAssemblyPath version fileName assemblyPath =
     let probingPath = Path.Combine(Path.GetDirectoryName(assemblyPath), fileName)
     Debug.WriteLine($"Considering {probingPath} based on RequestingAssembly")
     probe(probingPath, version)

let private onAssemblyResolve(args: ResolveEventArgs) : Assembly =
    // apply any existing policy
    let referenceName = new AssemblyName(AppDomain.CurrentDomain.ApplyPolicy(args.Name))
    let version = referenceName.Version
    let fileName = referenceName.Name + ".dll"

    args.RequestingAssembly
    |> ValueOption.ofObj
    |> ValueOption.map (fun assemblyPath -> assemblyPath.Location)
    |> ValueOption.bind (loadFromRequestingAssemblyPath version fileName)
    |> ValueOption.mapNone (fun () -> loadFromAppDomain fileName version)
    |> ValueOption.mapNone (fun () -> loadFromExecutingAssembly fileName version)
    |> ValueOption.mapNone (fun () -> loadFromCurrentDirectory fileName version)
    |> ValueOption.defaultValue null

[<CompiledName("Enable")>]
let public enable() = AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args -> onAssemblyResolve(args))
#endif
