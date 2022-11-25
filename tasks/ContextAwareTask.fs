namespace Snowflaqe.Tasks

open System;
open System.IO;
open System.Linq;
open System.Reflection;
#if (NET || NETCOREAPP)
open System.Runtime.Loader;
#endif
open Microsoft.Build.Framework;
open Microsoft.Build.Utilities;

[<AbstractClass>]
type public ContextAwareTask () as cat =
    inherit Task()

    let ``type`` = cat.GetType()
    let typeInfo = ``type``.GetTypeInfo()

    abstract ManagedDllDirectory : string with get
    default _.ManagedDllDirectory
        with get() =
            let codeBase = typeInfo.Assembly.Location
            let uri = new Uri(codeBase)
            Path.GetDirectoryName(uri.LocalPath)

    abstract UnmanagedDllDirectory : string with get
    default _.UnmanagedDllDirectory with get () = null

    abstract member ExecuteInner: unit -> bool

    override this.Execute() =
#if (NET || NETCOREAPP)
        let taskAssemblyPath = Uri(typeInfo.Assembly.Location).LocalPath
        let ctxt = CustomAssemblyLoader(this)
        let inContextAssembly = ctxt.LoadFromAssemblyPath(taskAssemblyPath)
        let innerTaskType = inContextAssembly.GetType(``type``.FullName)
        let innerTask = Activator.CreateInstance(innerTaskType)

        let outerProperties = ``type``.GetRuntimeProperties().ToDictionary(fun i -> i.Name);
        let innerProperties = innerTaskType.GetRuntimeProperties().ToDictionary(fun i -> i.Name);
        let propertiesDiscovery =
            outerProperties.Values
            |> Seq.filter (fun (outerProperty: PropertyInfo) -> not (isNull outerProperty.SetMethod || isNull outerProperty.GetMethod))
            |> Seq.map
                (fun outerProperty ->
                    let innerProperty = innerProperties.[outerProperty.Name]
                    (outerProperty, innerProperty))
        let propertiesMap = propertiesDiscovery |> Seq.toArray
        let outputPropertiesMap =
            propertiesDiscovery
            |> Seq.filter (fun (outerProperty, _) -> not (isNull (outerProperty.GetCustomAttribute<OutputAttribute>())))

        let propertiesMap =
            propertiesMap
            |> Seq.map
                (fun pair ->
                    let (outerProperty, innerProperty) = pair
                    innerProperty.SetValue(innerTask, outerProperty.GetValue(this))
                    pair)

        let executeInnerMethod =
            innerTaskType.GetMethod(nameof(this.ExecuteInner), (BindingFlags.Instance ||| BindingFlags.NonPublic))
        let result = executeInnerMethod.Invoke(innerTask, Array.empty) :?> bool

        let outputPropertiesMap =
            outputPropertiesMap
            |> Seq.map
                (fun (outerProperty, innerProperty) ->
                    outerProperty.SetValue(this, innerProperty.GetValue(innerTask)))

        result
#else
        // On .NET Framework (on Windows), we find native binaries by adding them to our PATH.
        if not (this.UnmanagedDllDirectory = null) then
            let pathEnvVar = Environment.GetEnvironmentVariable("PATH")
            let searchPaths = pathEnvVar.Split(Path.PathSeparator)
            if not (searchPaths.Contains(this.UnmanagedDllDirectory, StringComparer.OrdinalIgnoreCase)) then
                let pathEnvVar = $"{pathEnvVar}{Path.PathSeparator}{this.UnmanagedDllDirectory}"
                Environment.SetEnvironmentVariable("PATH", pathEnvVar)

        this.ExecuteInner()
#endif


#if (NET || NETCOREAPP)

and private CustomAssemblyLoader(loaderTask: ContextAwareTask) =
    inherit AssemblyLoadContext()

    member this.loaderTask = loaderTask

    override this.Load(assemblyName: AssemblyName) : Assembly =
        let assemblyPath = Path.Combine(this.loaderTask.ManagedDllDirectory, assemblyName.Name) + ".dll"
        if File.Exists(assemblyPath) then
            this.LoadFromAssemblyPath(assemblyPath) |> ignore
        AssemblyLoadContext.Default.LoadFromAssemblyName(assemblyName)

    override this.LoadUnmanagedDll(unmanagedDllName: string) : IntPtr =
        let unmanagedDllPath =
             Directory.EnumerateFiles(
                this.loaderTask.UnmanagedDllDirectory,
                $"{unmanagedDllName}.*").Concat(
                    Directory.EnumerateFiles(
                        this.loaderTask.UnmanagedDllDirectory,
                        $"lib{unmanagedDllName}.*"))
                .FirstOrDefault()

        if not (isNull unmanagedDllPath) then
            this.LoadUnmanagedDllFromPath(unmanagedDllPath) |> ignore

        base.LoadUnmanagedDll(unmanagedDllName)
#endif
