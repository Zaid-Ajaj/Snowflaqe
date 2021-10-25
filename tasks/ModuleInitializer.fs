#if !NETCOREAPP2_0
namespace Snowflaqe.Tasks

type ModuleInitializer () =
    [<CompiledName("Initialize")>]
    static member public initialize() = AssemblyResolver.enable()
#endif
