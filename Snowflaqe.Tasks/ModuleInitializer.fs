namespace Snowflaqe.Tasks

type ModuleInitializer() =
    static member public Initialize() =
        AssemblyResolver.Enable()

