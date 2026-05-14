# DOTNET Dev

## Profiling

```bash
    dotnet tool install --global dotnet-trace

    dotnet-trace ps # find process to profile

    dotnet-trace collect --process-id <PID> # then profile
```

Convert to speedscope

```bash
    dotnet-trace convert
```

open in (speedscope)[https://www.speedscope.app/]

### Pick the right profile

The whole reason dotnet-trace feels confusing the first time is that “what events should I capture?” has many right answers.
The tool ships with named profiles so you do not have to memorize keyword bitmasks. As of dotnet-trace 9.0.661903, the collect verb supports:

- `dotnet-common`: lightweight runtime diagnostics. GC, AssemblyLoader, Loader, JIT, Exceptions, Threading, JittedMethodILToNativeMap, and Compilation events at the Informational level.
    Equivalent to Microsoft-Windows-DotNETRuntime:0x100003801D:4.
- `dotnet-sampled-thread-time`: samples managed thread stacks at roughly 100 Hz to identify hotspots over time. Uses the runtime’s sample profiler with managed stacks.
- `gc-verbose`: GC collections plus sampled object allocations. Heavier than dotnet-common but the only way to find allocation hotspots without a memory profiler.
- `gc-collect`: GC collections only, very low overhead. Good for “is the GC pausing me?” without affecting steady-state throughput.
- `database`: ADO.NET and Entity Framework command events. Useful for catching N+1 queries.

When you run dotnet-trace collect with no flags, the tool now picks dotnet-common plus dotnet-sampled-thread-time by default. This combo replaces the old cpu-sampling profile, which sampled all threads regardless of CPU usage and led people to misread idle threads as hot. If you need the exact old behavior for back-compat with older traces, use --profile dotnet-sampled-thread-time --providers "Microsoft-Windows-DotNETRuntime:0x14C14FCCBD:4".

You can stack profiles with commas:

`dotnet-trace collect -p 21932 --profile dotnet-common,gc-verbose,database --duration 00:00:01:00`

For anything more bespoke, use `--providers`. The format is Provider[,Provider] where each provider is Name[:Flags[:Level[:KeyValueArgs]]].
For example, to capture only contention events at verbose level:

`dotnet-trace collect -p 21932 --providers "Microsoft-Windows-DotNETRuntime:0x4000:5"`

If you want a friendlier syntax for runtime keywords, --clrevents gc+contention --clreventlevel informational is equivalent to --providers Microsoft-Windows-DotNETRuntime:0x4001:4 and is much easier to read in scripts.

### Stop on a specific event

Long traces are noisy. If you only care about the slice between “JIT started compiling X” and “request finished”, dotnet-trace can stop the moment a specific event fires:

```bash
dotnet-trace collect -p 21932 \
  --stopping-event-provider-name Microsoft-Windows-DotNETRuntime \
  --stopping-event-event-name Method/JittingStarted \
  --stopping-event-payload-filter MethodNamespace:MyApp.HotPath,MethodName:Render
```

The event stream is parsed asynchronously, so a few extra events leak through after the match before the session actually closes. That is normally not a problem when you are looking at hotspots.

## Counters

```bash
    dotnet tool install --global dotnet-counters
```
