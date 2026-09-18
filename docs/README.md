# Documentation

For running Haxe programs on ash:

| | |
|---|---|
| [cli.md](cli.md) | command-line reference: modes, thresholds, build flags |
| [aot.md](aot.md) | native executables and wasm modules: build cost, dials, HDLL programs, troubleshooting |
| [debugging.md](debugging.md) | pinning a tier, environment variables, the profiler, bisecting a wrong answer |
| [simd.md](simd.md) | the ash-simd library: value types, slot primitives, lane semantics |
| [hdll.md](hdll.md) | writing a native library |
| [wasm/README.md](wasm/README.md) | the wasm target: hosts, threads, what is in scope |
| [wasm/host-abi.md](wasm/host-abi.md) | what a host implements: the fiber import, sockets |
| [wasm/hdlls.md](wasm/hdlls.md) | native libraries as wasm side modules |
| [mbhaxe.md](mbhaxe.md) | the MarbleGame fixture |

For changing ash: [CONTRIBUTING.md](../CONTRIBUTING.md), and the design
notes under [internals/](internals/).
