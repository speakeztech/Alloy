# Alloy - Historical Archive

> **Status: Absorbed into FNCS (January 2026)**
>
> This repository is preserved as a historical artifact. Its functionality has been
> fully absorbed into FNCS (F# Native Compiler Services). The repository will be
> made read-only and eventually delisted from active development.
>
> See blog entry: [Absorbing Alloy](https://speakez.tech/blog/absorbing-alloy/)

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![License: Commercial](https://img.shields.io/badge/License-Commercial-orange.svg)](Commercial.md)

## What Alloy Was

Alloy was a BCL-free F# standard library designed for native compilation. It proved
that F# could operate without the .NET Base Class Library, providing:

- **Native type implementations**: NativeStr, NativeBuffer, voption, etc.
- **Platform-agnostic APIs**: Console, Memory, Time, String operations
- **BCL-sympathetic naming**: Console.WriteLine, DateTime.Now, etc.
- **Platform bindings**: Declarative surface for syscalls and platform operations

## Why It Was Absorbed

Following patterns from ML, Rust, and Triton-CPU, we realized that:

1. **Types ARE the language** - NTUKind in FNCS defines the native type universe
2. **Operations ARE intrinsics** - Sys.*, NativePtr.*, Console.*, etc. in FNCS
3. **No library needed** - The compiler provides all native semantics

Alloy served its purpose: **proving BCL-free F# is possible**. But it was a stepping
stone, not the destination.

## The New Architecture

Application code now uses FNCS intrinsics directly:

```fsharp
// Application code - uses FNCS intrinsics, no library needed
let main() =
    Console.writeln "Hello, World!"  // FNCS intrinsic
    let id = Uuid.newV4()            // FNCS intrinsic, NTU type
    let now = DateTime.now()         // FNCS intrinsic, NTU type
    0
```

FNCS provides:
- **NTUKind types**: int, uint, string, Uuid, DateTime, TimeSpan, etc.
- **Intrinsic modules**: Sys.*, NativePtr.*, Console.*, String.*, Math.*, Uuid.*, DateTime.*
- **Platform resolution via quotations**: Platform-specific details resolved at code generation

## What Alloy Taught Us

Alloy demonstrated:
- **BCL-free F# is possible** - No System.* dependencies needed
- **Fat pointer types work** - (ptr, length) structs for strings and arrays
- **SRTP enables zero-cost abstractions** - Compile-time polymorphism without boxing
- **Platform bindings need rich metadata** - Quotation carriers, not stub declarations

These lessons are now embodied in FNCS as NTUKind types and intrinsic modules.

## Repository Lifecycle

1. **Now**: Preserved with this README explaining absorbed status
2. **Soon**: Pushed to FidelityFramework organization on GitHub
3. **Later**: Made read-only
4. **Eventually**: Delisted from active discovery (kept as historical reference)

## The Fidelity Framework

Alloy was part of the **Fidelity** native F# compilation ecosystem:

| Project | Role | Status |
|---------|------|--------|
| **[Firefly](https://github.com/FidelityFramework/Firefly)** | AOT compiler: F# → PSG → MLIR → Native binary | **Active** |
| **FNCS (fsnative)** | Native type universe and intrinsic modules | **Active** - types/ops are compiler intrinsics |
| **Alloy** | Native standard library | **Absorbed** into FNCS |
| **[BAREWire](https://github.com/FidelityFramework/BAREWire)** | Binary encoding, memory mapping, zero-copy IPC | Active |
| **[Farscape](https://github.com/FidelityFramework/Farscape)** | C/C++ header parsing for native library bindings | Active |
| **[XParsec](https://github.com/FidelityFramework/XParsec)** | Parser combinators powering PSG traversal | Active |

The name "Fidelity" reflects the framework's core mission: **preserving type and memory safety** from source code through compilation to native execution.

## See Also

- [Absorbing Alloy](/blog/absorbing-alloy/) - Blog entry explaining the evolution
- [Building Firefly With Alloy](/blog/building-firefly-with-alloy/) - The previous understanding
- [FNCS Architecture](/docs/FNCS_Architecture.md) - Current architecture
- [FidelityFramework](https://github.com/FidelityFramework) - Active development

## License

Alloy remains dual-licensed under both the Apache License 2.0 and a Commercial License.

### Open Source License

For open source projects, academic use, non-commercial applications, and internal tools, use Alloy under the **Apache License 2.0**.

### Commercial License

A Commercial License is required for incorporating Alloy into commercial products or services. See [Commercial.md](Commercial.md) for details.

### Patent Notice

Alloy is part of the Fidelity Framework, which includes technology covered by U.S. Patent Application No. 63/786,247 "System and Method for Zero-Copy Inter-Process Communication Using BARE Protocol". See [PATENTS.md](PATENTS.md) for licensing details.

## Acknowledgments

- **[fsil](https://github.com/ieviev/fsil)**: Foundational patterns for zero-cost F# abstractions
- **Don Syme and F# Contributors**: For the elegant language that makes this possible
- **The Fidelity Team**: For the native compilation infrastructure

---

*This repository stands as a testament to our commitment to forging our own path in the Fidelity framework. The lesson: Don't parallel the CLR. Build native from the compiler up.*
