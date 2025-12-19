# Alloy

The native F# standard library for the Fidelity Framework.

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![License: Commercial](https://img.shields.io/badge/License-Commercial-orange.svg)](Commercial.md)

<p align="center">
🚧 <strong>Under Active Development</strong> 🚧<br>
<em>This project is in early development and not intended for production use.</em>
</p>

## Overview

Alloy is the standard library for F# programs targeting native compilation through Firefly. Unlike traditional .NET libraries that depend on the Base Class Library (BCL) and runtime, Alloy provides **source-level implementations** that compile directly to native code without runtime dependencies.

### Key Characteristics

- **BCL-Free**: No System.* dependencies - compiles to standalone native binaries
- **Source Compilation**: Consumed as F# source files, not as a compiled DLL
- **BCL-Sympathetic API**: Familiar naming conventions for .NET developers
- **Native Types**: UTF-8 strings, value options, fixed-width integers
- **Platform Bindings**: Declarative surface for platform-specific operations

## The Fidelity Framework

Alloy is part of the **Fidelity** native F# compilation ecosystem:

| Project | Role |
|---------|------|
| **[Firefly](https://github.com/speakez-llc/firefly)** | AOT compiler: F# → PSG → MLIR → Native binary |
| **Alloy** | Native standard library with platform bindings |
| **[BAREWire](https://github.com/speakez-llc/barewire)** | Binary encoding, memory mapping, zero-copy IPC |
| **[Farscape](https://github.com/speakez-llc/farscape)** | C/C++ header parsing for native library bindings |
| **[XParsec](https://github.com/speakez-llc/xparsec)** | Parser combinators powering PSG traversal and header parsing |

The name "Fidelity" reflects the framework's core mission: **preserving type and memory safety** from source code through compilation to native execution.

## How Alloy Works

When you compile an F# program with Firefly:

```fsharp
open Alloy

let main () =
    Console.WriteLine "Hello, World!"
```

Firefly compiles your code **together with Alloy's source files**. The result is a standalone native binary with no .NET runtime dependency.

### BCL Sympathy, Not BCL Compatibility

Alloy provides familiar APIs but with native semantics:

| BCL Pattern | Alloy Equivalent | Difference |
|-------------|------------------|------------|
| `System.String` (UTF-16) | `NativeStr` (UTF-8) | Memory-efficient, C-compatible |
| `option<'T>` (nullable) | `voption<'T>` (struct) | Stack-allocated, null-free |
| `System.DateTime` | Native ticks | No timezone/calendar complexity |
| Exceptions | `Result<'T, 'E>` | Explicit error handling |
| `null` | Not permitted | Compiler-enforced null safety |

## Core Modules

### Native Types

```fsharp
open Alloy

// UTF-8 strings - memory efficient, C-compatible
let greeting: NativeStr = "Hello, World!"

// Value options - stack allocated, no null
let maybeValue: voption<int> = ValueSome 42

// Fixed-width integers with explicit sizes
let byte: u8 = 255uy
let word: i32 = 42
let quad: i64 = 1234567890L
```

### Console I/O

```fsharp
open Alloy

// Output
Console.Write "Enter name: "
Console.WriteLine "Hello!"

// Input with stack buffer
let name = Console.ReadLine ()
Console.WriteLine $"Hello, {name}!"
```

### Memory Operations

```fsharp
open Alloy

// Stack-allocated buffer
use buffer = Memory.stackAlloc<byte> 256

// Memory operations
Memory.copy src dst length
Memory.zero buffer
```

### Result-Based Error Handling

```fsharp
open Alloy

let divide x y =
    if y = 0 then Error "Division by zero"
    else Ok (x / y)

let result =
    divide 10 2
    |> Result.map (fun x -> x * 2)
    |> Result.defaultValue 0
```

## Platform Bindings

Alloy declares platform operations that Firefly's Alex component implements for each target:

```fsharp
// In Alloy - declaration only
module Platform.Bindings =
    let writeBytes fd buffer count : int = Unchecked.defaultof<int>
    let readBytes fd buffer count : int = Unchecked.defaultof<int>
    let getCurrentTicks () : int64 = Unchecked.defaultof<int64>
```

Alex recognizes calls to `Platform.Bindings.*` and generates platform-appropriate code:
- **Linux x86_64**: `syscall` instructions
- **Windows x86_64**: Win32 API calls
- **ARM Cortex-M**: Memory-mapped peripheral access
- **WebAssembly**: WASI imports

**This separation keeps Alloy platform-agnostic** while enabling efficient native code generation.

## Project Configuration

Firefly projects reference Alloy via `.fidproj` files:

```toml
[package]
name = "my_app"

[dependencies]
alloy = { path = "/path/to/Alloy/src" }

[build]
sources = ["Main.fs"]
output = "my_app"
output_kind = "console"  # or "freestanding"
```

## Design Principles

### 1. Source-Level Composition

Alloy is not a compiled DLL. Your code and Alloy's code are compiled together as a single unit, enabling:
- Whole-program optimization
- Dead code elimination
- Cross-module inlining

### 2. No Hidden Runtime

Every operation compiles to explicit machine code. There is no:
- Garbage collector
- JIT compiler
- Runtime type system
- Hidden allocations

### 3. Explicit Resource Management

Resources are managed through RAII patterns:

```fsharp
// File handle cleaned up at scope exit
use file = File.open "data.txt"
let content = File.readAll file
// file automatically closed here
```

### 4. Zero-Cost Abstractions

Alloy leverages F#'s statically resolved type parameters (SRTPs) for abstractions that compile away:

```fsharp
// Generic numeric operations - no boxing, no virtual dispatch
let inline square x = x * x

let intResult = square 5      // Compiles to: imul
let floatResult = square 5.0  // Compiles to: mulsd
```

## Relationship to fsil

Alloy builds on patterns established by [fsil](https://github.com/ieviev/fsil), particularly:
- Inline-by-default iteration
- SRTP-based polymorphism
- Collection protocols without interfaces

As the Fidelity ecosystem matures, these patterns are being absorbed into FNCS (F# Native Compiler Services), making them intrinsic to the language for native targets.

## Development Status

Alloy is under active development as part of the Fidelity Framework. Current focus areas:

- Core type implementations (NativeStr, voption, Result)
- Console I/O for validation samples
- Memory primitives for stack/arena allocation
- Platform binding surface for Alex integration

## Building and Testing

Alloy is tested through Firefly's sample programs:

```bash
# From Firefly repository
cd samples/console/FidelityHelloWorld/01_HelloWorldDirect

# Compile (includes Alloy sources)
firefly compile HelloWorld.fidproj

# Run native binary
./HelloWorld
```

## License

Alloy is dual-licensed under both the Apache License 2.0 and a Commercial License.

### Open Source License

For open source projects, academic use, non-commercial applications, and internal tools, use Alloy under the **Apache License 2.0**.

### Commercial License

A Commercial License is required for incorporating Alloy into commercial products or services. See [Commercial.md](Commercial.md) for details.

### Patent Notice

Alloy is part of the Fidelity Framework, which includes technology covered by U.S. Patent Application No. 63/786,247 "System and Method for Zero-Copy Inter-Process Communication Using BARE Protocol". See [PATENTS.md](PATENTS.md) for licensing details.

## Acknowledgments

- **[fsil](https://github.com/ieviev/fsil)**: Foundational patterns for zero-cost F# abstractions
- **Don Syme and F# Contributors**: For the elegant language that makes this possible
- **Firefly Team**: For the native compilation infrastructure
