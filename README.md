# Alloy

A zero-cost abstractions library for F# that is meant to provide native F# replacements for the BCL components.

## Overview

Alloy builds upon the foundation laid by [fsil](https://github.com/ieviev/fsil), providing additional zero-cost abstractions for numeric operations, collections, memory management, time operations, and string manipulation. Like fsil, Alloy leverages F#'s statically resolved type parameters (SRTPs) to ensure all abstractions are resolved at compile time with no runtime overhead.

## Core Features

- **Zero-cost abstractions**: All operations are inlined and compile to the same code as direct function calls
- **No runtime overhead**: Compile-time resolution means you get high-level abstractions with low-level performance
- **Type-safe operations**: Fully leverages F#'s type system for safety and correctness
- **Platform-native implementations**: Direct OS API calls for time and memory operations
- **Struct-based types**: ValueOption and other types avoid heap allocations

## From fsil to Alloy

Alloy extends fsil's core functionality with additional operations:

| fsil provides | Alloy adds |
|---------------|------------|
| `map`, `mapi` | `add`, `subtract`, `multiply`, `divide` |
| `iter`, `iteri` | `min`, `max`, `sum`, `average`, `product` |
| `fold` | `filter`, `choose`, `find`, `tryFind` |
| `zero`, `one` | `power`, `abs`, `sign`, `ceiling`, `floor`, `round` |
| Basic printing | String manipulation functions |
| | Memory primitives with region safety |
| | Time operations without System.DateTime |
| | Binary conversions for serialization |
| | UTF-8 encoding/decoding |
| | UUID v4/v5 generation |
| | Result type for error handling |
| | Span operations |

## Installation

```bash
dotnet add package Alloy
```

## Basic Usage

### Numeric Operations

```fsharp
open Alloy

// Basic arithmetic - works with any numeric type
let result = add 10 5           // 15
let product = multiply 4 7      // 28
let powered = power 2 8         // 256

// Collection operations
let numbers = [|1; 2; 3; 4; 5|]
let total = sum numbers         // 15
let avg = average numbers       // 3
let prod = product numbers      // 120

// Works with units of measure
[<Measure>] type meter
let distance = 100<meter>
let doubled = multiply distance 2  // 200<meter>
```

### ValueOption - Stack-Allocated Options

```fsharp
// Zero heap allocations - lives on the stack
let maybeValue = ValueOption.Some 42
let empty = ValueOption<string>.None

// Pattern matching
match maybeValue with
| Some v -> printfn "Value: %d" v
| None -> printfn "No value"

// Functional operations
let result = 
    maybeValue
    |> ValueOption.map (fun x -> x * 2)
    |> ValueOption.defaultValue 0
```

### Memory Operations

Type-safe memory operations with region tracking:

```fsharp
// Create memory regions
let data = Array.zeroCreate<byte> 1024
let memory = Memory.fromArray<int, region> data

// Safe memory operations
Memory.writeByte memory 0<offset> 42uy
let value = Memory.readByte memory 0<offset>

// Memory slicing
let slice = Memory.slice memory 10<offset> 100<bytes>
```

### Time Operations

Platform-native time without System.DateTime:

```fsharp
open Alloy.Time

// High-resolution timing
let start = Time.highResolutionTicks()
// ... perform work ...
let elapsed = Time.elapsedTime start (Time.highResolutionTicks())

// Basic time operations
let now = Time.now()
let unix = Time.currentUnixTimestamp()

// Create specific dates
let date = Time.createDateTime 2025 5 24 14 30 0 0
```

### String Operations

Essential string manipulation without System.String methods:

```fsharp
// Core string operations
let text = "  Hello, World!  "
let trimmed = String.trim text
let parts = String.split ',' "a,b,c"
let joined = String.join ", " [|"x"; "y"; "z"|]

// Efficient concatenation
let message = String.concat3 "Hello" " " "World"

// String inspection
let hasPrefix = String.startsWith "Hello" text
let contains = String.contains "World" text
```

### Binary Operations

Low-level binary conversions:

```fsharp
// Bit-pattern preserving conversions
let bits = Binary.singleToInt32Bits 3.14f
let restored = Binary.int32BitsToSingle bits

// Byte conversions
let bytes = Binary.getInt32Bytes 42
let value = Binary.toInt32 bytes 0
```

### UTF-8 Encoding

Pure F# UTF-8 implementation:

```fsharp
// Encode/decode without System.Text.Encoding
let utf8 = Utf8.getBytes "Hello, 世界!"
let text = Utf8.getString utf8
```

### UUID Generation

RFC 4122 compliant UUID implementation:

```fsharp
open Alloy.Uuid

// Generate UUIDs
let id = Uuid.newUuid()              // v4 random
let strId = Uuid.toString id         // "550e8400-e29b-41d4-a716-446655440000"
let parsed = Uuid.fromString strId
```

## Platform Support

Alloy provides platform-specific implementations for:
- **Windows**: kernel32.dll APIs
- **Linux**: libc APIs
- **macOS**: libSystem APIs
- **Portable**: Pure F# fallbacks

Platform selection is automatic and transparent.

## Design Principles

### Zero-Cost Abstractions

All Alloy operations compile to the same machine code as hand-written implementations:

```fsharp
// This Alloy code:
let result = add x y

// Compiles to the same assembly as:
let result = x + y
```

### Type Safety with Units of Measure

Alloy preserves F#'s units of measure throughout operations:

```fsharp
[<Measure>] type second
[<Measure>] type meter

let time = 10<second>
let distance = 50<meter>
let speed = divide distance time  // 5<meter/second>
```

### Memory Safety

Region-based types prevent memory errors at compile time:

```fsharp
[<Measure>] type heap
[<Measure>] type stack

// Types ensure you can't mix memory regions
let heapMem = Memory.fromArray<int, heap> heapData
let stackMem = Memory.fromArray<int, stack> stackData
// Memory.copy heapMem stackMem 10<bytes>  // Compile error!
```

## Performance

- **Inlined operations**: Everything is inlined by the F# compiler
- **Stack allocation**: ValueOption and similar types avoid heap allocation
- **Direct platform calls**: No abstraction layers for system operations
- **Cache-friendly**: Struct types improve memory locality

## Extending Alloy

Add Alloy operations to your own types:

```fsharp
type Vector2D = { X: float; Y: float }

// Implement required static members
type Vector2D with
    static member Add(a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
    static member Zero = { X = 0.0; Y = 0.0 }

// Now works with Alloy operations
let v1 = { X = 3.0; Y = 4.0 }
let v2 = { X = 1.0; Y = 2.0 }
let v3 = add v1 v2  // { X = 4.0; Y = 6.0 }
```

## API Reference TBD



- **Core**: Extended operations from fsil
- **Numerics**: Arithmetic and mathematical operations
- **String**: String manipulation functions
- **ValueOption**: Stack-allocated option type
- **Memory**: Type-safe memory operations
- **Time**: Platform-native time operations
- **Binary**: Binary conversion utilities
- **Utf8**: UTF-8 encoding/decoding
- **Uuid**: UUID generation and parsing
- **Result**: Functional error handling
- **Span**: Contiguous memory views
