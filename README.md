# Alloy - Historical Archive

> **Absorbed into FNCS (January 2026)** — This repository is preserved as a historical artifact.
> See: [Absorbing Alloy](https://speakez.tech/blog/absorbing-alloy/)

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![License: Commercial](https://img.shields.io/badge/License-Commercial-orange.svg)](Commercial.md)

## What Alloy Was

Alloy was a BCL-free F# standard library for native compilation, providing:

- Native type implementations (NativeStr, NativeBuffer, voption)
- Platform-agnostic APIs (Console, Memory, Time, String)
- Platform bindings for syscalls

It proved F# could operate without the .NET Base Class Library—a stepping stone to the current architecture where types and operations are compiler intrinsics in FNCS.

## Key Lessons

- Fat pointer types (ptr, length structs) work for strings and arrays
- SRTP enables zero-cost abstractions without boxing
- Platform bindings need rich metadata via quotation carriers

## License

Dual-licensed: [Apache 2.0](LICENSE) for open source/academic use, [Commercial](Commercial.md) for commercial products.

Patent Notice: Part of technology covered by U.S. Patent Application No. 63/786,247. See [PATENTS.md](PATENTS.md).

## Acknowledgments

- **[fsil](https://github.com/ieviev/fsil)**: Zero-cost F# abstraction patterns
- **Don Syme and F# Contributors**
- **The Fidelity Team**
