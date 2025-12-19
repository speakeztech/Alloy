#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Primitive operations and bindings that must be defined before all other Alloy modules.
/// Alex intercepts calls to Primitives.Bindings and provides platform-specific implementations.
[<AutoOpen>]
module Primitives =

    /// Standard file descriptors
    [<Literal>]
    let STDIN = 0
    [<Literal>]
    let STDOUT = 1
    [<Literal>]
    let STDERR = 2

    /// Re-export ofBytes from NativeTypes for use with panicwith
    let inline ofBytes (bytes: byte[]) : NativeStr = NativeString.ofBytes bytes

    /// Fundamental platform bindings. Alex intercepts all calls to this module.
    /// These are the lowest-level I/O and process control primitives.
    module Bindings =
        /// Write bytes to a file descriptor. Alex provides platform implementation.
        let writeBytes (_fd: int) (_buffer: nativeint) (_count: int) : int = 0

        /// Read bytes from a file descriptor. Alex provides platform implementation.
        let readBytes (_fd: int) (_buffer: nativeint) (_maxCount: int) : int = 0

        /// Abort the process immediately. Alex provides platform implementation.
        let abort (_exitCode: int) : unit = ()

    /// Write a NativeStr to a file descriptor.
    let inline writeStr (fd: int) (s: NativeStr) : int =
        Bindings.writeBytes fd (NativePtr.toNativeInt s.Pointer) s.Length

    /// Write a NativeStr to stderr.
    let inline writeErr (s: NativeStr) : unit =
        writeStr STDERR s |> ignore

    /// Write a newline to a file descriptor.
    let inline writeNewline (fd: int) : unit =
        let mutable nl = 10uy
        Bindings.writeBytes fd (NativePtr.toNativeInt &&nl) 1 |> ignore

    /// Native panic that aborts the program with exit code 1.
    /// Writes the error message to stderr before aborting.
    /// Use instead of failwith to avoid BCL string dependency.
    /// Usage: panicwith (ofBytes "message"B)
    let inline panicwith (message: NativeStr) : 'T =
        // Write "panic: " prefix
        writeErr (ofBytes "panic: "B)
        // Write the error message
        writeErr message
        // Write newline
        writeNewline STDERR
        // Abort with exit code 1
        Bindings.abort 1
        Unchecked.defaultof<'T>
