#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Primitive operations and bindings that must be defined before all other Alloy modules.
/// Alex intercepts calls to Primitives.Bindings and provides platform-specific implementations.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, 'string' has members: Pointer (nativeptr<byte>), Length (int)
[<AutoOpen>]
module Primitives =

    /// Standard file descriptors
    [<Literal>]
    let STDIN = 0
    [<Literal>]
    let STDOUT = 1
    [<Literal>]
    let STDERR = 2

    /// Creates a string from a byte literal.
    /// In native compilation, FNCS handles byte literal → string conversion.
    /// The -1 accounts for F#'s null terminator in byte literals.
    let inline ofBytes (bytes: byte[]) : string =
        // FNCS provides native string construction from byte literals
        // For .NET compat, this converts byte[] to string
        System.Text.Encoding.UTF8.GetString(bytes, 0, bytes.Length - 1)

    /// Fundamental platform bindings. Alex intercepts all calls to this module.
    /// These are the lowest-level I/O and process control primitives.
    module Bindings =
        /// Write bytes to a file descriptor. Alex provides platform implementation.
        let writeBytes (_fd: int) (_buffer: nativeint) (_count: int) : int = 0

        /// Read bytes from a file descriptor. Alex provides platform implementation.
        let readBytes (_fd: int) (_buffer: nativeint) (_maxCount: int) : int = 0

        /// Abort the process immediately. Alex provides platform implementation.
        let abort (_exitCode: int) : unit = ()

    /// Write a string to a file descriptor.
    /// In native compilation, string has .Pointer and .Length members via FNCS.
    let inline writeStr (fd: int) (s: string) : int =
        // FNCS provides: s.Pointer (nativeptr<byte>), s.Length (int)
        // For .NET compat, we encode to bytes
        let bytes = System.Text.Encoding.UTF8.GetBytes(s)
        use ptr = fixed bytes
        Bindings.writeBytes fd (NativePtr.toNativeInt ptr) bytes.Length

    /// Write a string to stderr.
    let inline writeErr (s: string) : unit =
        writeStr STDERR s |> ignore

    /// Write a newline to a file descriptor.
    let inline writeNewline (fd: int) : unit =
        let mutable nl = 10uy
        Bindings.writeBytes fd (NativePtr.toNativeInt &&nl) 1 |> ignore

    /// Native panic that aborts the program with exit code 1.
    /// Writes the error message to stderr before aborting.
    /// Usage: panicwith "message" or panicwith (ofBytes "message"B)
    let inline panicwith (message: string) : 'T =
        // Write "panic: " prefix
        writeErr "panic: "
        // Write the error message
        writeErr message
        // Write newline
        writeNewline STDERR
        // Abort with exit code 1
        Bindings.abort 1
        Unchecked.defaultof<'T>
