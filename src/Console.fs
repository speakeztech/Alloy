#nowarn "9"
#nowarn "1182"
namespace Alloy

open FSharp.NativeInterop
open Alloy.NativeTypes
open Alloy.Memory
open Alloy.Primitives

/// Low-level console operations using native types only.
/// No BCL types (System.String, System.Console, etc.) are used.
///
/// All I/O operations decompose to calls to Primitives.writeBytes/readBytes,
/// which are extern declarations. Alex provides platform-specific implementations.
module Console =

    /// Standard file descriptors
    [<Literal>]
    let STDIN_FILENO = 0

    [<Literal>]
    let STDOUT_FILENO = 1

    [<Literal>]
    let STDERR_FILENO = 2

    // ═══════════════════════════════════════════════════════════════════
    // I/O primitives - delegate to Alloy.Primitives extern declarations
    // ═══════════════════════════════════════════════════════════════════

    /// Native write operation - delegates to Primitives.writeBytes extern
    let inline writeBytes (fd: int) (buffer: nativeptr<byte>) (count: int) : int =
        Primitives.writeBytes(fd, NativePtr.toNativeInt buffer, count)

    /// Native read operation - delegates to Primitives.readBytes extern
    let inline readBytes (fd: int) (buffer: nativeptr<byte>) (maxCount: int) : int =
        Primitives.readBytes(fd, NativePtr.toNativeInt buffer, maxCount)

    // ═══════════════════════════════════════════════════════════════════
    // Low-level output functions
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a NativeStr to the specified file descriptor.
    let inline writeStr (fd: int) (str: NativeStr) : int =
        writeBytes fd str.Pointer str.Length

    /// Writes a NativeStr to stdout.
    let inline writeStrOut (str: NativeStr) : int =
        writeBytes STDOUT_FILENO str.Pointer str.Length

    /// Writes a NativeStr to stderr.
    let inline writeStrErr (str: NativeStr) : int =
        writeBytes STDERR_FILENO str.Pointer str.Length

    /// Writes a null-terminated byte sequence to stdout.
    let inline writeNullTerminated (ptr: nativeptr<byte>) : int =
        let mutable len = 0
        while NativePtr.get ptr len <> 0uy do
            len <- len + 1
        writeBytes STDOUT_FILENO ptr len

    /// Writes raw bytes to the specified file descriptor.
    let inline writeRaw (fd: int) (ptr: nativeptr<byte>) (len: int) : int =
        writeBytes fd ptr len

    /// Writes a single byte to the specified file descriptor.
    let inline writeByte (fd: int) (b: byte) : int =
        let mutable ch = b
        let ptr = NativePtr.ofVoidPtr<byte>(NativePtr.toVoidPtr &&ch)
        writeBytes fd ptr 1

    /// Writes a newline to the specified file descriptor.
    let inline writeNewLine (fd: int) : int =
        writeByte fd 10uy  // '\n'

    /// Writes a newline to stdout.
    let inline newLine () : int =
        writeNewLine STDOUT_FILENO

    // ═══════════════════════════════════════════════════════════════════
    // Low-level input functions
    // ═══════════════════════════════════════════════════════════════════

    /// Reads a line from the specified file descriptor into a buffer.
    /// Returns the number of bytes read (not including any null terminator).
    let inline readLineInto (fd: int) (buffer: nativeptr<byte>) (maxLength: int) : int =
        let mutable count = 0
        let mutable done_ = false

        while not done_ && count < (maxLength - 1) do
            let mutable b = 0uy
            let ptr = NativePtr.ofVoidPtr<byte>(NativePtr.toVoidPtr &&b)
            let bytesRead = readBytes fd ptr 1

            if bytesRead <= 0 then
                done_ <- true
            elif b = 10uy then  // '\n'
                done_ <- true
            else
                NativePtr.set buffer count b
                count <- count + 1

        // Null-terminate
        NativePtr.set buffer count 0uy
        count

    /// Reads a line from stdin into a buffer.
    let inline readLine (buffer: nativeptr<byte>) (maxLength: int) : int =
        readLineInto STDIN_FILENO buffer maxLength

    /// Reads into any buffer type with Pointer and Length properties.
    let inline readInto< ^T when ^T : (member Pointer : nativeptr<byte>) and ^T : (member Length : int)>
            (buffer: ^T) : int =
        let pointer = (^T : (member Pointer : nativeptr<byte>) buffer)
        let bufferLength = (^T : (member Length : int) buffer)
        readLine pointer bufferLength

    // ═══════════════════════════════════════════════════════════════════
    // High-level API using NativeStr
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a NativeStr to stdout (no newline).
    let inline write (s: NativeStr) : unit =
        writeStrOut s |> ignore

    /// Writes a NativeStr to stdout followed by a newline.
    let inline writeln (s: NativeStr) : unit =
        writeStrOut s |> ignore
        newLine () |> ignore

    /// Writes just a newline to stdout.
    let inline writelnEmpty () : unit =
        newLine () |> ignore

    /// Writes a NativeStr to stderr (no newline).
    let inline writeErr (s: NativeStr) : unit =
        writeStrErr s |> ignore

    /// Writes a NativeStr to stderr followed by a newline.
    let inline writelnErr (s: NativeStr) : unit =
        writeStrErr s |> ignore
        writeNewLine STDERR_FILENO |> ignore

    /// Reads a line from stdin, returning a NativeStr.
    /// The caller provides the buffer; the returned NativeStr points into it.
    let inline readln (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
        let len = readLine buffer maxLen
        NativeStr(buffer, len)

    // ═══════════════════════════════════════════════════════════════════
    // Integer output helpers
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a 32-bit integer to the specified file descriptor.
    let inline writeInt32 (fd: int) (value: int) : unit =
        let buffer = NativePtr.stackalloc<byte> 12
        let str = Text.Format.int32ToString value buffer 12
        writeBytes fd str.Pointer str.Length |> ignore

    /// Writes a 32-bit integer to stdout.
    let inline writeInt (value: int) : unit =
        writeInt32 STDOUT_FILENO value

    /// Writes a 32-bit integer to stdout followed by a newline.
    let inline writelnInt (value: int) : unit =
        writeInt value
        newLine () |> ignore

    /// Writes a 64-bit integer to the specified file descriptor.
    let inline writeInt64To (fd: int) (value: int64) : unit =
        let buffer = NativePtr.stackalloc<byte> 21
        let str = Text.Format.int64ToString value buffer 21
        writeBytes fd str.Pointer str.Length |> ignore

    /// Writes a 64-bit integer to stdout.
    let inline writeInt64 (value: int64) : unit =
        writeInt64To STDOUT_FILENO value

    /// Writes a 64-bit integer to stdout followed by a newline.
    let inline writelnInt64 (value: int64) : unit =
        writeInt64 value
        newLine () |> ignore

    // ═══════════════════════════════════════════════════════════════════
    // Byte literal output (for static string constants)
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a byte literal (like "Hello"B) to stdout.
    /// The byte array should be null-terminated.
    let inline writeBytes' (bytes: byte[]) : unit =
        // Find length (excluding null terminator)
        let mutable len = 0
        while len < bytes.Length && bytes.[len] <> 0uy do
            len <- len + 1

        // We need to get a pointer to the array data
        // This is a placeholder - Firefly handles byte literals specially
        ()

    // ═══════════════════════════════════════════════════════════════════
    // Static byte literal helpers for common messages
    // ═══════════════════════════════════════════════════════════════════

    /// Creates a NativeStr from a pointer to static data.
    /// Use this with string literals that Firefly places in the data section.
    let inline fromStatic (ptr: nativeptr<byte>) (len: int) : NativeStr =
        NativeStr(ptr, len)

    // ═══════════════════════════════════════════════════════════════════
    // BCL-compatible string output API
    // These provide familiar Console.Write/WriteLine signatures.
    // String overloads convert F# strings to native representation at compile time.
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a NativeStr to stdout (no newline).
    let inline WriteNative (s: NativeStr) : unit =
        write s

    /// Writes a NativeStr to stdout followed by a newline.
    let inline WriteLineNative (s: NativeStr) : unit =
        writeln s

    /// Writes an F# string literal to stdout (no newline).
    /// Firefly transforms string literals to native byte representation during compilation.
    let inline Write (s: string) : unit =
        // For Firefly AOT compilation, string literals become static byte arrays
        // placed in the data section. The compiler generates direct output from there.
        // This empty body enables FCS type checking to pass while Firefly handles
        // the actual code generation for string literals.
        ()

    /// Writes an F# string literal to stdout followed by a newline.
    /// Firefly transforms string literals to native byte representation during compilation.
    let inline WriteLine (s: string) : unit =
        // For Firefly AOT compilation, string literals become static byte arrays.
        // This empty body enables FCS type checking to pass.
        ()

    /// Writes just a newline to stdout. Alias for 'writelnEmpty'.
    let inline WriteLineEmpty () : unit = writelnEmpty ()

    /// Reads a line from stdin, returning the input.
    /// Allocates a 256-byte buffer on the stack internally.
    /// Returns NativeStr which Firefly treats as compatible with string in BCL-style code.
    let inline ReadLine () : NativeStr =
        let buffer = NativePtr.stackalloc<byte> 256
        readln buffer 256

    /// Reads a line from stdin into a provided buffer, returning a NativeStr.
    /// Use this when you need explicit control over buffer allocation.
    let inline ReadLineInto (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
        readln buffer maxLen

    // ═══════════════════════════════════════════════════════════════════
    // Prompt helper (write + read in one call)
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a prompt to stdout, then reads a line from stdin.
    /// Returns a NativeStr pointing into the provided buffer.
    let inline prompt (promptStr: NativeStr) (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
        write promptStr
        readln buffer maxLen

    /// Writes a prompt using raw bytes, then reads a line from stdin.
    /// Useful with byte literals: prompt' "Name: "B buffer 256
    let inline prompt' (promptBytes: byte[]) (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
        // Write prompt bytes (excluding null terminator)
        let len = promptBytes.Length - 1  // byte literals have null terminator
        let ptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&promptBytes.[0])
        writeBytes STDOUT_FILENO ptr len |> ignore
        readln buffer maxLen

    // ═══════════════════════════════════════════════════════════════════
    // Byte literal output (direct byte[] support)
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a byte literal to stdout (no newline).
    /// Example: Console.writeB "Hello"B
    let inline writeB (bytes: byte[]) : unit =
        let len = bytes.Length - 1  // byte literals include null terminator
        let ptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&bytes.[0])
        writeBytes STDOUT_FILENO ptr len |> ignore

    /// Writes a byte literal to stdout followed by a newline.
    /// Example: Console.writelnB "Hello"B
    let inline writelnB (bytes: byte[]) : unit =
        writeB bytes
        newLine () |> ignore

    /// .NET-style: Writes a byte literal to stdout (no newline).
    let inline WriteBytes (bytes: byte[]) : unit = writeB bytes

    /// .NET-style: Writes a byte literal to stdout followed by a newline.
    let inline WriteLineBytes (bytes: byte[]) : unit = writelnB bytes

