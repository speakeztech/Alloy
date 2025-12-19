#nowarn "9"
#nowarn "1182"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Memory
open Alloy.Primitives

/// Low-level console operations using native types only.
/// No BCL types (System.String, System.Console, etc.) are used.
///
/// Core I/O primitives are in Primitives.fs - this module builds on them.
/// Alex provides platform-specific implementations.
module Console =

    /// Standard file descriptors (re-exported from Primitives for convenience)
    let STDIN_FILENO = STDIN
    let STDOUT_FILENO = STDOUT
    let STDERR_FILENO = STDERR

    // ═══════════════════════════════════════════════════════════════════
    // I/O primitives - delegate to Primitives.Bindings (BCL-free)
    // ═══════════════════════════════════════════════════════════════════

    /// Native write operation - delegates to Primitives.Bindings.writeBytes
    let inline writeBytes (fd: int) (buffer: nativeptr<byte>) (count: int) : int =
        Bindings.writeBytes fd (NativePtr.toNativeInt buffer) count

    /// Native read operation - delegates to Primitives.Bindings.readBytes
    let inline readBytes (fd: int) (buffer: nativeptr<byte>) (maxCount: int) : int =
        Bindings.readBytes fd (NativePtr.toNativeInt buffer) maxCount

    // ═══════════════════════════════════════════════════════════════════
    // Low-level output functions
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a NativeStr to the specified file descriptor.
    let inline writeStr (fd: int) (str: NativeStr) : int =
        Primitives.writeStr fd str

    /// Writes a NativeStr to stdout.
    let inline writeStrOut (str: NativeStr) : int =
        Primitives.writeStr STDOUT str

    /// Writes a NativeStr to stderr.
    let inline writeStrErr (str: NativeStr) : int =
        Primitives.writeStr STDERR str

    /// Writes a null-terminated byte sequence to stdout.
    let inline writeNullTerminated (ptr: nativeptr<byte>) : int =
        let mutable len = 0
        while NativePtr.get ptr len <> 0uy do
            len <- len + 1
        writeBytes STDOUT ptr len

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
        writeNewLine STDOUT

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
        readLineInto 0 buffer maxLength

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
        writeNewLine 2 |> ignore

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
        writeInt32 1 value

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
        writeInt64To 1 value

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
    // ═══════════════════════════════════════════════════════════════════
    // BCL-compatible string output API
    // These provide familiar Console.Write/WriteLine signatures.
    // ═══════════════════════════════════════════════════════════════════
    //
    // Uses SRTP to enable both NativeStr and string to work with Write/WriteLine.
    // For string literals, Firefly performs compile-time transformation:
    // 1. The string literal bytes are placed in the data section
    // 2. A NativeStr (ptr, len) is constructed pointing to them
    // 3. The write function is called with that NativeStr
    //
    // At .NET runtime, strings are encoded to UTF-8 using Alloy.Utf8.getBytes
    // (pure F# implementation) and written. No BCL System.Text.Encoding dependency.
    // ═══════════════════════════════════════════════════════════════════

    /// Internal: Write a NativeStr to stdout
    let inline private writeNativeStr (s: NativeStr) : unit =
        write s

    /// Type that enables polymorphic Write operations via SRTP
    /// In native compilation, string = NativeStr, so we only need one overload.
    type WritableString =
        | WritableString

        static member inline ($) (WritableString, s: NativeStr) = writeNativeStr s

    /// Writes to stdout (no newline). Accepts both NativeStr and string.
    let inline Write s = WritableString $ s

    /// Writes to stdout followed by a newline. Accepts both NativeStr and string.
    let inline WriteLine s =
        WritableString $ s
        newLine () |> ignore

    /// Writes just a newline to stdout.
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
        writeBytes 1 ptr len |> ignore
        readln buffer maxLen

    // ═══════════════════════════════════════════════════════════════════
    // Byte literal output (direct byte[] support)
    // ═══════════════════════════════════════════════════════════════════

    /// Writes a byte literal to stdout (no newline).
    /// Example: Console.writeB "Hello"B
    let inline writeB (bytes: byte[]) : unit =
        let len = bytes.Length - 1  // byte literals include null terminator
        let ptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&bytes.[0])
        writeBytes 1 ptr len |> ignore

    /// Writes a byte literal to stdout followed by a newline.
    /// Example: Console.writelnB "Hello"B
    let inline writelnB (bytes: byte[]) : unit =
        writeB bytes
        newLine () |> ignore

    /// .NET-style: Writes a byte literal to stdout (no newline).
    let inline WriteBytes (bytes: byte[]) : unit = writeB bytes

    /// .NET-style: Writes a byte literal to stdout followed by a newline.
    let inline WriteLineBytes (bytes: byte[]) : unit = writelnB bytes

