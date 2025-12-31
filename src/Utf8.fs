#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Pure F# UTF-8 encoding/decoding implementation.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, string has: Pointer (nativeptr<byte>), Length (int)
///
/// For .NET compatibility, uses BCL encoding utilities.
/// FNCS compilation uses direct member access on string.
/// </summary>
module Utf8 =

    // ═══════════════════════════════════════════════════════════════════
    // Native (freestanding-compatible) encoding
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Encodes a string to UTF-8 bytes in a provided buffer.
    /// This is the native path for freestanding compilation - no BCL dependencies.
    /// </summary>
    /// <param name="dest">Destination buffer pointer</param>
    /// <param name="maxLen">Maximum bytes to write</param>
    /// <param name="s">The string to encode</param>
    /// <returns>The number of bytes written</returns>
    let inline getBytesTo (dest: nativeptr<byte>) (maxLen: int) (s: string) : int =
        if s.Length = 0 then
            0
        else
            let mutable pos = 0
            let mutable i = 0
            while i < s.Length && pos < maxLen do
                let c = int s.[i]
                if c < 0x80 then
                    // 1-byte sequence: ASCII
                    NativePtr.set dest pos (byte c)
                    pos <- pos + 1
                elif c < 0x800 then
                    // 2-byte sequence
                    if pos + 2 <= maxLen then
                        NativePtr.set dest pos (byte (0xC0 ||| (c >>> 6)))
                        NativePtr.set dest (pos + 1) (byte (0x80 ||| (c &&& 0x3F)))
                        pos <- pos + 2
                elif c < 0x10000 then
                    // 3-byte sequence
                    if pos + 3 <= maxLen then
                        NativePtr.set dest pos (byte (0xE0 ||| (c >>> 12)))
                        NativePtr.set dest (pos + 1) (byte (0x80 ||| ((c >>> 6) &&& 0x3F)))
                        NativePtr.set dest (pos + 2) (byte (0x80 ||| (c &&& 0x3F)))
                        pos <- pos + 3
                else
                    // 4-byte sequence
                    if pos + 4 <= maxLen then
                        NativePtr.set dest pos (byte (0xF0 ||| (c >>> 18)))
                        NativePtr.set dest (pos + 1) (byte (0x80 ||| ((c >>> 12) &&& 0x3F)))
                        NativePtr.set dest (pos + 2) (byte (0x80 ||| ((c >>> 6) &&& 0x3F)))
                        NativePtr.set dest (pos + 3) (byte (0x80 ||| (c &&& 0x3F)))
                        pos <- pos + 4
                i <- i + 1
            pos

    // ═══════════════════════════════════════════════════════════════════
    // UTF-8 byte sequence analysis
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Calculates the number of Unicode codepoints in a UTF-8 byte sequence.
    /// For ASCII strings, this equals the byte length.
    /// For multi-byte UTF-8, counts decoded codepoints.
    /// </summary>
    /// <param name="ptr">Pointer to UTF-8 bytes</param>
    /// <param name="len">Length in bytes</param>
    /// <returns>The number of Unicode codepoints</returns>
    let inline calculateCodepointCount (ptr: nativeptr<byte>) (len: int) : int =
        if len = 0 then 0
        else
            let mutable count = 0
            let mutable i = 0
            while i < len do
                let b = int (NativePtr.get ptr i)
                if b < 0x80 then
                    // 1-byte sequence
                    count <- count + 1
                    i <- i + 1
                elif b < 0xE0 then
                    // 2-byte sequence
                    count <- count + 1
                    i <- i + 2
                elif b < 0xF0 then
                    // 3-byte sequence
                    count <- count + 1
                    i <- i + 3
                else
                    // 4-byte sequence
                    count <- count + 1
                    i <- i + 4
            count

    /// <summary>
    /// Checks if a UTF-8 byte sequence contains only ASCII characters (bytes &lt; 128).
    /// </summary>
    /// <param name="ptr">Pointer to UTF-8 bytes</param>
    /// <param name="len">Length in bytes</param>
    /// <returns>True if all bytes are ASCII</returns>
    let inline isAscii (ptr: nativeptr<byte>) (len: int) : bool =
        let mutable allAscii = true
        let mutable i = 0
        while allAscii && i < len do
            if int (NativePtr.get ptr i) >= 0x80 then
                allAscii <- false
            i <- i + 1
        allAscii

    // ═══════════════════════════════════════════════════════════════════
    // String-based operations (for .NET compatibility)
    // FNCS provides string with Pointer/Length for native compilation
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Gets the UTF-8 byte count for a string.
    /// For .NET compat, uses BCL encoding. FNCS uses string.Length directly (already UTF-8).
    /// </summary>
    let inline byteCount (s: string) : int =
        System.Text.Encoding.UTF8.GetByteCount(s)

    /// <summary>
    /// Gets UTF-8 bytes from a string as a byte array.
    /// For .NET compat, uses BCL encoding. FNCS constructs from string.Pointer/Length.
    /// </summary>
    let inline getBytes (s: string) : byte[] =
        System.Text.Encoding.UTF8.GetBytes(s)

    /// <summary>
    /// Creates a string from UTF-8 bytes.
    /// For .NET compat, uses BCL encoding. FNCS constructs directly from (ptr, len).
    /// </summary>
    let inline fromBytes (bytes: byte[]) : string =
        System.Text.Encoding.UTF8.GetString(bytes)
