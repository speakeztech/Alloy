#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Pure F# UTF-8 encoding/decoding implementation
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
    /// <returns>A NativeStr pointing to the encoded bytes in the buffer</returns>
    let inline getBytesTo (dest: nativeptr<byte>) (maxLen: int) (s: string) : NativeStr =
        if s.Length = 0 then
            NativeStr(Unchecked.defaultof<nativeptr<byte>>, 0)
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
            NativeStr(dest, pos)

    // ═══════════════════════════════════════════════════════════════════
    // Additional native string operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Calculates the UTF-8 encoded length of a NativeStr.
    /// For ASCII strings, this equals the byte length.
    /// For multi-byte UTF-8, counts decoded codepoints.
    /// </summary>
    /// <param name="s">The NativeStr to measure</param>
    /// <returns>The number of Unicode codepoints in the string</returns>
    let inline calculateCodepointCount (s: NativeStr) : int =
        if s.Length = 0 then 0
        else
            let mutable count = 0
            let mutable i = 0
            while i < s.Length do
                let b = int (NativePtr.get s.Pointer i)
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
    /// Checks if a NativeStr contains only ASCII characters (bytes &lt; 128).
    /// </summary>
    let inline isAscii (s: NativeStr) : bool =
        let mutable allAscii = true
        let mutable i = 0
        while allAscii && i < s.Length do
            if int (NativePtr.get s.Pointer i) >= 0x80 then
                allAscii <- false
            i <- i + 1
        allAscii