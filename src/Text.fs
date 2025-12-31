#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Zero-allocation text processing for native compilation.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, string has: Pointer (nativeptr<byte>), Length (int)
///
/// For .NET compatibility, operations that need low-level byte access use
/// BCL encoding utilities. FNCS compilation uses direct member access.
module Text =

    // ═══════════════════════════════════════════════════════════════════
    // UTF-8 Encoding/Decoding
    // ═══════════════════════════════════════════════════════════════════

    module UTF8 =
        /// Encodes a single Unicode codepoint to UTF-8 bytes.
        /// Returns the number of bytes written (1-4).
        let inline encodeCodepoint (codepoint: int) (buffer: nativeptr<byte>) (offset: int) : int =
            if codepoint < 0x80 then
                // ASCII (1 byte)
                NativePtr.set buffer offset (byte codepoint)
                1
            elif codepoint < 0x800 then
                // 2-byte sequence
                NativePtr.set buffer offset (0xC0uy ||| byte (codepoint >>> 6))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte (codepoint &&& 0x3F))
                2
            elif codepoint < 0x10000 then
                // 3-byte sequence
                NativePtr.set buffer offset (0xE0uy ||| byte (codepoint >>> 12))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte ((codepoint >>> 6) &&& 0x3F))
                NativePtr.set buffer (offset + 2) (0x80uy ||| byte (codepoint &&& 0x3F))
                3
            else
                // 4-byte sequence
                NativePtr.set buffer offset (0xF0uy ||| byte (codepoint >>> 18))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte ((codepoint >>> 12) &&& 0x3F))
                NativePtr.set buffer (offset + 2) (0x80uy ||| byte ((codepoint >>> 6) &&& 0x3F))
                NativePtr.set buffer (offset + 3) (0x80uy ||| byte (codepoint &&& 0x3F))
                4

        /// Decodes a UTF-8 byte sequence to a Unicode codepoint.
        /// Returns (codepoint, bytes_consumed).
        let inline decodeCodepoint (buffer: nativeptr<byte>) (offset: int) (maxLen: int) : struct (int * int) =
            if maxLen <= 0 then
                struct (0, 0)
            else
                let b0 = NativePtr.get buffer offset
                if b0 < 0x80uy then
                    // ASCII
                    struct (int b0, 1)
                elif b0 < 0xC0uy then
                    // Invalid continuation byte at start
                    struct (0xFFFD, 1)  // Replacement character
                elif b0 < 0xE0uy then
                    // 2-byte sequence
                    if maxLen < 2 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let cp = ((int (b0 &&& 0x1Fuy)) <<< 6) ||| (int (b1 &&& 0x3Fuy))
                        struct (cp, 2)
                elif b0 < 0xF0uy then
                    // 3-byte sequence
                    if maxLen < 3 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let b2 = NativePtr.get buffer (offset + 2)
                        let cp = ((int (b0 &&& 0x0Fuy)) <<< 12) |||
                                 ((int (b1 &&& 0x3Fuy)) <<< 6) |||
                                 (int (b2 &&& 0x3Fuy))
                        struct (cp, 3)
                else
                    // 4-byte sequence
                    if maxLen < 4 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let b2 = NativePtr.get buffer (offset + 2)
                        let b3 = NativePtr.get buffer (offset + 3)
                        let cp = ((int (b0 &&& 0x07uy)) <<< 18) |||
                                 ((int (b1 &&& 0x3Fuy)) <<< 12) |||
                                 ((int (b2 &&& 0x3Fuy)) <<< 6) |||
                                 (int (b3 &&& 0x3Fuy))
                        struct (cp, 4)

        /// Counts the number of Unicode codepoints in a UTF-8 byte sequence.
        /// For .NET compat, uses BCL encoding. FNCS uses direct byte access.
        let inline countCodepoints (str: string) : int =
            // For .NET compat
            str.Length  // This counts UTF-16 code units, not codepoints
            // FNCS would use: direct byte traversal of str.Pointer/str.Length

        /// Gets UTF-8 bytes from a string.
        /// For .NET compat, uses BCL encoding. FNCS uses direct access.
        let inline getBytes (str: string) : byte[] =
            System.Text.Encoding.UTF8.GetBytes(str)

        /// Creates a string from UTF-8 bytes.
        /// For .NET compat, uses BCL encoding. FNCS constructs directly.
        let inline fromBytes (bytes: byte[]) : string =
            System.Text.Encoding.UTF8.GetString(bytes)

    // ═══════════════════════════════════════════════════════════════════
    // String Operations
    // FNCS provides string with Pointer/Length. These are library functions.
    // ═══════════════════════════════════════════════════════════════════

    module Str =
        /// Returns true if the string is empty or null.
        let inline isEmpty (s: string) : bool =
            System.String.IsNullOrEmpty(s)

        /// Returns the byte length of the string (UTF-8).
        /// For .NET compat, encodes to get byte count.
        let inline byteLength (s: string) : int =
            System.Text.Encoding.UTF8.GetByteCount(s)

        /// Returns the character length of the string.
        let inline length (s: string) : int =
            s.Length

        /// Compares two strings for equality.
        let inline equals (a: string) (b: string) : bool =
            a = b

        /// Compares two strings lexicographically.
        /// Returns: negative if a < b, zero if a = b, positive if a > b.
        let inline compare (a: string) (b: string) : int =
            System.String.Compare(a, b, System.StringComparison.Ordinal)

        /// Checks if a string starts with a prefix.
        let inline startsWith (prefix: string) (s: string) : bool =
            s.StartsWith(prefix)

        /// Checks if a string ends with a suffix.
        let inline endsWith (suffix: string) (s: string) : bool =
            s.EndsWith(suffix)

        /// Creates a substring of the string.
        let inline slice (start: int) (len: int) (s: string) : string =
            if start < 0 || start >= s.Length then ""
            else
                let actualLen = if start + len > s.Length then s.Length - start else len
                s.Substring(start, actualLen)

        /// Finds the first occurrence of a substring.
        /// Returns the index, or -1 if not found.
        let inline indexOf (needle: string) (haystack: string) : int =
            haystack.IndexOf(needle)

        /// Counts occurrences of a substring.
        let inline countOccurrences (needle: string) (haystack: string) : int =
            if System.String.IsNullOrEmpty(needle) then 0
            else
                let mutable count = 0
                let mutable startIndex = 0
                while startIndex < haystack.Length do
                    let foundIndex = haystack.IndexOf(needle, startIndex)
                    if foundIndex >= 0 then
                        count <- count + 1
                        startIndex <- foundIndex + needle.Length
                    else
                        startIndex <- haystack.Length
                count

        /// Concatenates two strings.
        let inline concat (a: string) (b: string) : string =
            a + b

        /// Replaces all occurrences of oldValue with newValue.
        let inline replace (original: string) (oldValue: string) (newValue: string) : string =
            original.Replace(oldValue, newValue)

    // ═══════════════════════════════════════════════════════════════════
    // Integer to String Conversion
    // ═══════════════════════════════════════════════════════════════════

    module Format =
        /// Converts a 32-bit signed integer to a string.
        let inline int32ToString (value: int) : string =
            string value

        /// Converts a 64-bit signed integer to a string.
        let inline int64ToString (value: int64) : string =
            string value

        /// Converts a 32-bit unsigned integer to hexadecimal.
        let inline uint32ToHex (value: uint32) (uppercase: bool) : string =
            if uppercase then
                value.ToString("X")
            else
                value.ToString("x")

        /// Converts a 64-bit unsigned integer to hexadecimal.
        let inline uint64ToHex (value: uint64) (uppercase: bool) : string =
            if uppercase then
                value.ToString("X")
            else
                value.ToString("x")

        /// Converts an integer to a string with padding.
        let inline int32ToStringPadded (value: int) (width: int) (padChar: char) : string =
            let s = string value
            if s.Length >= width then s
            else System.String(padChar, width - s.Length) + s
