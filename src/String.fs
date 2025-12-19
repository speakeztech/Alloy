#nowarn "9"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Core
open Alloy.Numerics

/// <summary>
/// Native string operations for Alloy.
/// Provides familiar F# string API patterns that compile to native code.
///
/// Design principles:
/// - No null concept - strings are always valid NativeStr values
/// - Use Option&lt;NativeStr&gt; for optional/absent strings
/// - API surface similar to BCL but without null-related operations
/// </summary>
[<AutoOpen>]
module String =

    // ═══════════════════════════════════════════════════════════════════
    // Basic Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Returns the length of a string in bytes.</summary>
    let inline length (s: NativeStr) : int = s.Length

    /// <summary>Checks if a string is empty (zero length).</summary>
    let inline isEmpty (s: NativeStr) : bool = s.Length = 0

    /// <summary>Gets the character at the specified index.</summary>
    let inline charAt (index: int) (s: NativeStr) : char = s.[index]

    /// <summary>Gets the byte at the specified index.</summary>
    let inline byteAt (index: int) (s: NativeStr) : byte =
        if index < 0 || index >= s.Length then 0uy
        else NativePtr.get s.Pointer index

    // ═══════════════════════════════════════════════════════════════════
    // Concatenation (requires caller-provided buffer)
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Concatenates two strings into a destination buffer.</summary>
    let inline concat2 (dest: nativeptr<byte>) (s1: NativeStr) (s2: NativeStr) : NativeStr =
        NativeString.concat2 dest s1 s2

    /// <summary>Concatenates three strings into a destination buffer.</summary>
    let inline concat3 (dest: nativeptr<byte>) (s1: NativeStr) (s2: NativeStr) (s3: NativeStr) : NativeStr =
        NativeString.concat3 dest s1 s2 s3

    // ═══════════════════════════════════════════════════════════════════
    // Comparison
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Checks if two strings are equal.</summary>
    let inline equals (s1: NativeStr) (s2: NativeStr) : bool =
        s1.EqualsStr(s2)

    /// <summary>Compares two strings byte-by-byte.</summary>
    let inline compare (s1: NativeStr) (s2: NativeStr) : int =
        let minLen = if s1.Length < s2.Length then s1.Length else s2.Length
        let mutable result = 0
        let mutable i = 0
        while result = 0 && i < minLen do
            let b1 = NativePtr.get s1.Pointer i
            let b2 = NativePtr.get s2.Pointer i
            if b1 < b2 then result <- -1
            elif b1 > b2 then result <- 1
            i <- i + 1
        if result = 0 then
            if s1.Length < s2.Length then -1
            elif s1.Length > s2.Length then 1
            else 0
        else result

    // ═══════════════════════════════════════════════════════════════════
    // Character Classification
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Checks if a character is a digit (0-9).</summary>
    let inline isDigit (c: char) : bool =
        c >= '0' && c <= '9'

    /// <summary>Checks if a character is whitespace.</summary>
    let inline isWhitespace (c: char) : bool =
        c = ' ' || c = '\t' || c = '\n' || c = '\r'

    /// <summary>Checks if a character is a letter (a-z, A-Z).</summary>
    let inline isLetter (c: char) : bool =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    /// <summary>Converts a character to lowercase.</summary>
    let inline toLowerChar (c: char) : char =
        if c >= 'A' && c <= 'Z' then char (int c + 32)
        else c

    /// <summary>Converts a character to uppercase.</summary>
    let inline toUpperChar (c: char) : char =
        if c >= 'a' && c <= 'z' then char (int c - 32)
        else c

    // ═══════════════════════════════════════════════════════════════════
    // Substring Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Gets a substring starting at the given index with the given length.</summary>
    let inline substring (start: int) (len: int) (s: NativeStr) : NativeStr =
        if start < 0 || start >= s.Length || len <= 0 then
            NativeStr.Empty
        else
            let actualLen = if start + len > s.Length then s.Length - start else len
            NativeStr(NativePtr.add s.Pointer start, actualLen)

    /// <summary>Gets a substring from the given index to the end.</summary>
    let inline substringFrom (start: int) (s: NativeStr) : NativeStr =
        if start < 0 || start >= s.Length then
            NativeStr.Empty
        else
            NativeStr(NativePtr.add s.Pointer start, s.Length - start)

    // ═══════════════════════════════════════════════════════════════════
    // Search Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Checks if a string starts with the given prefix.</summary>
    let inline startsWith (prefix: NativeStr) (s: NativeStr) : bool =
        if prefix.Length > s.Length then false
        else
            let mutable matches = true
            let mutable i = 0
            while matches && i < prefix.Length do
                if NativePtr.get s.Pointer i <> NativePtr.get prefix.Pointer i then
                    matches <- false
                i <- i + 1
            matches

    /// <summary>Checks if a string ends with the given suffix.</summary>
    let inline endsWith (suffix: NativeStr) (s: NativeStr) : bool =
        if suffix.Length > s.Length then false
        else
            let offset = s.Length - suffix.Length
            let mutable matches = true
            let mutable i = 0
            while matches && i < suffix.Length do
                if NativePtr.get s.Pointer (offset + i) <> NativePtr.get suffix.Pointer i then
                    matches <- false
                i <- i + 1
            matches

    /// <summary>Checks if a string contains the given substring.</summary>
    let inline contains (needle: NativeStr) (s: NativeStr) : bool =
        if needle.Length = 0 then true
        elif needle.Length > s.Length then false
        else
            let mutable found = false
            let mutable i = 0
            while not found && i <= s.Length - needle.Length do
                let mutable matches = true
                let mutable j = 0
                while matches && j < needle.Length do
                    if NativePtr.get s.Pointer (i + j) <> NativePtr.get needle.Pointer j then
                        matches <- false
                    j <- j + 1
                if matches then found <- true
                i <- i + 1
            found

    /// <summary>Returns the index of the first occurrence of the substring, or -1 if not found.</summary>
    let inline indexOf (needle: NativeStr) (s: NativeStr) : int =
        if needle.Length = 0 then 0
        elif needle.Length > s.Length then -1
        else
            let mutable result = -1
            let mutable i = 0
            while result = -1 && i <= s.Length - needle.Length do
                let mutable matches = true
                let mutable j = 0
                while matches && j < needle.Length do
                    if NativePtr.get s.Pointer (i + j) <> NativePtr.get needle.Pointer j then
                        matches <- false
                    j <- j + 1
                if matches then result <- i
                i <- i + 1
            result

    // ═══════════════════════════════════════════════════════════════════
    // Trim Operations (return views into original string)
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Removes leading whitespace, returning a view into the original string.</summary>
    let inline trimStart (s: NativeStr) : NativeStr =
        let mutable start = 0
        while start < s.Length && isWhitespace s.[start] do
            start <- start + 1
        if start = 0 then s
        elif start >= s.Length then NativeStr.Empty
        else NativeStr(NativePtr.add s.Pointer start, s.Length - start)

    /// <summary>Removes trailing whitespace, returning a view into the original string.</summary>
    let inline trimEnd (s: NativeStr) : NativeStr =
        let mutable endIdx = s.Length - 1
        while endIdx >= 0 && isWhitespace s.[endIdx] do
            endIdx <- endIdx - 1
        if endIdx = s.Length - 1 then s
        elif endIdx < 0 then NativeStr.Empty
        else NativeStr(s.Pointer, endIdx + 1)

    /// <summary>Removes leading and trailing whitespace, returning a view into the original string.</summary>
    let inline trim (s: NativeStr) : NativeStr =
        trimStart s |> trimEnd

    // ═══════════════════════════════════════════════════════════════════
    // Conversion (using ToNativeString pattern)
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Converts a value to NativeStr using SRTP.
    /// Types must implement ToNativeString member for native string conversion.
    /// </summary>
    let inline toNativeString< ^T when ^T : (member ToNativeString : unit -> NativeStr)> (x: ^T) : NativeStr =
        (^T : (member ToNativeString : unit -> NativeStr) x)
