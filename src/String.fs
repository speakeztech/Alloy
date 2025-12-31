#nowarn "9"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Core
open Alloy.Numerics

/// <summary>
/// Native string operations for Alloy.
/// Provides familiar F# string API patterns that compile to native code.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, string has: Pointer (nativeptr<byte>), Length (int)
///
/// Design principles:
/// - No null concept - strings are always valid values
/// - Use option<string> for optional/absent strings
/// - API surface similar to BCL but without null-related operations
/// </summary>
[<AutoOpen>]
module String =

    // ═══════════════════════════════════════════════════════════════════
    // Basic Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Returns the length of a string in characters.</summary>
    let inline length (s: string) : int = s.Length

    /// <summary>Checks if a string is empty (zero length).</summary>
    let inline isEmpty (s: string) : bool = s.Length = 0

    /// <summary>Gets the character at the specified index.</summary>
    let inline charAt (index: int) (s: string) : char = s.[index]

    // ═══════════════════════════════════════════════════════════════════
    // Concatenation
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Concatenates two strings.</summary>
    let inline concat2 (s1: string) (s2: string) : string = s1 + s2

    /// <summary>Concatenates three strings.</summary>
    let inline concat3 (s1: string) (s2: string) (s3: string) : string = s1 + s2 + s3

    // ═══════════════════════════════════════════════════════════════════
    // Comparison
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Checks if two strings are equal.</summary>
    let inline equals (s1: string) (s2: string) : bool = s1 = s2

    /// <summary>Compares two strings.</summary>
    let inline compare (s1: string) (s2: string) : int =
        System.String.Compare(s1, s2, System.StringComparison.Ordinal)

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
    let inline substring (start: int) (len: int) (s: string) : string =
        if start < 0 || start >= s.Length || len <= 0 then ""
        else
            let actualLen = if start + len > s.Length then s.Length - start else len
            s.Substring(start, actualLen)

    /// <summary>Gets a substring from the given index to the end.</summary>
    let inline substringFrom (start: int) (s: string) : string =
        if start < 0 || start >= s.Length then ""
        else s.Substring(start)

    // ═══════════════════════════════════════════════════════════════════
    // Search Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Checks if a string starts with the given prefix.</summary>
    let inline startsWith (prefix: string) (s: string) : bool =
        s.StartsWith(prefix)

    /// <summary>Checks if a string ends with the given suffix.</summary>
    let inline endsWith (suffix: string) (s: string) : bool =
        s.EndsWith(suffix)

    /// <summary>Checks if a string contains the given substring.</summary>
    let inline contains (needle: string) (s: string) : bool =
        s.Contains(needle)

    /// <summary>Returns the index of the first occurrence of the substring, or -1 if not found.</summary>
    let inline indexOf (needle: string) (s: string) : int =
        s.IndexOf(needle)

    // ═══════════════════════════════════════════════════════════════════
    // Trim Operations
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>Removes leading whitespace.</summary>
    let inline trimStart (s: string) : string = s.TrimStart()

    /// <summary>Removes trailing whitespace.</summary>
    let inline trimEnd (s: string) : string = s.TrimEnd()

    /// <summary>Removes leading and trailing whitespace.</summary>
    let inline trim (s: string) : string = s.Trim()

    // ═══════════════════════════════════════════════════════════════════
    // Conversion
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Converts a value to string using SRTP.
    /// Types must implement ToString member for string conversion.
    /// </summary>
    let inline toString< ^T when ^T : (member ToString : unit -> string)> (x: ^T) : string =
        (^T : (member ToString : unit -> string) x)
