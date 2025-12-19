#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Native string type for freestanding compilation.
/// This is a fat pointer (pointer + length) that Firefly emits as an MLIR struct.
/// Similar to Rust's &str or Fable's LrcStr for the Rust backend.
/// </summary>
[<AutoOpen>]
module NativeString =

    /// <summary>
    /// A native string is a pointer to UTF-8 bytes plus a length.
    /// This struct is emitted by Firefly as: !llvm.struct<(ptr, i64)>
    /// API-compatible with BCL string for familiar F# patterns.
    /// </summary>
    [<Struct>]
    type NativeStr =
        val Pointer: nativeptr<byte>
        val Length: int

        new (ptr: nativeptr<byte>, len: int) =
            { Pointer = ptr; Length = len }

        /// Indexer for character access (returns char from UTF-8 byte)
        member this.Item
            with get(index: int) : char =
                if index < 0 || index >= this.Length then
                    char 0
                else
                    char (NativePtr.get this.Pointer index)

        /// Checks if this string equals another (byte-by-byte comparison)
        member this.EqualsStr(other: NativeStr) : bool =
            if this.Length <> other.Length then false
            else
                let mutable equal = true
                let mutable i = 0
                while equal && i < this.Length do
                    if NativePtr.get this.Pointer i <> NativePtr.get other.Pointer i then
                        equal <- false
                    i <- i + 1
                equal

        /// Returns empty string for null-like behavior
        static member Empty = NativeStr(NativePtr.nullPtr<byte>, 0)

    /// <summary>
    /// Shadow type for BCL string. When Alloy.NativeTypes is opened,
    /// 'string' resolves to NativeStr for native compilation.
    /// </summary>
    type string = NativeStr

    /// <summary>
    /// Creates a NativeStr from a pointer and length.
    /// This is the primary constructor used by Console.ReadLine and similar operations.
    /// </summary>
    let inline create (ptr: nativeptr<byte>) (len: int) : NativeStr =
        NativeStr(ptr, len)

    /// <summary>
    /// Creates an empty NativeStr.
    /// Returns a struct with null pointer and zero length.
    /// </summary>
    let inline empty () : NativeStr =
        NativeStr(NativePtr.nullPtr<byte>, 0)

    /// <summary>
    /// Returns true if the string is empty.
    /// </summary>
    let inline isEmpty (s: NativeStr) : bool =
        s.Length = 0

    /// <summary>
    /// Returns the length of the string in bytes.
    /// </summary>
    let inline length (s: NativeStr) : int =
        s.Length

    /// <summary>
    /// Gets the byte at the specified index.
    /// </summary>
    let inline byteAt (index: int) (s: NativeStr) : byte =
        if index < 0 || index >= s.Length then
            0uy
        else
            NativePtr.get s.Pointer index

    /// <summary>
    /// Writes the contents of a NativeStr to a destination buffer.
    /// Returns the number of bytes written.
    /// </summary>
    let inline copyTo (dest: nativeptr<byte>) (s: NativeStr) : int =
        for i = 0 to s.Length - 1 do
            NativePtr.set dest i (NativePtr.get s.Pointer i)
        s.Length

    /// <summary>
    /// ByteArray is a type alias for NativeStr.
    /// Used for byte array literals ("..."B) which compile to the same
    /// representation as native strings: a pointer to bytes plus length.
    /// </summary>
    type ByteArray = NativeStr

    /// <summary>
    /// ByteArray operations module.
    /// </summary>
    [<RequireQualifiedAccess>]
    module ByteArray =
        /// Creates a ByteArray from a pointer and length.
        let inline create (ptr: nativeptr<byte>) (len: int) : ByteArray =
            NativeStr(ptr, len)

        /// Returns the length of the byte array.
        let inline length (arr: ByteArray) : int =
            arr.Length

        /// Returns the pointer to the byte data.
        let inline pointer (arr: ByteArray) : nativeptr<byte> =
            arr.Pointer

        /// Gets the byte at the specified index.
        let inline get (index: int) (arr: ByteArray) : byte =
            NativePtr.get arr.Pointer index

        /// Creates an empty ByteArray.
        let inline empty () : ByteArray =
            NativeStr(NativePtr.nullPtr<byte>, 0)

    // ═══════════════════════════════════════════════════════════════════
    // Byte literal helpers
    // These make it easier to work with F# byte literals ("..."B)
    // ═══════════════════════════════════════════════════════════════════

    /// Creates a NativeStr from a byte literal (byte[]).
    /// Automatically handles the null terminator that F# adds.
    /// Example: let hello = NativeStr.ofBytes "Hello"B
    let inline ofBytes (bytes: byte[]) : NativeStr =
        let len = bytes.Length - 1  // F# byte literals include null terminator
        let ptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&bytes.[0])
        NativeStr(ptr, len)

    /// Gets a pointer to the start of a byte literal.
    /// Useful when you need the raw pointer.
    let inline bytesPtr (bytes: byte[]) : nativeptr<byte> =
        NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&bytes.[0])

    /// Gets the length of a byte literal (excluding null terminator).
    let inline bytesLen (bytes: byte[]) : int =
        bytes.Length - 1

    // ═══════════════════════════════════════════════════════════════════════
    // Semantic String Primitives
    // ═══════════════════════════════════════════════════════════════════════
    // These primitives express INTENT for string operations.
    // The Firefly compiler (via Alex) will lower these to target-optimal code.
    // At the F# level, these delegate to Memory.copy for .NET compat.
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Copy a NativeStr to a destination buffer, returning a new NativeStr.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="dest">Destination buffer pointer</param>
    /// <param name="s">Source NativeStr to copy</param>
    /// <returns>A new NativeStr pointing to the destination</returns>
    let inline copyToBuffer (dest: nativeptr<byte>) (s: NativeStr) : NativeStr =
        for i = 0 to s.Length - 1 do
            NativePtr.set dest i (NativePtr.get s.Pointer i)
        NativeStr(dest, s.Length)

    /// <summary>
    /// Concatenate two NativeStr values into a destination buffer.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="dest">Destination buffer pointer (must have capacity for both strings)</param>
    /// <param name="a">First string</param>
    /// <param name="b">Second string</param>
    /// <returns>A new NativeStr pointing to the concatenated result</returns>
    let inline concat2 (dest: nativeptr<byte>) (a: NativeStr) (b: NativeStr) : NativeStr =
        // Copy first string
        for i = 0 to a.Length - 1 do
            NativePtr.set dest i (NativePtr.get a.Pointer i)
        // Copy second string after first
        let destB = NativePtr.add dest a.Length
        for i = 0 to b.Length - 1 do
            NativePtr.set destB i (NativePtr.get b.Pointer i)
        NativeStr(dest, a.Length + b.Length)

    /// <summary>
    /// Concatenate three NativeStr values into a destination buffer.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// Common pattern: prefix + content + suffix (e.g., "Hello, " + name + "!")
    /// </summary>
    /// <param name="dest">Destination buffer pointer</param>
    /// <param name="a">First string</param>
    /// <param name="b">Second string</param>
    /// <param name="c">Third string</param>
    /// <returns>A new NativeStr pointing to the concatenated result</returns>
    let inline concat3 (dest: nativeptr<byte>) (a: NativeStr) (b: NativeStr) (c: NativeStr) : NativeStr =
        let mutable pos = 0
        // Copy first string
        for i = 0 to a.Length - 1 do
            NativePtr.set dest pos (NativePtr.get a.Pointer i)
            pos <- pos + 1
        // Copy second string
        for i = 0 to b.Length - 1 do
            NativePtr.set dest pos (NativePtr.get b.Pointer i)
            pos <- pos + 1
        // Copy third string
        for i = 0 to c.Length - 1 do
            NativePtr.set dest pos (NativePtr.get c.Pointer i)
            pos <- pos + 1
        NativeStr(dest, pos)

    /// <summary>
    /// Create a NativeStr from a byte literal, copying to a destination buffer.
    /// This is useful when you need a mutable copy of a string literal.
    /// </summary>
    /// <param name="dest">Destination buffer pointer</param>
    /// <param name="bytes">Source byte literal</param>
    /// <returns>A new NativeStr pointing to the copy</returns>
    let inline fromBytesTo (dest: nativeptr<byte>) (bytes: byte[]) : NativeStr =
        let len = bytes.Length - 1  // Exclude null terminator
        for i = 0 to len - 1 do
            NativePtr.set dest i bytes.[i]
        NativeStr(dest, len)
