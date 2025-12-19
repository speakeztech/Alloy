#nowarn "9"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Memory

/// Zero-allocation text processing using native types only.
/// No BCL types (System.String, System.Char, etc.) are used.
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
        let inline countCodepoints (str: NativeStr) : int =
            let mutable count = 0
            let mutable i = 0
            while i < str.Length do
                let b = NativePtr.get str.Pointer i
                if b < 0x80uy then i <- i + 1
                elif b < 0xE0uy then i <- i + 2
                elif b < 0xF0uy then i <- i + 3
                else i <- i + 4
                count <- count + 1
            count

    // ═══════════════════════════════════════════════════════════════════
    // NativeStr Operations
    // ═══════════════════════════════════════════════════════════════════

    module NativeStr =
        /// Creates a NativeStr from a pointer and length.
        let inline create (ptr: nativeptr<byte>) (len: int) : NativeStr =
            NativeString.create ptr len

        /// Returns an empty NativeStr.
        let inline empty () : NativeStr =
            NativeString.empty()

        /// Returns true if the string is empty or null.
        let inline isEmpty (s: NativeStr) : bool =
            NativeString.isEmpty s

        /// Returns the byte length of the string.
        let inline length (s: NativeStr) : int =
            NativeString.length s

        /// Returns the number of Unicode codepoints (not bytes).
        let inline codepointCount (s: NativeStr) : int =
            UTF8.countCodepoints s

        /// Gets the byte at the specified index.
        let inline byteAt (index: int) (s: NativeStr) : byte =
            NativeString.byteAt index s

        /// Compares two NativeStr for equality.
        let inline equals (a: NativeStr) (b: NativeStr) : bool =
            if a.Length <> b.Length then false
            else
                let mutable equal = true
                let mutable i = 0
                while equal && i < a.Length do
                    if NativePtr.get a.Pointer i <> NativePtr.get b.Pointer i then
                        equal <- false
                    i <- i + 1
                equal

        /// Compares two NativeStr lexicographically.
        /// Returns: negative if a < b, zero if a = b, positive if a > b.
        let inline compare (a: NativeStr) (b: NativeStr) : int =
            let minLen = if a.Length < b.Length then a.Length else b.Length
            let mutable result = 0
            let mutable i = 0
            while result = 0 && i < minLen do
                let ba = NativePtr.get a.Pointer i
                let bb = NativePtr.get b.Pointer i
                if ba < bb then result <- -1
                elif ba > bb then result <- 1
                i <- i + 1
            if result = 0 then
                if a.Length < b.Length then -1
                elif a.Length > b.Length then 1
                else 0
            else result

        /// Checks if a string starts with a prefix.
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

        /// Checks if a string ends with a suffix.
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

        /// Creates a slice (substring) of the string.
        /// Returns a new NativeStr pointing into the same memory.
        let inline slice (start: int) (len: int) (s: NativeStr) : NativeStr =
            if start < 0 || start >= s.Length then
                empty()
            else
                let actualLen = if start + len > s.Length then s.Length - start else len
                NativeStr(NativePtr.add s.Pointer start, actualLen)

        /// Copies the string to a destination buffer.
        /// Returns the number of bytes copied.
        let inline copyTo (dest: nativeptr<byte>) (s: NativeStr) : int =
            NativeString.copyTo dest s

        /// Finds the first occurrence of a byte in the string.
        /// Returns the index, or -1 if not found.
        let inline indexOfByte (b: byte) (s: NativeStr) : int =
            let mutable result = -1
            let mutable i = 0
            while result = -1 && i < s.Length do
                if NativePtr.get s.Pointer i = b then
                    result <- i
                i <- i + 1
            result

        /// Finds the first occurrence of a substring.
        /// Returns the index, or -1 if not found.
        let inline indexOf (needle: NativeStr) (haystack: NativeStr) : int =
            if needle.Length = 0 then 0
            elif needle.Length > haystack.Length then -1
            else
                let mutable result = -1
                let mutable i = 0
                let maxStart = haystack.Length - needle.Length
                while result = -1 && i <= maxStart do
                    let mutable matches = true
                    let mutable j = 0
                    while matches && j < needle.Length do
                        if NativePtr.get haystack.Pointer (i + j) <> NativePtr.get needle.Pointer j then
                            matches <- false
                        j <- j + 1
                    if matches then result <- i
                    i <- i + 1
                result

        /// Counts occurrences of a substring.
        let inline countOccurrences (needle: NativeStr) (haystack: NativeStr) : int =
            if needle.Length = 0 || needle.Length > haystack.Length then 0
            else
                let mutable count = 0
                let mutable i = 0
                let maxStart = haystack.Length - needle.Length
                while i <= maxStart do
                    let mutable matches = true
                    let mutable j = 0
                    while matches && j < needle.Length do
                        if NativePtr.get haystack.Pointer (i + j) <> NativePtr.get needle.Pointer j then
                            matches <- false
                        j <- j + 1
                    if matches then
                        count <- count + 1
                        i <- i + needle.Length
                    else
                        i <- i + 1
                count

        /// Concatenates two strings into a destination buffer.
        /// Returns a NativeStr pointing to the concatenated result.
        let inline concat (dest: nativeptr<byte>) (maxLen: int) (a: NativeStr) (b: NativeStr) : NativeStr =
            let totalLen = a.Length + b.Length
            if totalLen > maxLen then
                empty()
            else
                // Copy first string
                for i = 0 to a.Length - 1 do
                    NativePtr.set dest i (NativePtr.get a.Pointer i)
                // Copy second string
                for i = 0 to b.Length - 1 do
                    NativePtr.set dest (a.Length + i) (NativePtr.get b.Pointer i)
                NativeStr(dest, totalLen)

        /// Replaces all occurrences of oldValue with newValue.
        /// Result is written to dest buffer.
        let inline replace (dest: nativeptr<byte>) (maxLen: int)
                          (original: NativeStr) (oldValue: NativeStr) (newValue: NativeStr) : NativeStr =
            if oldValue.Length = 0 then
                // No replacement needed, just copy
                let copyLen = if original.Length > maxLen then maxLen else original.Length
                for i = 0 to copyLen - 1 do
                    NativePtr.set dest i (NativePtr.get original.Pointer i)
                NativeStr(dest, copyLen)
            else
                let mutable srcPos = 0
                let mutable dstPos = 0

                while srcPos < original.Length && dstPos < maxLen do
                    // Check for match at current position
                    let remaining = original.Length - srcPos
                    if remaining >= oldValue.Length then
                        let mutable isMatch = true
                        let mutable j = 0
                        while isMatch && j < oldValue.Length do
                            if NativePtr.get original.Pointer (srcPos + j) <> NativePtr.get oldValue.Pointer j then
                                isMatch <- false
                            j <- j + 1

                        if isMatch then
                            // Copy replacement
                            let copyLen = if dstPos + newValue.Length > maxLen
                                          then maxLen - dstPos
                                          else newValue.Length
                            for k = 0 to copyLen - 1 do
                                NativePtr.set dest (dstPos + k) (NativePtr.get newValue.Pointer k)
                            dstPos <- dstPos + copyLen
                            srcPos <- srcPos + oldValue.Length
                        else
                            // Copy single byte
                            NativePtr.set dest dstPos (NativePtr.get original.Pointer srcPos)
                            dstPos <- dstPos + 1
                            srcPos <- srcPos + 1
                    else
                        // Not enough remaining for a match, copy byte
                        NativePtr.set dest dstPos (NativePtr.get original.Pointer srcPos)
                        dstPos <- dstPos + 1
                        srcPos <- srcPos + 1

                NativeStr(dest, dstPos)

    // ═══════════════════════════════════════════════════════════════════
    // Integer to String Conversion
    // ═══════════════════════════════════════════════════════════════════

    module Format =
        /// Converts a 32-bit signed integer to a string.
        /// Writes to the provided buffer and returns a NativeStr.
        let inline int32ToString (value: int) (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
            if maxLen < 12 then  // Max i32 is 11 chars + sign
                NativeStr.empty()
            else
                // Handle zero specially
                if value = 0 then
                    NativePtr.set buffer 0 48uy  // '0'
                    NativeStr(buffer, 1)
                else
                    let mutable pos = 0
                    let mutable v = value

                    // Handle negative
                    if v < 0 then
                        NativePtr.set buffer 0 45uy  // '-'
                        pos <- 1
                        v <- -v

                    // Count digits
                    let mutable temp = v
                    let mutable digitCount = 0
                    while temp > 0 do
                        digitCount <- digitCount + 1
                        temp <- temp / 10

                    // Write digits in reverse order
                    let startPos = pos
                    pos <- pos + digitCount
                    let mutable writePos = pos - 1
                    while v > 0 do
                        // digit is 0-9, safe to cast directly to byte
                        let digit = byte (v % 10)
                        NativePtr.set buffer writePos (digit + 48uy)  // digit + '0'
                        writePos <- writePos - 1
                        v <- v / 10

                    NativeStr(buffer, pos)

        /// Converts a 64-bit signed integer to a string.
        let inline int64ToString (value: int64) (buffer: nativeptr<byte>) (maxLen: int) : NativeStr =
            if maxLen < 21 then  // Max i64 is 20 chars + sign
                NativeStr.empty()
            else
                if value = 0L then
                    NativePtr.set buffer 0 48uy  // '0'
                    NativeStr(buffer, 1)
                else
                    let mutable pos = 0
                    let mutable v = value

                    if v < 0L then
                        NativePtr.set buffer 0 45uy  // '-'
                        pos <- 1
                        v <- -v

                    let mutable temp = v
                    let mutable digitCount = 0
                    while temp > 0L do
                        digitCount <- digitCount + 1
                        temp <- temp / 10L

                    pos <- pos + digitCount
                    let mutable writePos = pos - 1
                    while v > 0L do
                        // digit is 0-9, safe to cast directly to byte
                        let digit = byte (v % 10L)
                        NativePtr.set buffer writePos (digit + 48uy)  // digit + '0'
                        writePos <- writePos - 1
                        v <- v / 10L

                    NativeStr(buffer, pos)

        /// Converts a 32-bit unsigned integer to hexadecimal.
        let inline uint32ToHex (value: uint32) (buffer: nativeptr<byte>) (uppercase: bool) : NativeStr =
            if value = 0u then
                NativePtr.set buffer 0 48uy  // '0'
                NativeStr(buffer, 1)
            else
                let hexChars =
                    if uppercase then "0123456789ABCDEF"B
                    else "0123456789abcdef"B

                // Count hex digits
                let mutable temp = value
                let mutable digitCount = 0
                while temp > 0u do
                    digitCount <- digitCount + 1
                    temp <- temp >>> 4

                // Write digits
                let mutable v = value
                let mutable pos = digitCount - 1
                while v > 0u do
                    let nibble = int (v &&& 0xFu)
                    NativePtr.set buffer pos hexChars.[nibble]
                    pos <- pos - 1
                    v <- v >>> 4

                NativeStr(buffer, digitCount)
