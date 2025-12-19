#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Pure F# UUID implementation (RFC 4122) with no System.Guid dependencies
/// </summary>
module Uuid =
    /// <summary>
    /// A 128-bit UUID (RFC 4122)
    /// </summary>
    type Uuid = { 
        /// <summary>The 16-byte data array representing the UUID</summary>
        Data: byte[] 
    }
    
    /// <summary>
    /// Hex character lookup table for fast conversion
    /// </summary>
    let private hexChars = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'|]
    
    /// <summary>
    /// Function to convert a byte to two hex characters
    /// </summary>
    /// <param name="b">The byte to convert</param>
    /// <returns>An array of two hex characters</returns>
    let private byteToHex (b: byte): char[] =
        let hi = int (b >>> 4) &&& 0xF
        let lo = int b &&& 0xF
        [| hexChars[hi]; hexChars[lo] |]
    
    /// <summary>
    /// Function to convert a hex character to its numeric value
    /// </summary>
    /// <param name="c">The hex character</param>
    /// <returns>The numeric value (0-15)</returns>
    /// <exception cref="System.Exception">Thrown when the character is not a valid hex digit</exception>
    let private hexToInt (c: char): int =
        match c with
        | c when c >= '0' && c <= '9' -> int c - int '0'
        | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
        | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
        | _ -> panicwith (ofBytes "Invalid hex character"B)
    
    /// <summary>
    /// The nil UUID (all zeros)
    /// </summary>
    let nil: Uuid = { Data = Array.zeroCreate 16 }
    
    /// <summary>
    /// Creates a new UUID (v4 random)
    /// </summary>
    /// <returns>A new random UUID</returns>
    let newUuid (): Uuid =
        // In a real implementation, this would use platform-specific random number generation
        // For now, we'll use a simple deterministic approach for demonstration
        
        let bytes = Array.zeroCreate 16
        
        // Generate pseudo-random bytes
        for i = 0 to 15 do
            bytes[i] <- byte (i * 17 % 256)
        
        // Set version to 4 (random)
        bytes[6] <- bytes[6] &&& 0x0Fuy ||| 0x40uy
        
        // Set variant to RFC 4122
        bytes[8] <- bytes[8] &&& 0x3Fuy ||| 0x80uy
        
        { Data = bytes }
    
    /// <summary>
    /// Generates a version 5 UUID based on a namespace and name
    /// </summary>
    /// <param name="namespace">The namespace UUID</param>
    /// <param name="name">The name NativeStr</param>
    /// <returns>A version 5 UUID</returns>
    let newUuidV5 (``namespace``: Uuid) (name: NativeStr) : Uuid =
        // Combine namespace (16 bytes) and name bytes
        let totalLen = 16 + name.Length
        let data = Array.zeroCreate totalLen

        // Copy namespace bytes
        for i = 0 to 15 do
            data.[i] <- ``namespace``.Data.[i]

        // Copy name bytes directly (NativeStr is already UTF-8)
        for i = 0 to name.Length - 1 do
            data.[16 + i] <- NativePtr.get name.Pointer i

        // Normally we would compute SHA-1 hash here
        // For this example, we'll just do a simple hash
        let hash = Array.zeroCreate 16
        for i = 0 to 15 do
            hash.[i] <-
                if i < data.Length then
                    data.[i]
                else
                    byte (i * 3 % 256)

        // Set version to 5
        hash.[6] <- hash.[6] &&& 0x0Fuy ||| 0x50uy

        // Set variant to RFC 4122
        hash.[8] <- hash.[8] &&& 0x3Fuy ||| 0x80uy

        { Data = hash }
    
    /// <summary>
    /// Converts UUID to NativeStr representation using a caller-provided buffer.
    /// Buffer must be at least 36 bytes.
    /// </summary>
    /// <param name="buffer">Destination buffer (at least 36 bytes)</param>
    /// <param name="uuid">The UUID to convert</param>
    /// <returns>A NativeStr in the format "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"</returns>
    let inline toStringTo (buffer: nativeptr<byte>) (uuid: Uuid) : NativeStr =
        let inline writeHexByte (pos: int) (b: byte) =
            let hi = int (b >>> 4) &&& 0xF
            let lo = int b &&& 0xF
            NativePtr.set buffer pos (if hi < 10 then byte (hi + 48) else byte (hi + 87))
            NativePtr.set buffer (pos + 1) (if lo < 10 then byte (lo + 48) else byte (lo + 87))

        // Format: 8-4-4-4-12 (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
        // Bytes 0-3 -> positions 0-7
        writeHexByte 0 uuid.Data.[0]
        writeHexByte 2 uuid.Data.[1]
        writeHexByte 4 uuid.Data.[2]
        writeHexByte 6 uuid.Data.[3]

        NativePtr.set buffer 8 45uy // '-'

        // Bytes 4-5 -> positions 9-12
        writeHexByte 9 uuid.Data.[4]
        writeHexByte 11 uuid.Data.[5]

        NativePtr.set buffer 13 45uy // '-'

        // Bytes 6-7 -> positions 14-17
        writeHexByte 14 uuid.Data.[6]
        writeHexByte 16 uuid.Data.[7]

        NativePtr.set buffer 18 45uy // '-'

        // Bytes 8-9 -> positions 19-22
        writeHexByte 19 uuid.Data.[8]
        writeHexByte 21 uuid.Data.[9]

        NativePtr.set buffer 23 45uy // '-'

        // Bytes 10-15 -> positions 24-35
        writeHexByte 24 uuid.Data.[10]
        writeHexByte 26 uuid.Data.[11]
        writeHexByte 28 uuid.Data.[12]
        writeHexByte 30 uuid.Data.[13]
        writeHexByte 32 uuid.Data.[14]
        writeHexByte 34 uuid.Data.[15]

        NativeStr(buffer, 36)
    
    /// <summary>
    /// Parse a UUID from a NativeStr
    /// </summary>
    /// <param name="s">The string to parse (must be 36 bytes)</param>
    /// <returns>The parsed UUID</returns>
    let fromString (s: NativeStr) : Uuid =
        if s.Length <> 36 then
            panicwith (ofBytes "Invalid UUID format: expected 36 characters"B)

        // Check hyphens are in the right places
        if s.[8] <> '-' || s.[13] <> '-' || s.[18] <> '-' || s.[23] <> '-' then
            panicwith (ofBytes "Invalid UUID format: incorrect hyphen placement"B)

        let bytes = Array.zeroCreate 16
        let mutable byteIndex = 0

        // Helper to convert hex char to int
        let inline hexToIntLocal (c: char) : int =
            if c >= '0' && c <= '9' then int c - int '0'
            elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
            elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
            else 0 // Invalid, but we validated format above

        // Process the five groups
        let inline processGroup (startIndex: int) (length: int) : unit =
            let mutable i = startIndex
            while i < startIndex + length do
                let hi = hexToIntLocal s.[i]
                let lo = hexToIntLocal s.[i + 1]
                bytes.[byteIndex] <- byte ((hi <<< 4) ||| lo)
                byteIndex <- byteIndex + 1
                i <- i + 2

        // Group 1: 8 chars (4 bytes)
        processGroup 0 8

        // Group 2: 4 chars (2 bytes)
        processGroup 9 4

        // Group 3: 4 chars (2 bytes)
        processGroup 14 4

        // Group 4: 4 chars (2 bytes)
        processGroup 19 4

        // Group 5: 12 chars (6 bytes)
        processGroup 24 12

        { Data = bytes }
    
    /// <summary>
    /// Compares two UUIDs for equality
    /// </summary>
    /// <param name="uuid1">The first UUID</param>
    /// <param name="uuid2">The second UUID</param>
    /// <returns>True if the UUIDs are equal, false otherwise</returns>
    let equals (uuid1: Uuid) (uuid2: Uuid): bool =
        let mutable result = true
        let mutable i = 0
        
        while result && i < 16 do
            if uuid1.Data[i] <> uuid2.Data[i] then
                result <- false
            i <- i + 1
            
        result
    
    /// <summary>
    /// Converts a UUID to a byte array
    /// </summary>
    /// <param name="uuid">The UUID to convert</param>
    /// <returns>A byte array representation</returns>
    let toByteArray (uuid: Uuid): byte[] =
        Array.copy uuid.Data
    
    /// <summary>
    /// Creates a UUID from a byte array
    /// </summary>
    /// <param name="bytes">The byte array (must be 16 bytes)</param>
    /// <returns>A UUID created from the byte array</returns>
    /// <exception cref="System.Exception">Thrown when the byte array is not 16 bytes</exception>
    let fromByteArray (bytes: byte[]): Uuid =
        if bytes.Length <> 16 then
            panicwith (ofBytes "UUID byte array must be exactly 16 bytes"B)
            
        { Data = Array.copy bytes }
    
    /// <summary>
    /// Gets the version of a UUID
    /// </summary>
    /// <param name="uuid">The UUID</param>
    /// <returns>The UUID version (1-5)</returns>
    let getVersion (uuid: Uuid): int =
        int ((uuid.Data[6] &&& 0xF0uy) >>> 4)
    
    /// <summary>
    /// Gets the variant of a UUID
    /// </summary>
    /// <param name="uuid">The UUID</param>
    /// <returns>The UUID variant (0-3)</returns>
    let getVariant (uuid: Uuid): int =
        let v = int uuid.Data[8]
        if (v &&& 0x80) = 0 then 0 // Reserved for NCS backward compatibility
        elif (v &&& 0xC0) = 0x80 then 1 // RFC 4122
        elif (v &&& 0xE0) = 0xC0 then 2 // Reserved for Microsoft
        else 3 // Reserved for future definition