#nowarn "9"

namespace Alloy

open FSharp.NativeInterop
open Alloy

/// <summary>
/// Pure F# binary conversion utilities built on Alloy's zero-cost abstractions
/// </summary>
[<RequireQualifiedAccess>]
module Binary =
    /// <summary>
    /// Convert from float32 to Int32 bits
    /// </summary>
    /// <param name="value">The float32 value to convert</param>
    /// <returns>The bit pattern as an int32</returns>
    let singleToInt32Bits (value: float32) : int32 =
        // Create mutable value to take address
        let mutable v = value
        // Use FSharp.NativeInterop to reinterpret the bits
        let valuePtr = &&v
        NativePtr.read (NativePtr.ofNativeInt<int32> (NativePtr.toNativeInt valuePtr))
    
    /// <summary>
    /// Convert from Int32 bits to float32
    /// </summary>
    /// <param name="value">The int32 bit pattern</param>
    /// <returns>The corresponding float32 value</returns>
    let int32BitsToSingle (value: int32) : float32 =
        let mutable v = value
        let valuePtr = &&v
        NativePtr.read (NativePtr.ofNativeInt<float32> (NativePtr.toNativeInt valuePtr))
    
    /// <summary>
    /// Convert from float to Int64 bits
    /// </summary>
    /// <param name="value">The float value to convert</param>
    /// <returns>The bit pattern as an int64</returns>
    let doubleToInt64Bits (value: float) : int64 =
        let mutable v = value
        let valuePtr = &&v
        NativePtr.read (NativePtr.ofNativeInt<int64> (NativePtr.toNativeInt valuePtr))
    
    /// <summary>
    /// Convert from Int64 bits to float
    /// </summary>
    /// <param name="value">The int64 bit pattern</param>
    /// <returns>The corresponding float value</returns>
    let int64BitsToDouble (value: int64) : float =
        let mutable v = value
        let valuePtr = &&v
        NativePtr.read (NativePtr.ofNativeInt<float> (NativePtr.toNativeInt valuePtr))
    
    /// <summary>
    /// Get bytes from an Int16
    /// </summary>
    /// <param name="value">The int16 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getInt16Bytes (value: int16) : byte[] =
        [|
            byte (value &&& 0xFFs)
            byte (value >>> 8)
        |]
    
    /// <summary>
    /// Get bytes from a UInt16
    /// </summary>
    /// <param name="value">The uint16 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getUInt16Bytes (value: uint16) : byte[] =
        [|
            byte (value &&& 0xFFus)
            byte (value >>> 8)
        |]
    
    /// <summary>
    /// Get bytes from an Int32
    /// </summary>
    /// <param name="value">The int32 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getInt32Bytes (value: int32) : byte[] =
        [|
            byte (value &&& 0xFF)
            byte ((value >>> 8) &&& 0xFF)
            byte ((value >>> 16) &&& 0xFF)
            byte ((value >>> 24) &&& 0xFF)
        |]
    
    /// <summary>
    /// Get bytes from a UInt32
    /// </summary>
    /// <param name="value">The uint32 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getUInt32Bytes (value: uint32) : byte[] =
        [|
            byte (value &&& 0xFFu)
            byte ((value >>> 8) &&& 0xFFu)
            byte ((value >>> 16) &&& 0xFFu)
            byte ((value >>> 24) &&& 0xFFu)
        |]
    
    /// <summary>
    /// Get bytes from an Int64
    /// </summary>
    /// <param name="value">The int64 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getInt64Bytes (value: int64) : byte[] =
        [|
            byte (value &&& 0xFFL)
            byte ((value >>> 8) &&& 0xFFL)
            byte ((value >>> 16) &&& 0xFFL)
            byte ((value >>> 24) &&& 0xFFL)
            byte ((value >>> 32) &&& 0xFFL)
            byte ((value >>> 40) &&& 0xFFL)
            byte ((value >>> 48) &&& 0xFFL)
            byte ((value >>> 56) &&& 0xFFL)
        |]
    
    /// <summary>
    /// Get bytes from a UInt64
    /// </summary>
    /// <param name="value">The uint64 value to convert</param>
    /// <returns>A byte array containing the value in little-endian format</returns>
    let getUInt64Bytes (value: uint64) : byte[] =
        [|
            byte (value &&& 0xFFUL)
            byte ((value >>> 8) &&& 0xFFUL)
            byte ((value >>> 16) &&& 0xFFUL)
            byte ((value >>> 24) &&& 0xFFUL)
            byte ((value >>> 32) &&& 0xFFUL)
            byte ((value >>> 40) &&& 0xFFUL)
            byte ((value >>> 48) &&& 0xFFUL)
            byte ((value >>> 56) &&& 0xFFUL)
        |]
    
    /// <summary>
    /// Convert bytes to an Int16
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The int16 value</returns>
    let toInt16 (bytes: byte[]) (startIndex: int) : int16 =
        if startIndex + 1 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = int16 bytes[startIndex]
        let b1 = int16 bytes[startIndex + 1]
        
        b0 ||| (b1 <<< 8)
    
    /// <summary>
    /// Convert bytes to a UInt16
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The uint16 value</returns>
    let toUInt16 (bytes: byte[]) (startIndex: int) : uint16 =
        if startIndex + 1 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = uint16 bytes[startIndex]
        let b1 = uint16 bytes[startIndex + 1]
        
        b0 ||| (b1 <<< 8)
    
    /// <summary>
    /// Convert bytes to an Int32
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The int32 value</returns>
    let toInt32 (bytes: byte[]) (startIndex: int) : int32 =
        if startIndex + 3 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = int32 bytes[startIndex]
        let b1 = int32 bytes[startIndex + 1]
        let b2 = int32 bytes[startIndex + 2]
        let b3 = int32 bytes[startIndex + 3]
        
        b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)
    
    /// <summary>
    /// Convert bytes to a UInt32
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The uint32 value</returns>
    let toUInt32 (bytes: byte[]) (startIndex: int) : uint32 =
        if startIndex + 3 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = uint32 bytes[startIndex]
        let b1 = uint32 bytes[startIndex + 1]
        let b2 = uint32 bytes[startIndex + 2]
        let b3 = uint32 bytes[startIndex + 3]
        
        b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)
    
    /// <summary>
    /// Convert bytes to an Int64
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The int64 value</returns>
    let toInt64 (bytes: byte[]) (startIndex: int) : int64 =
        if startIndex + 7 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = int64 bytes[startIndex]
        let b1 = int64 bytes[startIndex + 1]
        let b2 = int64 bytes[startIndex + 2]
        let b3 = int64 bytes[startIndex + 3]
        let b4 = int64 bytes[startIndex + 4]
        let b5 = int64 bytes[startIndex + 5]
        let b6 = int64 bytes[startIndex + 6]
        let b7 = int64 bytes[startIndex + 7]
        
        b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24) |||
        (b4 <<< 32) ||| (b5 <<< 40) ||| (b6 <<< 48) ||| (b7 <<< 56)
    
    /// <summary>
    /// Convert bytes to a UInt64
    /// </summary>
    /// <param name="bytes">The byte array</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The uint64 value</returns>
    let toUInt64 (bytes: byte[]) (startIndex: int) : uint64 =
        if startIndex + 7 >= bytes.Length then panicwith (ofBytes "Array too small"B)
        let b0 = uint64 bytes[startIndex]
        let b1 = uint64 bytes[startIndex + 1]
        let b2 = uint64 bytes[startIndex + 2]
        let b3 = uint64 bytes[startIndex + 3]
        let b4 = uint64 bytes[startIndex + 4]
        let b5 = uint64 bytes[startIndex + 5]
        let b6 = uint64 bytes[startIndex + 6]
        let b7 = uint64 bytes[startIndex + 7]
        
        b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24) |||
        (b4 <<< 32) ||| (b5 <<< 40) ||| (b6 <<< 48) ||| (b7 <<< 56)
        
    /// <summary>
    /// Write an Int16 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeInt16 (value: int16) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 1 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFFs)
        buffer[startIndex + 1] <- byte (value >>> 8)
    
    /// <summary>
    /// Write a UInt16 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeUInt16 (value: uint16) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 1 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFFus)
        buffer[startIndex + 1] <- byte (value >>> 8)
    
    /// <summary>
    /// Write an Int32 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeInt32 (value: int32) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 3 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFF)
        buffer[startIndex + 1] <- byte ((value >>> 8) &&& 0xFF)
        buffer[startIndex + 2] <- byte ((value >>> 16) &&& 0xFF)
        buffer[startIndex + 3] <- byte ((value >>> 24) &&& 0xFF)
    
    /// <summary>
    /// Write a UInt32 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeUInt32 (value: uint32) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 3 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFFu)
        buffer[startIndex + 1] <- byte ((value >>> 8) &&& 0xFFu)
        buffer[startIndex + 2] <- byte ((value >>> 16) &&& 0xFFu)
        buffer[startIndex + 3] <- byte ((value >>> 24) &&& 0xFFu)
    
    /// <summary>
    /// Write an Int64 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeInt64 (value: int64) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 7 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFFL)
        buffer[startIndex + 1] <- byte ((value >>> 8) &&& 0xFFL)
        buffer[startIndex + 2] <- byte ((value >>> 16) &&& 0xFFL)
        buffer[startIndex + 3] <- byte ((value >>> 24) &&& 0xFFL)
        buffer[startIndex + 4] <- byte ((value >>> 32) &&& 0xFFL)
        buffer[startIndex + 5] <- byte ((value >>> 40) &&& 0xFFL)
        buffer[startIndex + 6] <- byte ((value >>> 48) &&& 0xFFL)
        buffer[startIndex + 7] <- byte ((value >>> 56) &&& 0xFFL)
    
    /// <summary>
    /// Write a UInt64 value to a buffer
    /// </summary>
    /// <param name="value">The value to write</param>
    /// <param name="buffer">The destination buffer</param>
    /// <param name="startIndex">The starting index</param>
    let writeUInt64 (value: uint64) (buffer: byte[]) (startIndex: int) : unit =
        if startIndex + 7 >= buffer.Length then panicwith (ofBytes "Buffer too small"B)
        buffer[startIndex] <- byte (value &&& 0xFFUL)
        buffer[startIndex + 1] <- byte ((value >>> 8) &&& 0xFFUL)
        buffer[startIndex + 2] <- byte ((value >>> 16) &&& 0xFFUL)
        buffer[startIndex + 3] <- byte ((value >>> 24) &&& 0xFFUL)
        buffer[startIndex + 4] <- byte ((value >>> 32) &&& 0xFFUL)
        buffer[startIndex + 5] <- byte ((value >>> 40) &&& 0xFFUL)
        buffer[startIndex + 6] <- byte ((value >>> 48) &&& 0xFFUL)
        buffer[startIndex + 7] <- byte ((value >>> 56) &&& 0xFFUL)