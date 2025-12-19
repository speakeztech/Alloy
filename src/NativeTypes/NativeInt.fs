#nowarn "9"
namespace Alloy

/// <summary>
/// Fixed-width integer types and operations for freestanding compilation.
/// These map directly to LLVM integer types without any .NET overhead.
/// Provides explicit bit-width operations for systems programming.
/// </summary>
[<AutoOpen>]
module NativeInt =

    // ═══════════════════════════════════════════════════════════════════
    // Type aliases for clarity
    // These are the exact same F# types but with explicit naming
    // ═══════════════════════════════════════════════════════════════════

    /// 8-bit signed integer (i8)
    type I8 = sbyte
    /// 8-bit unsigned integer (i8)
    type U8 = byte
    /// 16-bit signed integer (i16)
    type I16 = int16
    /// 16-bit unsigned integer (i16)
    type U16 = uint16
    /// 32-bit signed integer (i32)
    type I32 = int32
    /// 32-bit unsigned integer (i32)
    type U32 = uint32
    /// 64-bit signed integer (i64)
    type I64 = int64
    /// 64-bit unsigned integer (i64)
    type U64 = uint64
    /// Native-width signed integer (ptr-sized)
    type ISize = nativeint
    /// Native-width unsigned integer (ptr-sized)
    type USize = unativeint

    // ═══════════════════════════════════════════════════════════════════
    // Bit manipulation operations
    // ═══════════════════════════════════════════════════════════════════

    /// Counts the number of set bits (population count)
    let inline popcount32 (x: I32) : I32 =
        let mutable n = uint32 x
        n <- n - ((n >>> 1) &&& 0x55555555u)
        n <- (n &&& 0x33333333u) + ((n >>> 2) &&& 0x33333333u)
        n <- (n + (n >>> 4)) &&& 0x0F0F0F0Fu
        n <- n * 0x01010101u
        int32 (n >>> 24)

    /// Counts the number of set bits for 64-bit
    let inline popcount64 (x: I64) : I32 =
        let mutable n = uint64 x
        n <- n - ((n >>> 1) &&& 0x5555555555555555UL)
        n <- (n &&& 0x3333333333333333UL) + ((n >>> 2) &&& 0x3333333333333333UL)
        n <- (n + (n >>> 4)) &&& 0x0F0F0F0F0F0F0F0FUL
        n <- n * 0x0101010101010101UL
        int32 (n >>> 56)

    /// Counts leading zeros for 32-bit
    let inline clz32 (x: I32) : I32 =
        if x = 0 then 32
        else
            let mutable n = 0
            let mutable v = uint32 x
            if v &&& 0xFFFF0000u = 0u then n <- n + 16; v <- v <<< 16
            if v &&& 0xFF000000u = 0u then n <- n + 8; v <- v <<< 8
            if v &&& 0xF0000000u = 0u then n <- n + 4; v <- v <<< 4
            if v &&& 0xC0000000u = 0u then n <- n + 2; v <- v <<< 2
            if v &&& 0x80000000u = 0u then n <- n + 1
            n

    /// Counts leading zeros for 64-bit
    let inline clz64 (x: I64) : I32 =
        if x = 0L then 64
        else
            let mutable n = 0
            let mutable v = uint64 x
            if v &&& 0xFFFFFFFF00000000UL = 0UL then n <- n + 32; v <- v <<< 32
            if v &&& 0xFFFF000000000000UL = 0UL then n <- n + 16; v <- v <<< 16
            if v &&& 0xFF00000000000000UL = 0UL then n <- n + 8; v <- v <<< 8
            if v &&& 0xF000000000000000UL = 0UL then n <- n + 4; v <- v <<< 4
            if v &&& 0xC000000000000000UL = 0UL then n <- n + 2; v <- v <<< 2
            if v &&& 0x8000000000000000UL = 0UL then n <- n + 1
            n

    /// Counts trailing zeros for 32-bit
    let inline ctz32 (x: I32) : I32 =
        if x = 0 then 32
        else
            let mutable n = 0
            let mutable v = uint32 x
            if v &&& 0x0000FFFFu = 0u then n <- n + 16; v <- v >>> 16
            if v &&& 0x000000FFu = 0u then n <- n + 8; v <- v >>> 8
            if v &&& 0x0000000Fu = 0u then n <- n + 4; v <- v >>> 4
            if v &&& 0x00000003u = 0u then n <- n + 2; v <- v >>> 2
            if v &&& 0x00000001u = 0u then n <- n + 1
            n

    /// Counts trailing zeros for 64-bit
    let inline ctz64 (x: I64) : I32 =
        if x = 0L then 64
        else
            let mutable n = 0
            let mutable v = uint64 x
            if v &&& 0x00000000FFFFFFFFuL = 0UL then n <- n + 32; v <- v >>> 32
            if v &&& 0x000000000000FFFFuL = 0UL then n <- n + 16; v <- v >>> 16
            if v &&& 0x00000000000000FFuL = 0UL then n <- n + 8; v <- v >>> 8
            if v &&& 0x000000000000000FuL = 0UL then n <- n + 4; v <- v >>> 4
            if v &&& 0x0000000000000003uL = 0UL then n <- n + 2; v <- v >>> 2
            if v &&& 0x0000000000000001uL = 0UL then n <- n + 1
            n

    /// Rotates bits left for 32-bit
    let inline rotl32 (x: I32) (n: I32) : I32 =
        let n = n &&& 31
        int32 ((uint32 x <<< n) ||| (uint32 x >>> (32 - n)))

    /// Rotates bits right for 32-bit
    let inline rotr32 (x: I32) (n: I32) : I32 =
        let n = n &&& 31
        int32 ((uint32 x >>> n) ||| (uint32 x <<< (32 - n)))

    /// Rotates bits left for 64-bit
    let inline rotl64 (x: I64) (n: I32) : I64 =
        let n = n &&& 63
        int64 ((uint64 x <<< n) ||| (uint64 x >>> (64 - n)))

    /// Rotates bits right for 64-bit
    let inline rotr64 (x: I64) (n: I32) : I64 =
        let n = n &&& 63
        int64 ((uint64 x >>> n) ||| (uint64 x <<< (64 - n)))

    /// Byte swap for 16-bit
    let inline bswap16 (x: I16) : I16 =
        int16 (((uint16 x &&& 0xFFus) <<< 8) ||| ((uint16 x &&& 0xFF00us) >>> 8))

    /// Byte swap for 32-bit
    let inline bswap32 (x: I32) : I32 =
        let v = uint32 x
        int32 (((v &&& 0x000000FFu) <<< 24) |||
               ((v &&& 0x0000FF00u) <<< 8) |||
               ((v &&& 0x00FF0000u) >>> 8) |||
               ((v &&& 0xFF000000u) >>> 24))

    /// Byte swap for 64-bit
    let inline bswap64 (x: I64) : I64 =
        let v = uint64 x
        int64 (((v &&& 0x00000000000000FFUL) <<< 56) |||
               ((v &&& 0x000000000000FF00UL) <<< 40) |||
               ((v &&& 0x0000000000FF0000UL) <<< 24) |||
               ((v &&& 0x00000000FF000000UL) <<< 8) |||
               ((v &&& 0x000000FF00000000UL) >>> 8) |||
               ((v &&& 0x0000FF0000000000UL) >>> 24) |||
               ((v &&& 0x00FF000000000000UL) >>> 40) |||
               ((v &&& 0xFF00000000000000UL) >>> 56))

    // ═══════════════════════════════════════════════════════════════════
    // Overflow-checked operations
    // ═══════════════════════════════════════════════════════════════════

    /// Adds two 32-bit integers with overflow detection
    let inline addChecked32 (a: I32) (b: I32) : I32 voption =
        let result = int64 a + int64 b
        if result > int64 System.Int32.MaxValue || result < int64 System.Int32.MinValue then
            ValueNone
        else
            ValueSome (int32 result)

    /// Subtracts two 32-bit integers with overflow detection
    let inline subChecked32 (a: I32) (b: I32) : I32 voption =
        let result = int64 a - int64 b
        if result > int64 System.Int32.MaxValue || result < int64 System.Int32.MinValue then
            ValueNone
        else
            ValueSome (int32 result)

    /// Multiplies two 32-bit integers with overflow detection
    let inline mulChecked32 (a: I32) (b: I32) : I32 voption =
        let result = int64 a * int64 b
        if result > int64 System.Int32.MaxValue || result < int64 System.Int32.MinValue then
            ValueNone
        else
            ValueSome (int32 result)

    // ═══════════════════════════════════════════════════════════════════
    // Saturating operations (clamp on overflow instead of wrap)
    // ═══════════════════════════════════════════════════════════════════

    /// Saturating add for 32-bit signed
    let inline addSat32 (a: I32) (b: I32) : I32 =
        let result = int64 a + int64 b
        if result > int64 System.Int32.MaxValue then System.Int32.MaxValue
        elif result < int64 System.Int32.MinValue then System.Int32.MinValue
        else int32 result

    /// Saturating subtract for 32-bit signed
    let inline subSat32 (a: I32) (b: I32) : I32 =
        let result = int64 a - int64 b
        if result > int64 System.Int32.MaxValue then System.Int32.MaxValue
        elif result < int64 System.Int32.MinValue then System.Int32.MinValue
        else int32 result

    /// Saturating add for 32-bit unsigned
    let inline addSatU32 (a: U32) (b: U32) : U32 =
        let result = uint64 a + uint64 b
        if result > uint64 System.UInt32.MaxValue then System.UInt32.MaxValue
        else uint32 result

    /// Saturating subtract for 32-bit unsigned
    let inline subSatU32 (a: U32) (b: U32) : U32 =
        if b > a then 0u
        else a - b

    // ═══════════════════════════════════════════════════════════════════
    // Sign/zero extension helpers
    // ═══════════════════════════════════════════════════════════════════

    /// Sign-extends 8-bit to 32-bit
    let inline sext8to32 (x: I8) : I32 = int32 x

    /// Sign-extends 16-bit to 32-bit
    let inline sext16to32 (x: I16) : I32 = int32 x

    /// Sign-extends 32-bit to 64-bit
    let inline sext32to64 (x: I32) : I64 = int64 x

    /// Zero-extends 8-bit to 32-bit
    let inline zext8to32 (x: U8) : U32 = uint32 x

    /// Zero-extends 16-bit to 32-bit
    let inline zext16to32 (x: U16) : U32 = uint32 x

    /// Zero-extends 32-bit to 64-bit
    let inline zext32to64 (x: U32) : U64 = uint64 x

    /// Truncates 32-bit to 8-bit
    let inline trunc32to8 (x: I32) : I8 = sbyte x

    /// Truncates 32-bit to 16-bit
    let inline trunc32to16 (x: I32) : I16 = int16 x

    /// Truncates 64-bit to 32-bit
    let inline trunc64to32 (x: I64) : I32 = int32 x

    // ═══════════════════════════════════════════════════════════════════
    // Alignment helpers
    // ═══════════════════════════════════════════════════════════════════

    /// Aligns a value up to the next multiple of alignment
    let inline alignUp (value: ISize) (alignment: ISize) : ISize =
        (value + alignment - 1n) &&& ~~~(alignment - 1n)

    /// Aligns a value down to the previous multiple of alignment
    let inline alignDown (value: ISize) (alignment: ISize) : ISize =
        value &&& ~~~(alignment - 1n)

    /// Checks if a value is aligned to the given alignment
    let inline isAligned (value: ISize) (alignment: ISize) : bool =
        (value &&& (alignment - 1n)) = 0n

    // ═══════════════════════════════════════════════════════════════════
    // Power of two helpers
    // ═══════════════════════════════════════════════════════════════════

    /// Checks if a value is a power of two
    let inline isPowerOfTwo32 (x: I32) : bool =
        x > 0 && (x &&& (x - 1)) = 0

    /// Checks if a value is a power of two (64-bit)
    let inline isPowerOfTwo64 (x: I64) : bool =
        x > 0L && (x &&& (x - 1L)) = 0L

    /// Rounds up to the next power of two
    let inline nextPowerOfTwo32 (x: I32) : I32 =
        if x <= 0 then 1
        else
            let mutable v = uint32 (x - 1)
            v <- v ||| (v >>> 1)
            v <- v ||| (v >>> 2)
            v <- v ||| (v >>> 4)
            v <- v ||| (v >>> 8)
            v <- v ||| (v >>> 16)
            int32 (v + 1u)

    /// Rounds up to the next power of two (64-bit)
    let inline nextPowerOfTwo64 (x: I64) : I64 =
        if x <= 0L then 1L
        else
            let mutable v = uint64 (x - 1L)
            v <- v ||| (v >>> 1)
            v <- v ||| (v >>> 2)
            v <- v ||| (v >>> 4)
            v <- v ||| (v >>> 8)
            v <- v ||| (v >>> 16)
            v <- v ||| (v >>> 32)
            int64 (v + 1UL)

    /// Returns the log base 2 of a power of two
    let inline log2PowerOfTwo32 (x: I32) : I32 =
        31 - clz32 x
