#nowarn "86" 
#nowarn "64" 
#nowarn "77" 
#nowarn "9" 
#nowarn "3535"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Mathematics operations for the Alloy framework.
/// Provides zero-allocation numeric operations through statically resolved type parameters.
/// </summary>
module Math =
    
    /// <summary>
    /// Internal helpers for bit manipulation
    /// </summary>
    [<AutoOpen>]
    module private BitOperations =
        /// <summary>
        /// Reinterpret the bits of a value as another type using pointer casting
        /// </summary>
        let inline reinterpretBits<'TFrom, 'TTo when 'TFrom : unmanaged and 'TTo : unmanaged> (value: 'TFrom) : 'TTo =
            let mutable v = value
            let ptr = &&v |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<'TTo>
            NativePtr.read ptr
    
    /// <summary>
    /// Numeric capabilities for static resolution
    /// </summary>
    module NumericCapabilities =
        /// <summary>
        /// Types that provide a zero value
        /// </summary>
        type IZero<'T> =
            static abstract member Zero : unit -> 'T
            
        /// <summary>
        /// Types that provide a unit/one value
        /// </summary>
        type IOne<'T> =
            static abstract member One : unit -> 'T
            
        /// <summary>
        /// Int32 implementation of IZero and IOne
        /// </summary>
        type Int32Numeric =
            static member Zero() = 0
            static member One() = 1
            interface IZero<int> with
                static member Zero() = Int32Numeric.Zero()
            interface IOne<int> with
                static member One() = Int32Numeric.One()
                
        /// <summary>
        /// Int64 implementation of IZero and IOne
        /// </summary>
        type Int64Numeric =
            static member Zero() = 0L
            static member One() = 1L
            interface IZero<int64> with
                static member Zero() = Int64Numeric.Zero()
            interface IOne<int64> with
                static member One() = Int64Numeric.One()
                
        /// <summary>
        /// Float implementation of IZero and IOne
        /// </summary>
        type FloatNumeric =
            static member Zero() = 0.0
            static member One() = 1.0
            interface IZero<float> with
                static member Zero() = FloatNumeric.Zero()
            interface IOne<float> with
                static member One() = FloatNumeric.One()
                
        /// <summary>
        /// Float32 implementation of IZero and IOne
        /// </summary>
        type Float32Numeric =
            static member Zero() = 0.0f
            static member One() = 1.0f
            interface IZero<float32> with
                static member Zero() = Float32Numeric.Zero()
            interface IOne<float32> with
                static member One() = Float32Numeric.One()
    
    /// <summary>
    /// Pure native math implementations with zero dependencies.
    /// All functions work directly with binary representations and bit manipulation.
    /// </summary>
    [<RequireQualifiedAccess>]
    module NativeMath =
        
        /// <summary>
        /// IEEE 754 double precision constants
        /// </summary>
        module DoubleConstants =
            let [<Literal>] SIGN_MASK = 0x8000000000000000UL
            let [<Literal>] EXPONENT_MASK = 0x7FF0000000000000UL
            let [<Literal>] MANTISSA_MASK = 0x000FFFFFFFFFFFFFUL
            let [<Literal>] EXPONENT_BIAS = 1023
            let [<Literal>] MANTISSA_BITS = 52
            let [<Literal>] EXPONENT_SHIFT = 52
        
        /// <summary>
        /// IEEE 754 single precision constants  
        /// </summary>
        module FloatConstants =
            let [<Literal>] SIGN_MASK = 0x80000000u
            let [<Literal>] EXPONENT_MASK = 0x7F800000u
            let [<Literal>] MANTISSA_MASK = 0x007FFFFFu
            let [<Literal>] EXPONENT_BIAS = 127
            let [<Literal>] MANTISSA_BITS = 23
            let [<Literal>] EXPONENT_SHIFT = 23
        
        /// <summary>
        /// Convert double to its bit representation
        /// </summary>
        let inline doubleToBits (value: float) : uint64 =
            reinterpretBits<float, uint64> value
        
        /// <summary>
        /// Convert bit representation to double
        /// </summary>
        let inline bitsToDouble (bits: uint64) : float =
            reinterpretBits<uint64, float> bits
        
        /// <summary>
        /// Convert float to its bit representation
        /// </summary>
        let inline floatToBits (value: float32) : uint32 =
            reinterpretBits<float32, uint32> value
        
        /// <summary>
        /// Convert bit representation to float
        /// </summary>
        let inline bitsToFloat (bits: uint32) : float32 =
            reinterpretBits<uint32, float32> bits
        
        /// <summary>
        /// Fast ceiling function using bit manipulation
        /// </summary>
        let ceiling (value: float) : float =
            if value <> value then value // NaN
            else
                let bits = doubleToBits value
                let exponent = int ((bits &&& DoubleConstants.EXPONENT_MASK) >>> DoubleConstants.EXPONENT_SHIFT) - DoubleConstants.EXPONENT_BIAS
                
                if exponent >= DoubleConstants.MANTISSA_BITS then
                    value
                elif exponent < 0 then
                    if bits &&& DoubleConstants.SIGN_MASK = 0UL then 1.0 else 0.0
                else
                    let mantissaShift = DoubleConstants.MANTISSA_BITS - exponent
                    let integerMask = ~~~((1UL <<< mantissaShift) - 1UL)
                    let integerPart = bits &&& integerMask
                    
                    if integerPart = bits then
                        value
                    else
                        let result = bitsToDouble integerPart
                        if bits &&& DoubleConstants.SIGN_MASK = 0UL then
                            result + 1.0
                        else
                            result
        
        /// <summary>
        /// Fast floor function using bit manipulation
        /// </summary>
        let floor (value: float) : float =
            if value <> value then value // NaN
            else
                let bits = doubleToBits value
                let exponent = int ((bits &&& DoubleConstants.EXPONENT_MASK) >>> DoubleConstants.EXPONENT_SHIFT) - DoubleConstants.EXPONENT_BIAS
                
                if exponent >= DoubleConstants.MANTISSA_BITS then
                    value
                elif exponent < 0 then
                    if bits &&& DoubleConstants.SIGN_MASK = 0UL then 0.0 else -1.0
                else
                    let mantissaShift = DoubleConstants.MANTISSA_BITS - exponent
                    let integerMask = ~~~((1UL <<< mantissaShift) - 1UL)
                    let integerPart = bits &&& integerMask
                    
                    if integerPart = bits then
                        value
                    else
                        let result = bitsToDouble integerPart
                        if bits &&& DoubleConstants.SIGN_MASK = 0UL then
                            result
                        else
                            result - 1.0
        
        /// <summary>
        /// Round to nearest integer
        /// </summary>
        let round (value: float) : float =
            let f = floor value
            let diff = value - f
            
            if diff < 0.5 then
                f
            elif diff > 0.5 then
                f + 1.0
            else
                if (int f) % 2 = 0 then f else f + 1.0
        
        /// <summary>
        /// Single precision ceiling
        /// </summary>
        let ceilingf (value: float32) : float32 =
            if value <> value then value // NaN
            else
                let bits = floatToBits value
                let exponent = int ((bits &&& FloatConstants.EXPONENT_MASK) >>> FloatConstants.EXPONENT_SHIFT) - FloatConstants.EXPONENT_BIAS
                
                if exponent >= FloatConstants.MANTISSA_BITS then
                    value
                elif exponent < 0 then
                    if bits &&& FloatConstants.SIGN_MASK = 0u then 1.0f else 0.0f
                else
                    let mantissaShift = FloatConstants.MANTISSA_BITS - exponent
                    let integerMask = ~~~((1u <<< mantissaShift) - 1u)
                    let integerPart = bits &&& integerMask
                    
                    if integerPart = bits then
                        value
                    else
                        let result = bitsToFloat integerPart
                        if bits &&& FloatConstants.SIGN_MASK = 0u then
                            result + 1.0f
                        else
                            result
        
        /// <summary>
        /// Single precision floor
        /// </summary>
        let floorf (value: float32) : float32 =
            if value <> value then value // NaN
            else
                let bits = floatToBits value
                let exponent = int ((bits &&& FloatConstants.EXPONENT_MASK) >>> FloatConstants.EXPONENT_SHIFT) - FloatConstants.EXPONENT_BIAS
                
                if exponent >= FloatConstants.MANTISSA_BITS then
                    value
                elif exponent < 0 then
                    if bits &&& FloatConstants.SIGN_MASK = 0u then 0.0f else -1.0f
                else
                    let mantissaShift = FloatConstants.MANTISSA_BITS - exponent
                    let integerMask = ~~~((1u <<< mantissaShift) - 1u)
                    let integerPart = bits &&& integerMask
                    
                    if integerPart = bits then
                        value
                    else
                        let result = bitsToFloat integerPart
                        if bits &&& FloatConstants.SIGN_MASK = 0u then
                            result
                        else
                            result - 1.0f
        
        /// <summary>
        /// Single precision round
        /// </summary>
        let roundf (value: float32) : float32 =
            let f = floorf value
            let diff = value - f
            
            if diff < 0.5f then
                f
            elif diff > 0.5f then
                f + 1.0f
            else
                if (int f) % 2 = 0 then f else f + 1.0f
    
    /// <summary>
    /// SRTP-based generic numeric operations
    /// </summary>
    module Generic =
        open NumericCapabilities
        
        /// <summary>Gets the zero value for a type.</summary>
        let inline zero< ^T when ^T :> IZero< ^T >> : ^T =
            (^T : (static member Zero : unit -> ^T) ())
            
        /// <summary>Gets the one value for a type.</summary>
        let inline one< ^T when ^T :> IOne< ^T >> : ^T =
            (^T : (static member One : unit -> ^T) ())
    
    /// <summary>
    /// Core mathematical functions leveraging F# built-ins where appropriate
    /// </summary>
    module Functions =
        
        // Basic arithmetic - use F# built-ins (already inline and zero-allocation)
        let inline add (x: ^T) (y: ^T) : ^T = x + y
        let inline subtract (x: ^T) (y: ^T) : ^T = x - y
        let inline multiply (x: ^T) (y: ^T) : ^T = x * y
        let inline divide (x: ^T) (y: ^T) : ^T = x / y
        let inline modulo (x: ^T) (y: ^T) : ^T = x % y
        
        // Special numeric operations
        let inline power (x: ^T) (y: int) : ^T = 
            // Custom power implementation without pown dependency
            let mutable result = x
            let mutable i = 1
            while i < y do
                result <- result * x
                i <- i + 1
            result
        
        // Math functions with direct implementations
        let inline abs (x: ^T) : ^T = 
            if x < LanguagePrimitives.GenericZero< ^T > then
                -x
            else
                x
                
        let inline sign (x: ^T) : int = 
            if x < LanguagePrimitives.GenericZero< ^T > then -1
            elif x > LanguagePrimitives.GenericZero< ^T > then 1
            else 0
        
        // Specialized functions for different numeric types
        
        // Float implementation
        let inline sqrtFloat (x: float) : float =
            // Direct translation of IEEE-754 algorithm for square root
            if x < 0.0 then nan
            elif x = 0.0 then 0.0
            else
                // Start with an approximation
                let mutable guess = x / 2.0
                let mutable lastGuess = 0.0
                
                // Use Newton's method to refine
                while abs (guess - lastGuess) > 1e-10 do
                    lastGuess <- guess
                    guess <- 0.5 * (guess + x / guess)
                
                guess
                
        // Float32 implementation
        let inline sqrtFloat32 (x: float32) : float32 =
            // Direct translation of IEEE-754 algorithm for square root
            if x < 0.0f then nanf
            elif x = 0.0f then 0.0f
            else
                // Start with an approximation
                let mutable guess = x / 2.0f
                let mutable lastGuess = 0.0f
                
                // Use Newton's method to refine
                while abs (guess - lastGuess) > 1e-5f do
                    lastGuess <- guess
                    guess <- 0.5f * (guess + x / guess)
                
                guess
        
        // Type-dispatched sqrt function
        let inline sqrt (x: ^T) : ^T =
            match box x with
            | :? float as f -> sqrtFloat f |> unbox< ^T >
            | :? float32 as f -> sqrtFloat32 f |> unbox< ^T >
            | _ -> failwith "sqrt not implemented for this type"
        
        // Rounding functions - use custom NativeMath implementations
        let inline ceiling (x: float) = NativeMath.ceiling x
        let inline floor (x: float) = NativeMath.floor x
        let inline round (x: float) = NativeMath.round x
        let inline ceilingf (x: float32) = NativeMath.ceilingf x
        let inline floorf (x: float32) = NativeMath.floorf x
        let inline roundf (x: float32) = NativeMath.roundf x
        
        // Trigonometric approximations with Taylor series
        let inline sinFloat (x: float) : float =
            // Reduce to [-π, π]
            let pi = 3.14159265358979323846
            let pi2 = 2.0 * pi
            let mutable x = x % pi2
            if x > pi then
                x <- x - pi2
            elif x < -pi then
                x <- x + pi2
                
            // Taylor approximation for sin(x)
            let mutable term = x
            let mutable result = term
            let mutable i = 1
            
            while abs term > 1e-10 do
                term <- -term * x * x / float((2 * i) * (2 * i + 1))
                result <- result + term
                i <- i + 1
                
            result
            
        let inline cosFloat (x: float) : float =
            // Reduce to [-π, π]
            let pi = 3.14159265358979323846
            let pi2 = 2.0 * pi
            let mutable x = x % pi2
            if x > pi then
                x <- x - pi2
            elif x < -pi then
                x <- x + pi2
                
            // Taylor approximation for cos(x)
            let mutable term = 1.0
            let mutable result = term
            let mutable i = 1
            
            while abs term > 1e-10 do
                term <- -term * x * x / float((2 * i - 1) * (2 * i))
                result <- result + term
                i <- i + 1
                
            result
        
        // Type-dispatched trig functions
        let inline sin (x: ^T) : ^T =
            match box x with
            | :? float as f -> sinFloat f |> unbox< ^T >
            | :? float32 as f -> float32 (sinFloat (float f)) |> unbox< ^T >
            | _ -> failwith "sin not implemented for this type"
            
        let inline cos (x: ^T) : ^T =
            match box x with
            | :? float as f -> cosFloat f |> unbox< ^T >
            | :? float32 as f -> float32 (cosFloat (float f)) |> unbox< ^T >
            | _ -> failwith "cos not implemented for this type"
            
        let inline tan (x: ^T) : ^T =
            let s = sin x
            let c = cos x
            s / c
        
        // Min/Max - use direct comparisons
        let inline min (x: ^T) (y: ^T) : ^T = if x < y then x else y
        let inline max (x: ^T) (y: ^T) : ^T = if x > y then x else y
        
        // Comparison functions - use F# built-ins
        let inline equals (x: ^T) (y: ^T) = x = y
        let inline lessThan (x: ^T) (y: ^T) = x < y
        let inline greaterThan (x: ^T) (y: ^T) = x > y
        let inline lessThanOrEqual (x: ^T) (y: ^T) = x <= y
        let inline greaterThanOrEqual (x: ^T) (y: ^T) = x >= y
        
        // Array operations with explicit implementations
        let inline sum (xs: 'T[]) : 'T = 
            if xs.Length = 0 then
                match box Unchecked.defaultof<'T> with
                | :? int -> unbox<'T> 0
                | :? int64 -> unbox<'T> 0L
                | :? float -> unbox<'T> 0.0
                | :? float32 -> unbox<'T> 0.0f
                | _ -> Unchecked.defaultof<'T>
            else
                let mutable acc = xs[0]
                for i = 1 to xs.Length - 1 do
                    acc <- acc + xs[i]
                acc
        
        let inline average (xs: 'T[]) : 'T = 
            if xs.Length = 0 then 
                match box Unchecked.defaultof<'T> with
                | :? int -> unbox<'T> 0
                | :? int64 -> unbox<'T> 0L
                | :? float -> unbox<'T> 0.0
                | :? float32 -> unbox<'T> 0.0f
                | _ -> Unchecked.defaultof<'T>
            else
                let mutable acc = xs[0]
                for i = 1 to xs.Length - 1 do
                    acc <- acc + xs[i]
                
                match box acc with
                | :? int as i -> unbox<'T> (i / xs.Length)
                | :? int64 as i -> unbox<'T> (i / int64 xs.Length)
                | :? float as f -> unbox<'T> (f / float xs.Length)
                | :? float32 as f -> unbox<'T> (f / float32 xs.Length)
                | _ -> acc  // Default case
        
        let inline product (xs: 'T[]) : 'T = 
            if xs.Length = 0 then
                match box Unchecked.defaultof<'T> with
                | :? int -> unbox<'T> 1
                | :? int64 -> unbox<'T> 1L
                | :? float -> unbox<'T> 1.0
                | :? float32 -> unbox<'T> 1.0f
                | _ -> Unchecked.defaultof<'T>
            else
                let mutable acc = xs[0]
                for i = 1 to xs.Length - 1 do
                    acc <- acc * xs[i]
                acc
    
    /// <summary>
    /// Constants for various mathematical operations
    /// </summary>
    module Constants =
        // Define commonly used constants with high precision
        let Pi = 3.14159265358979323846
        let E = 2.71828182845904523536
        let Sqrt2 = 1.41421356237309504880
        let Sqrt3 = 1.73205080756887729352
        let Ln2 = 0.693147180559945309417
        let Ln10 = 2.30258509299404568402
    
    /// <summary>
    /// Operators defined as functions for explicit import
    /// </summary>
    module Operators =
        let inline (+) x y = x + y
        let inline (-) x y = x - y
        let inline (*) x y = x * y
        let inline (/) x y = x / y
        let inline (%) x y = x % y
        let inline (<<<) x y = x <<< y
        let inline (>>>) x y = x >>> y
        let inline (~~~) x = ~~~x
        let inline (&&&) x y = x &&& y
        let inline (|||) x y = x ||| y
        let inline (^^^) x y = x ^^^ y