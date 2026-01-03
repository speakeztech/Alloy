#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Memory operations and types for efficient, dependency-free memory management
/// </summary>
module Memory =
    
    /// <summary>
    /// Internal helper functions for native pointer operations.
    /// These are defined first to avoid circular references.
    /// </summary>
    [<AutoOpen>]
    module private NativeOperations =
        /// <summary>Gets a value from a pointer at a specified index.</summary>
        let inline getFromPtr (ptr: nativeptr<'T>) (index: int) : 'T =
            NativePtr.get ptr index
            
        /// <summary>Sets a value at a pointer's specified index.</summary>
        let inline setAtPtr (ptr: nativeptr<'T>) (index: int) (value: 'T) : unit =
            NativePtr.set ptr index value
            
        /// <summary>Adds an offset to a pointer.</summary>
        let inline addToPtr (ptr: nativeptr<'T>) (offset: int) : nativeptr<'T> =
            NativePtr.add ptr offset
            
        /// <summary>Allocates memory on the stack.</summary>
        let inline allocOnStack<'T when 'T : unmanaged> (count: int) : nativeptr<'T> =
            NativePtr.stackalloc<'T> count
    
    /// <summary>
    /// Represents a contiguous region of memory.
    /// </summary>
    /// <typeparam name="'T">The type of elements in the span.</typeparam>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type Span<'T when 'T : unmanaged> =
        val private _array: ValueOption<'T[]>
        val private _pointer: ValueOption<nativeptr<'T>>
        val private _start: int
        val private _length: int
        
        /// <summary>Creates a new span over the entirety of a specified array.</summary>
        /// <param name="array">The array from which to create the span.</param>
        new (array: 'T[]) = 
            { _array = ValueSome array
              _pointer = ValueNone
              _start = 0
              _length = array.Length }
        
        /// <summary>Creates a new span over a portion of a specified array.</summary>
        /// <param name="array">The array from which to create the span.</param>
        /// <param name="start">The index at which to begin the span.</param>
        /// <param name="length">The number of elements to include in the span.</param>
        new (array: 'T[], start: int, length: int) = 
            if start < 0 || length < 0 || start + length > array.Length then
                panicwith (ofBytes "Invalid span parameters"B)
            { _array = ValueSome array
              _pointer = ValueNone
              _start = start
              _length = length }
        
        /// <summary>Creates a new span from a native pointer.</summary>
        /// <param name="pointer">The native pointer to the memory.</param>
        /// <param name="length">The number of elements accessible from the pointer.</param>
        new (pointer: nativeptr<'T>, length: int) = 
            if length < 0 then
                panicwith (ofBytes "Invalid span length"B)
            { _array = ValueNone
              _pointer = ValueSome pointer
              _start = 0
              _length = length }
      
        /// <summary>Gets the element at the specified index.</summary>
        /// <returns>The element at the specified index.</returns>
        member this.Item 
            with get(index: int) : 'T = 
                if index < 0 || index >= this._length then
                    panicwith (ofBytes "Index out of range"B)
                match this._array, this._pointer with
                | ValueSome arr, ValueNone -> arr[this._start + index]
                | ValueNone, ValueSome ptr -> getFromPtr ptr index
                | _ -> panicwith (ofBytes "Invalid span state"B)
                
            and set(index: int) (value: 'T) =
                if index < 0 || index >= this._length then
                    panicwith (ofBytes "Index out of range"B)
                match this._array, this._pointer with
                | ValueSome arr, ValueNone -> arr[this._start + index] <- value
                | ValueNone, ValueSome ptr -> setAtPtr ptr index value
                | _ -> panicwith (ofBytes "Invalid span state"B)
        
        /// <summary>Gets the length of the span.</summary>
        member this.Length = this._length
        
        /// <summary>Clears the contents of this span.</summary>
        member this.Clear() =
            match this._array, this._pointer with
            | ValueSome arr, ValueNone ->
                for i = 0 to this._length - 1 do
                    arr[this._start + i] <- NativeDefault.zeroed<'T>()
            | ValueNone, ValueSome ptr ->
                for i = 0 to this._length - 1 do
                    setAtPtr ptr i (NativeDefault.zeroed<'T>())
            | _ -> panicwith (ofBytes "Invalid span state"B)
        
        /// <summary>Fills the span with a specified value.</summary>
        /// <param name="value">The value to fill the span with.</param>
        member this.Fill(value: 'T) =
            match this._array, this._pointer with
            | ValueSome arr, ValueNone ->
                for i = 0 to this._length - 1 do
                    arr[this._start + i] <- value
            | ValueNone, ValueSome ptr ->
                for i = 0 to this._length - 1 do
                    setAtPtr ptr i value
            | _ -> panicwith (ofBytes "Invalid span state"B)
                
        /// <summary>Copies the contents of this span into another span.</summary>
        /// <param name="destination">The span to copy elements into.</param>
        member this.CopyTo(destination: byref<Span<'T>>) =
            let copyLength = min this._length destination.Length
            for i = 0 to copyLength - 1 do
                destination[i] <- this[i]
                
        /// <summary>Creates a new span from a slice of this span.</summary>
        /// <param name="start">The index at which to begin the slice.</param>
        /// <param name="length">The number of elements to include in the slice.</param>
        /// <returns>A new span representing a slice of this span.</returns>
        member this.Slice(start: int, length: int) =
            if start < 0 || length < 0 || start + length > this._length then
                panicwith (ofBytes "Invalid slice parameters"B)
                
            match this._array, this._pointer with
            | ValueSome arr, ValueNone ->
                Span(arr, this._start + start, length)
            | ValueNone, ValueSome ptr ->
                let offsetPtr = addToPtr ptr start
                Span(offsetPtr, length)
            | _ -> panicwith (ofBytes "Invalid span state"B)
    
    /// <summary>
    /// Represents a read-only contiguous region of memory.
    /// </summary>
    /// <typeparam name="'T">The type of elements in the span.</typeparam>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type ReadOnlySpan<'T when 'T : unmanaged> =
        val private _array: ValueOption<'T[]>
        val private _pointer: ValueOption<nativeptr<'T>>
        val private _start: int
        val private _length: int
        
        /// <summary>Creates a new read-only span over the entirety of a specified array.</summary>
        /// <param name="array">The array from which to create the span.</param>
        new (array: 'T[]) = 
            { _array = ValueSome array
              _pointer = ValueNone
              _start = 0
              _length = array.Length }
        
        /// <summary>Creates a new read-only span over a portion of a specified array.</summary>
        /// <param name="array">The array from which to create the span.</param>
        /// <param name="start">The index at which to begin the span.</param>
        /// <param name="length">The number of elements to include in the span.</param>
        new (array: 'T[], start: int, length: int) = 
            if start < 0 || length < 0 || start + length > array.Length then
                panicwith (ofBytes "Invalid span parameters"B)
            { _array = ValueSome array
              _pointer = ValueNone
              _start = start
              _length = length }
        
        /// <summary>Creates a new read-only span from a native pointer.</summary>
        /// <param name="pointer">The native pointer to the memory.</param>
        /// <param name="length">The number of elements accessible from the pointer.</param>
        new (pointer: nativeptr<'T>, length: int) = 
            if length < 0 then
                panicwith (ofBytes "Invalid span length"B)
            { _array = ValueNone
              _pointer = ValueSome pointer
              _start = 0
              _length = length }
      
        /// <summary>Gets the element at the specified index.</summary>
        /// <returns>The element at the specified index.</returns>
        member this.Item 
            with get(index: int) : 'T = 
                if index < 0 || index >= this._length then
                    panicwith (ofBytes "Index out of range"B)
                match this._array, this._pointer with
                | ValueSome arr, ValueNone -> arr[this._start + index]
                | ValueNone, ValueSome ptr -> getFromPtr ptr index
                | _ -> panicwith (ofBytes "Invalid span state"B)
        
        /// <summary>Gets the length of the span.</summary>
        member this.Length = this._length
        
        /// <summary>Copies the contents of this read-only span into a writable span.</summary>
        /// <param name="destination">The span to copy elements into.</param>
        member this.CopyTo(destination: byref<Span<'T>>) =
            let copyLength = min this._length destination.Length
            for i = 0 to copyLength - 1 do
                destination[i] <- this[i]
                
        /// <summary>Creates a new read-only span from a slice of this span.</summary>
        /// <param name="start">The index at which to begin the slice.</param>
        /// <param name="length">The number of elements to include in the slice.</param>
        /// <returns>A new read-only span representing a slice of this span.</returns>
        member this.Slice(start: int, length: int) =
            if start < 0 || length < 0 || start + length > this._length then
                panicwith (ofBytes "Invalid slice parameters"B)
                
            match this._array, this._pointer with
            | ValueSome arr, ValueNone ->
                ReadOnlySpan(arr, this._start + start, length)
            | ValueNone, ValueSome ptr ->
                let offsetPtr = addToPtr ptr start
                ReadOnlySpan(offsetPtr, length)
            | _ -> panicwith (ofBytes "Invalid span state"B)
    
    /// <summary>
    /// A fixed-size buffer allocated on the stack.
    /// </summary>
    /// <typeparam name="'T">The type of elements in the buffer.</typeparam>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type StackBuffer<'T when 'T : unmanaged> =
        val private ptr: nativeptr<'T>
        val private size: int
        
        /// <summary>Creates a new stack-allocated buffer with the specified size.</summary>
        /// <param name="size">The number of elements in the buffer.</param>
        new (size: int) =
            { ptr = allocOnStack<'T> size
              size = size }
        
        /// <summary>Gets the pointer to the buffer memory.</summary>
        member this.Pointer = this.ptr
        
        /// <summary>Gets the length of the buffer in elements.</summary>
        member this.Length = this.size
        
        /// <summary>Creates a span over the entire buffer.</summary>
        /// <returns>A new span over the entire buffer.</returns>
        member this.AsSpan() =
            Span(this.ptr, this.size)
            
        /// <summary>Creates a span over a portion of the buffer.</summary>
        /// <param name="start">The index at which to begin the span.</param>
        /// <param name="length">The number of elements to include in the span.</param>
        /// <returns>A new span over a portion of the buffer.</returns>
        member this.AsSpan(start: int, length: int) =
            if start < 0 || length < 0 || start + length > this.size then
                panicwith (ofBytes "Invalid span parameters"B)
            let offsetPtr = addToPtr this.ptr start
            Span(offsetPtr, length)
            
        /// <summary>Disposes of the buffer. For StackBuffer, this is a no-op as stack memory is cleaned up automatically.</summary>
        member this.Dispose() = ()

        interface System.IDisposable with
            member this.Dispose() = ()

        /// <summary>Creates a read-only span over the entire buffer.</summary>
        /// <returns>A new read-only span over the entire buffer.</returns>
        member this.AsReadOnlySpan() =
            ReadOnlySpan(this.ptr, this.size)
            
        /// <summary>Creates a read-only span over a portion of the buffer.</summary>
        /// <param name="start">The index at which to begin the span.</param>
        /// <param name="length">The number of elements to include in the span.</param>
        /// <returns>A new read-only span over a portion of the buffer.</returns>
        member this.AsReadOnlySpan(start: int, length: int) =
            if start < 0 || length < 0 || start + length > this.size then
                panicwith (ofBytes "Invalid span parameters"B)
            let offsetPtr = addToPtr this.ptr start
            ReadOnlySpan(offsetPtr, length)
    
    /// <summary>
    /// Functions for creating and working with stack-allocated buffers
    /// </summary>
    let inline stackBuffer<'T when 'T : unmanaged> size : StackBuffer<'T> =
        new StackBuffer<'T>(size)
    
    /// <summary>
    /// Static functions for working with spans
    /// </summary>
    module Span =
        /// <summary>Creates a span from an array.</summary>
        let inline asSpan (array: 'T[]) = 
            Span<'T>(array)
            
        /// <summary>Creates a span from a portion of an array.</summary>
        let inline sliceSpan (array: 'T[]) start length = 
            Span<'T>(array, start, length)
            
        /// <summary>Creates a span from a native pointer.</summary>
        let inline fromPointer (pointer: nativeptr<'T>) length = 
            Span<'T>(pointer, length)
            
        /// <summary>Gets an element from a span at the specified index.</summary>
        let inline get (span: Span<'T>) index = 
            span[index]
            
        /// <summary>Sets an element in a span at the specified index.</summary>
        let inline set (span: byref<Span<'T>>) index value = 
            span[index] <- value
            
        /// <summary>Maps values from one span to another using a mapping function.</summary>
        let inline mapSpan (mapper: 'T -> 'U) (source: Span<'T>) (destination: byref<Span<'U>>) =
            let minLength = fun a b -> if a < b then a else b
            let length = minLength source.Length destination.Length
            for i = 0 to length - 1 do
                destination[i] <- mapper (source[i])
            
    /// <summary>
    /// Module containing functions for working with ReadOnlySpan
    /// </summary>
    module ReadOnlySpan =
        /// <summary>Creates a read-only span from an array.</summary>
        let inline asReadOnlySpan (array: 'T[]) = 
            ReadOnlySpan<'T>(array)
            
        /// <summary>Creates a read-only span from a portion of an array.</summary>
        let inline sliceReadOnlySpan (array: 'T[]) start length = 
            ReadOnlySpan<'T>(array, start, length)
            
        /// <summary>Creates a read-only span from a native pointer.</summary>
        let inline fromPointerReadOnly (pointer: nativeptr<'T>) length = 
            ReadOnlySpan<'T>(pointer, length)
            
        /// <summary>Gets an element from a read-only span at the specified index.</summary>
        let inline get (span: ReadOnlySpan<'T>) index = 
            span[index]
            
    /// <summary>
    /// Extension methods for working with native buffers through static resolution
    /// </summary>
    module NativeBuffer =
        /// <summary>Gets a pointer from a buffer type that has a Pointer property</summary>
        let inline getPointer< ^T, ^U when ^T : (member Pointer : nativeptr< ^U >)> (buffer: ^T) : nativeptr< ^U > =
            ((^T) : (member Pointer : nativeptr< ^U >) buffer)
            
        /// <summary>Gets the length from a buffer type that has a Length property</summary>
        let inline getLength< ^T when ^T : (member Length : int)> (buffer: ^T) : int =
            ((^T) : (member Length : int) buffer)
            
        /// <summary>Creates a span from a buffer that has an AsSpan method</summary>
        let inline asSpan< ^T, ^U when ^T : (member AsSpan : unit -> Span< ^U >)> (buffer: ^T) : Span< ^U > =
            ((^T) : (member AsSpan : unit -> Span< ^U >) buffer)
            
        /// <summary>Creates a span from a section of a buffer that has an AsSpan method</summary>
        let inline asSpanSlice< ^T, ^U when ^T : (member AsSpan : int * int -> Span< ^U >)>
            (buffer: ^T) start length : Span< ^U > =
            ((^T) : (member AsSpan : int * int -> Span< ^U >) (buffer, start, length))

    // ═══════════════════════════════════════════════════════════════════════
    // Semantic Memory Primitives
    // ═══════════════════════════════════════════════════════════════════════
    // These primitives express INTENT rather than implementation.
    // The Firefly compiler (via Alex) will lower these to target-optimal code:
    // - ARM: Tight stack-based loops
    // - x86_64: SIMD vectorized operations (SSE/AVX)
    // - GPU: Parallel lane execution
    // - Large sizes: memcpy/memset intrinsics
    //
    // At the F# level, these are implemented as simple loops for .NET compat.
    // The Firefly compiler recognizes these patterns and emits optimal code.
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Copy bytes from source to destination.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="src">Source pointer</param>
    /// <param name="dest">Destination pointer</param>
    /// <param name="length">Number of bytes to copy</param>
    let inline copy (src: nativeptr<byte>) (dest: nativeptr<byte>) (length: int) : unit =
        for i = 0 to length - 1 do
            NativePtr.set dest i (NativePtr.get src i)

    /// <summary>
    /// Copy elements from source to destination with type safety.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="src">Source pointer</param>
    /// <param name="dest">Destination pointer</param>
    /// <param name="count">Number of elements to copy</param>
    let inline copyElements<'T when 'T : unmanaged> (src: nativeptr<'T>) (dest: nativeptr<'T>) (count: int) : unit =
        for i = 0 to count - 1 do
            NativePtr.set dest i (NativePtr.get src i)

    /// <summary>
    /// Zero a memory region.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="dest">Destination pointer</param>
    /// <param name="length">Number of bytes to zero</param>
    let inline zero (dest: nativeptr<byte>) (length: int) : unit =
        for i = 0 to length - 1 do
            NativePtr.set dest i 0uy

    /// <summary>
    /// Zero elements in a memory region with type safety.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="dest">Destination pointer</param>
    /// <param name="count">Number of elements to zero</param>
    let inline zeroElements<'T when 'T : unmanaged> (dest: nativeptr<'T>) (count: int) : unit =
        for i = 0 to count - 1 do
            NativePtr.set dest i (NativeDefault.zeroed<'T>())

    /// <summary>
    /// Fill a memory region with a byte value.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="dest">Destination pointer</param>
    /// <param name="value">Byte value to fill with</param>
    /// <param name="length">Number of bytes to fill</param>
    let inline fill (dest: nativeptr<byte>) (value: byte) (length: int) : unit =
        for i = 0 to length - 1 do
            NativePtr.set dest i value

    /// <summary>
    /// Compare two memory regions for equality.
    /// This is a semantic primitive - Firefly will emit target-optimal code.
    /// </summary>
    /// <param name="a">First memory region</param>
    /// <param name="b">Second memory region</param>
    /// <param name="length">Number of bytes to compare</param>
    /// <returns>True if regions are identical</returns>
    let inline compare (a: nativeptr<byte>) (b: nativeptr<byte>) (length: int) : bool =
        let mutable equal = true
        let mutable i = 0
        while equal && i < length do
            equal <- NativePtr.get a i = NativePtr.get b i
            i <- i + 1
        equal