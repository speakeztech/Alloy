#nowarn "9"
namespace Alloy

open Fsil
open Alloy

/// <summary>
/// Represents a contiguous region of memory.
/// </summary>
/// <typeparam name="T">The type of elements in the span.</typeparam>
[<Struct>]
type Span<'T> =
    val private _array: 'T[]
    val private _start: int
    val private _length: int
    
    /// <summary>Creates a new span over the entirety of a specified array.</summary>
    /// <param name="array">The array from which to create the span.</param>
    new (array: 'T[]) = { _array = array; _start = 0; _length = array.Length }
    
    /// <summary>Creates a new span over a portion of a specified array.</summary>
    /// <param name="array">The array from which to create the span.</param>
    /// <param name="start">The index at which to begin the span.</param>
    /// <param name="length">The number of elements to include in the span.</param>
    new (array: 'T[], start: int, length: int) = 
        if start < 0 || length < 0 || start + length > array.Length then
            panicwith (ofBytes "Invalid span parameters"B)
        { _array = array; _start = start; _length = length }
    
    /// <summary>Gets the element at the specified index.</summary>
    /// <param name="index">The index of the element to get.</param>
    /// <returns>The element at the specified index.</returns>
    member this.Item 
        with get(index: int) : 'T = 
            if index < 0 || index >= this._length then
                panicwith (ofBytes "Index out of range"B)
            this._array.[this._start + index]
        and set(index: int) (value: 'T) =
            if index < 0 || index >= this._length then
                panicwith (ofBytes "Index out of range"B)
            this._array.[this._start + index] <- value
    
    /// <summary>Gets the length of the span.</summary>
    member this.Length = this._length
    
    /// <summary>Clears the contents of this span.</summary>
    member this.Clear() =
        for i = 0 to this._length - 1 do
            this._array.[this._start + i] <- Unchecked.defaultof<'T>
    
    /// <summary>Fills the span with a specified value.</summary>
    /// <param name="value">The value to fill the span with.</param>
    member this.Fill(value: 'T) =
        for i = 0 to this._length - 1 do
            this._array.[this._start + i] <- value

/// <summary>
/// Represents a read-only contiguous region of memory.
/// </summary>
/// <typeparam name="T">The type of elements in the read-only span.</typeparam>
[<Struct>]
type ReadOnlySpan<'T> =
    val private _array: 'T[]
    val private _start: int
    val private _length: int
    
    /// <summary>Creates a new read-only span over the entirety of a specified array.</summary>
    /// <param name="array">The array from which to create the read-only span.</param>
    new (array: 'T[]) = { _array = array; _start = 0; _length = array.Length }
    
    /// <summary>Creates a new read-only span over a portion of a specified array.</summary>
    /// <param name="array">The array from which to create the read-only span.</param>
    /// <param name="start">The index at which to begin the read-only span.</param>
    /// <param name="length">The number of elements to include in the read-only span.</param>
    new (array: 'T[], start: int, length: int) = 
        if start < 0 || length < 0 || start + length > array.Length then
            panicwith (ofBytes "Invalid span parameters"B)
        { _array = array; _start = start; _length = length }
    
    /// <summary>Gets the element at the specified index.</summary>
    /// <param name="index">The index of the element to get.</param>
    /// <returns>The element at the specified index.</returns>
    member this.Item 
        with get(index: int) : 'T = 
            if index < 0 || index >= this._length then
                panicwith (ofBytes "Index out of range"B)
            this._array.[this._start + index]
    
    /// <summary>Gets the length of the read-only span.</summary>
    member this.Length = this._length
    
    /// <summary>Copies the contents of this read-only span into a destination span.</summary>
    /// <param name="destination">The span into which to copy the contents of this read-only span.</param>
    member this.CopyTo(destination: Span<'T>) =
        let mutable dest = destination // Create a mutable local copy
        let copyLength = min this._length dest.Length
        for i = 0 to copyLength - 1 do
            dest.[i] <- this.[i]

/// <summary>
/// Represents a contiguous region of memory. All functions are exposed through the 
/// AutoOpen attribute, making them accessible when opening the Alloy namespace.
/// </summary>
[<AutoOpen>]
module Span =
    /// <summary>Creates a Span from an array.</summary>
    /// <param name="array">The source array.</param>
    /// <typeparam name="T">The type of elements in the array.</typeparam>
    /// <returns>A Span that represents the entire array.</returns>
    let inline asSpan (array: 'T[]) : Span<'T> = 
        Span<'T>(array)

    /// <summary>Creates a Span from a subrange of an array.</summary>
    /// <param name="array">The source array.</param>
    /// <param name="start">The zero-based index of the first element in the Span.</param>
    /// <param name="length">The number of elements in the Span.</param>
    /// <typeparam name="T">The type of elements in the array.</typeparam>
    /// <returns>A Span that represents a portion of the array.</returns>
    let inline sliceSpan (array: 'T[]) (start: int) (length: int) : Span<'T> =
        Span<'T>(array, start, length)

    /// <summary>Creates a ReadOnlySpan from an array.</summary>
    /// <param name="array">The source array.</param>
    /// <typeparam name="T">The type of elements in the array.</typeparam>
    /// <returns>A ReadOnlySpan that represents the entire array.</returns>
    let inline asReadOnlySpan (array: 'T[]) : ReadOnlySpan<'T> = 
        ReadOnlySpan<'T>(array)

    /// <summary>Creates a ReadOnlySpan from a subrange of an array.</summary>
    /// <param name="array">The source array.</param>
    /// <param name="start">The zero-based index of the first element in the ReadOnlySpan.</param>
    /// <param name="length">The number of elements in the ReadOnlySpan.</param>
    /// <typeparam name="T">The type of elements in the array.</typeparam>
    /// <returns>A ReadOnlySpan that represents a portion of the array.</returns>
    let inline sliceReadOnlySpan (array: 'T[]) (start: int) (length: int) : ReadOnlySpan<'T> =
        ReadOnlySpan<'T>(array, start, length)

    /// <summary>Gets the value at the specified index in a Span.</summary>
    /// <param name="span">The span to read from.</param>
    /// <param name="index">The index from which to get the value.</param>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    /// <returns>The value at the specified index.</returns>
    let inline get (span: Span<'T>) (index: int) : 'T =
        span.[index]

    /// <summary>Gets the value at the specified index in a ReadOnlySpan.</summary>
    /// <param name="span">The read-only span to read from.</param>
    /// <param name="index">The index from which to get the value.</param>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    /// <returns>The value at the specified index.</returns>
    let inline getReadOnly (span: ReadOnlySpan<'T>) (index: int) : 'T =
        span.[index]

    /// <summary>Sets the value at the specified index in a Span.</summary>
    /// <param name="span">The span to modify.</param>
    /// <param name="index">The index at which to set the value.</param>
    /// <param name="value">The value to set.</param>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    let inline set (span: Span<'T>) (index: int) (value: 'T) : unit =
        let mutable s = span
        s.[index] <- value

    /// <summary>Maps a function over elements in a ReadOnlySpan and writes results to a destination Span.</summary>
    /// <param name="mapper">The mapping function to apply to each element.</param>
    /// <param name="source">The source ReadOnlySpan.</param>
    /// <param name="destination">The destination Span where results will be written.</param>
    /// <typeparam name="T">The type of elements in the source span.</typeparam>
    /// <typeparam name="U">The type of elements in the destination span.</typeparam>
    let inline mapSpan (mapper: 'T -> 'U) (source: ReadOnlySpan<'T>) (destination: Span<'U>) : unit =
        let minLength a b = if a < b then a else b
        let length = minLength source.Length destination.Length
        let mutable dest = destination
        for i = 0 to length - 1 do
            dest.[i] <- mapper source.[i]

    /// <summary>Fills all elements of a Span with the specified value.</summary>
    /// <param name="value">The value to fill the span with.</param>
    /// <param name="span">The span to fill.</param>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    let inline fillSpan (value: 'T) (span: Span<'T>) : unit =
        let mutable s = span
        s.Fill(value)

    /// <summary>Clears all elements in a Span (sets them to default value).</summary>
    /// <param name="span">The span to clear.</param>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    let inline clearSpan (span: Span<'T>) : unit =
        let mutable s = span
        s.Clear()

    /// <summary>Copies elements from source Span to destination Span.</summary>
    /// <param name="source">The source ReadOnlySpan.</param>
    /// <param name="destination">The destination Span.</param>
    /// <typeparam name="T">The type of elements in the spans.</typeparam>
    let inline copySpan (source: ReadOnlySpan<'T>) (destination: Span<'T>) : unit =
        let mutable dest = destination
        source.CopyTo(dest)

    /// <summary>Filters elements in a ReadOnlySpan based on a predicate and writes results to a destination Span.</summary>
    /// <param name="predicate">The predicate function used to test elements.</param>
    /// <param name="source">The source ReadOnlySpan.</param>
    /// <param name="destination">The destination Span where matched elements will be written.</param>
    /// <typeparam name="T">The type of elements in the spans.</typeparam>
    /// <returns>The number of elements written to the destination span.</returns>
    let inline filterSpan (predicate: 'T -> bool) (source: ReadOnlySpan<'T>) (destination: Span<'T>) : int =
        let mutable count = 0
        let minLength a b = if a < b then a else b
        let length = minLength source.Length destination.Length
        let mutable dest = destination
        
        for i = 0 to source.Length - 1 do
            if predicate source.[i] && count < length then
                dest.[count] <- source.[i]
                count <- count + 1
                
        count

    /// <summary>Applies a function that combines an element with the accumulator over a ReadOnlySpan.</summary>
    /// <param name="folder">The function to update the state given the current state and element.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="span">The span to fold over.</param>
    /// <typeparam name="State">The type of the state.</typeparam>
    /// <typeparam name="T">The type of elements in the span.</typeparam>
    /// <returns>The final accumulated state.</returns>
    let inline foldSpan (folder: 'State -> 'T -> 'State) (state: 'State) (span: ReadOnlySpan<'T>) : 'State =
        let mutable result = state
        for i = 0 to span.Length - 1 do
            result <- folder result span.[i]
        result

    /// <summary>Sums all elements in a Span.</summary>
    /// <param name="span">The span containing elements to sum.</param>
    /// <typeparam name="T">The type of elements in the span, must support zero and addition.</typeparam>
    /// <returns>The sum of all elements in the span.</returns>
    let inline sumSpan<'T when 'T: (static member Zero: 'T) 
                        and (^T or Numerics.BasicOps or Numerics.MeasureOps): (static member Add: ^T * ^T -> ^T)> 
        (span: ReadOnlySpan<'T>) : 'T =
        
        if span.Length = 0 then 
            zero<'T>
        else
            let mutable result = span.[0]
            for i = 1 to span.Length - 1 do
                result <- add result span.[i]
            result

    /// <summary>Computes the average of elements in a Span.</summary>
    /// <param name="span">The span containing elements to average.</param>
    /// <typeparam name="T">The type of elements in the span, must support zero, addition, and division.</typeparam>
    /// <returns>The average of all elements in the span.</returns>
    let inline averageSpan<'T when 'T: (static member Zero: 'T) 
                        and (^T or Numerics.BasicOps or Numerics.MeasureOps): (static member Add: ^T * ^T -> ^T)
                        and (^T or Numerics.BasicOps or Numerics.MeasureOps or Numerics.MeasureMeasureOps): (static member Divide: ^T * ^T -> ^T)> 
        (span: ReadOnlySpan<'T>) : 'T =
        
        if span.Length = 0 then 
            zero<'T>
        else
            let mutable sum = span.[0]
            for i = 1 to span.Length - 1 do
                sum <- add sum span.[i]
            
            // Handle each type separately without pattern matching
            let result =
                // Integer division
                if (box sum) :? int then
                    let mutable intSum = unbox<int> (box sum)
                    let mutable length = span.Length
                    box (intSum / length)
                // Float division
                elif (box sum) :? float then
                    let mutable floatSum = unbox<float> (box sum)
                    let mutable length = float span.Length
                    box (floatSum / length)
                // Int64 division
                elif (box sum) :? int64 then
                    let mutable longSum = unbox<int64> (box sum)
                    let mutable length = int64 span.Length
                    box (longSum / length)
                // Float32 division
                elif (box sum) :? float32 then
                    let mutable float32Sum = unbox<float32> (box sum)
                    let mutable length = float32 span.Length
                    box (float32Sum / length)
                // Default case
                else
                    let mutable length = span.Length
                    box (divide sum (box length :?> 'T))
                    
            result :?> 'T