#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Native span types for freestanding compilation.
/// Provides both read-only and mutable views over contiguous memory regions.
/// These are fat pointers (ptr + length) emitted as MLIR structs.
/// </summary>
[<AutoOpen>]
module NativeSpan =

    /// <summary>
    /// A read-only span over a contiguous memory region.
    /// Emitted as: !llvm.struct<(ptr, i64)>
    /// </summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type ReadOnlySpan<'T when 'T : unmanaged> =
        val Pointer: nativeptr<'T>
        val Length: int

        new (ptr: nativeptr<'T>, len: int) =
            { Pointer = ptr; Length = len }

    /// <summary>
    /// A mutable span over a contiguous memory region.
    /// Emitted as: !llvm.struct<(ptr, i64)>
    /// </summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type Span<'T when 'T : unmanaged> =
        val Pointer: nativeptr<'T>
        val Length: int

        new (ptr: nativeptr<'T>, len: int) =
            { Pointer = ptr; Length = len }

    // ═══════════════════════════════════════════════════════════════════
    // ReadOnlySpan operations
    // ═══════════════════════════════════════════════════════════════════

    module ReadOnlySpan =
        /// Creates a ReadOnlySpan from a pointer and length.
        let inline create (ptr: nativeptr<'T>) (len: int) : ReadOnlySpan<'T> =
            ReadOnlySpan(ptr, len)

        /// Creates an empty ReadOnlySpan.
        let inline empty<'T when 'T : unmanaged> : ReadOnlySpan<'T> =
            ReadOnlySpan(NativePtr.nullPtr<'T>, 0)

        /// Returns true if the span is empty.
        let inline isEmpty (span: ReadOnlySpan<'T>) : bool =
            span.Length = 0

        /// Returns the length of the span.
        let inline length (span: ReadOnlySpan<'T>) : int =
            span.Length

        /// Gets the element at the specified index.
        let inline get (index: int) (span: ReadOnlySpan<'T>) : 'T =
            if index < 0 || index >= span.Length then
                Unchecked.defaultof<'T>
            else
                NativePtr.get span.Pointer index

        /// Gets the first element.
        let inline head (span: ReadOnlySpan<'T>) : 'T =
            get 0 span

        /// Gets the last element.
        let inline last (span: ReadOnlySpan<'T>) : 'T =
            get (span.Length - 1) span

        /// Creates a slice of the span.
        let inline slice (start: int) (len: int) (span: ReadOnlySpan<'T>) : ReadOnlySpan<'T> =
            if start < 0 || start >= span.Length then
                empty<'T>
            else
                let actualLen = min len (span.Length - start)
                ReadOnlySpan(NativePtr.add span.Pointer start, actualLen)

        /// Creates a slice from start to end.
        let inline sliceFrom (start: int) (span: ReadOnlySpan<'T>) : ReadOnlySpan<'T> =
            slice start (span.Length - start) span

        /// Creates a slice from beginning with given length.
        let inline take (count: int) (span: ReadOnlySpan<'T>) : ReadOnlySpan<'T> =
            slice 0 count span

        /// Skips the first count elements.
        let inline skip (count: int) (span: ReadOnlySpan<'T>) : ReadOnlySpan<'T> =
            sliceFrom count span

        /// Iterates over each element.
        let inline iter (f: 'T -> unit) (span: ReadOnlySpan<'T>) : unit =
            for i = 0 to span.Length - 1 do
                f (NativePtr.get span.Pointer i)

        /// Iterates over each element with index.
        let inline iteri (f: int -> 'T -> unit) (span: ReadOnlySpan<'T>) : unit =
            for i = 0 to span.Length - 1 do
                f i (NativePtr.get span.Pointer i)

        /// Folds over the span.
        let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (span: ReadOnlySpan<'T>) : 'State =
            let mutable acc = state
            for i = 0 to span.Length - 1 do
                acc <- folder acc (NativePtr.get span.Pointer i)
            acc

        /// Returns true if any element satisfies the predicate.
        let inline exists (predicate: 'T -> bool) (span: ReadOnlySpan<'T>) : bool =
            let mutable found = false
            let mutable i = 0
            while not found && i < span.Length do
                if predicate (NativePtr.get span.Pointer i) then
                    found <- true
                i <- i + 1
            found

        /// Returns true if all elements satisfy the predicate.
        let inline forall (predicate: 'T -> bool) (span: ReadOnlySpan<'T>) : bool =
            let mutable allMatch = true
            let mutable i = 0
            while allMatch && i < span.Length do
                if not (predicate (NativePtr.get span.Pointer i)) then
                    allMatch <- false
                i <- i + 1
            allMatch

        /// Copies to a mutable span.
        let inline copyTo (dest: Span<'T>) (src: ReadOnlySpan<'T>) : int =
            let count = min src.Length dest.Length
            for i = 0 to count - 1 do
                NativePtr.set dest.Pointer i (NativePtr.get src.Pointer i)
            count

    // ═══════════════════════════════════════════════════════════════════
    // Span (mutable) operations
    // ═══════════════════════════════════════════════════════════════════

    module Span =
        /// Creates a Span from a pointer and length.
        let inline create (ptr: nativeptr<'T>) (len: int) : Span<'T> =
            Span(ptr, len)

        /// Creates an empty Span.
        let inline empty<'T when 'T : unmanaged> : Span<'T> =
            Span(NativePtr.nullPtr<'T>, 0)

        /// Returns true if the span is empty.
        let inline isEmpty (span: Span<'T>) : bool =
            span.Length = 0

        /// Returns the length of the span.
        let inline length (span: Span<'T>) : int =
            span.Length

        /// Gets the element at the specified index.
        let inline get (index: int) (span: Span<'T>) : 'T =
            if index < 0 || index >= span.Length then
                Unchecked.defaultof<'T>
            else
                NativePtr.get span.Pointer index

        /// Sets the element at the specified index.
        let inline set (index: int) (value: 'T) (span: Span<'T>) : unit =
            if index >= 0 && index < span.Length then
                NativePtr.set span.Pointer index value

        /// Gets the first element.
        let inline head (span: Span<'T>) : 'T =
            get 0 span

        /// Gets the last element.
        let inline last (span: Span<'T>) : 'T =
            get (span.Length - 1) span

        /// Creates a slice of the span.
        let inline slice (start: int) (len: int) (span: Span<'T>) : Span<'T> =
            if start < 0 || start >= span.Length then
                empty<'T>
            else
                let actualLen = min len (span.Length - start)
                Span(NativePtr.add span.Pointer start, actualLen)

        /// Creates a slice from start to end.
        let inline sliceFrom (start: int) (span: Span<'T>) : Span<'T> =
            slice start (span.Length - start) span

        /// Creates a slice from beginning with given length.
        let inline take (count: int) (span: Span<'T>) : Span<'T> =
            slice 0 count span

        /// Skips the first count elements.
        let inline skip (count: int) (span: Span<'T>) : Span<'T> =
            sliceFrom count span

        /// Fills the span with a value.
        let inline fill (value: 'T) (span: Span<'T>) : unit =
            for i = 0 to span.Length - 1 do
                NativePtr.set span.Pointer i value

        /// Clears the span (fills with default value).
        let inline clear (span: Span<'T>) : unit =
            fill Unchecked.defaultof<'T> span

        /// Reverses elements in place.
        let inline reverse (span: Span<'T>) : unit =
            let mutable i = 0
            let mutable j = span.Length - 1
            while i < j do
                let temp = NativePtr.get span.Pointer i
                NativePtr.set span.Pointer i (NativePtr.get span.Pointer j)
                NativePtr.set span.Pointer j temp
                i <- i + 1
                j <- j - 1

        /// Converts to a read-only span.
        let inline asReadOnly (span: Span<'T>) : ReadOnlySpan<'T> =
            ReadOnlySpan(span.Pointer, span.Length)

        /// Iterates over each element.
        let inline iter (f: 'T -> unit) (span: Span<'T>) : unit =
            for i = 0 to span.Length - 1 do
                f (NativePtr.get span.Pointer i)

        /// Iterates over each element with index.
        let inline iteri (f: int -> 'T -> unit) (span: Span<'T>) : unit =
            for i = 0 to span.Length - 1 do
                f i (NativePtr.get span.Pointer i)

        /// Maps a function over each element in place.
        let inline mapInPlace (f: 'T -> 'T) (span: Span<'T>) : unit =
            for i = 0 to span.Length - 1 do
                NativePtr.set span.Pointer i (f (NativePtr.get span.Pointer i))

        /// Folds over the span.
        let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (span: Span<'T>) : 'State =
            let mutable acc = state
            for i = 0 to span.Length - 1 do
                acc <- folder acc (NativePtr.get span.Pointer i)
            acc

        /// Copies from another span.
        let inline copyFrom (src: Span<'T>) (dest: Span<'T>) : int =
            let count = min src.Length dest.Length
            for i = 0 to count - 1 do
                NativePtr.set dest.Pointer i (NativePtr.get src.Pointer i)
            count

        /// Copies from a read-only span.
        let inline copyFromReadOnly (src: ReadOnlySpan<'T>) (dest: Span<'T>) : int =
            let count = min src.Length dest.Length
            for i = 0 to count - 1 do
                NativePtr.set dest.Pointer i (NativePtr.get src.Pointer i)
            count
