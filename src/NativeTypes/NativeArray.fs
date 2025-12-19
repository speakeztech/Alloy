#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Native array type for freestanding compilation.
/// This is a fat pointer (pointer + length) that Firefly emits as an MLIR struct.
/// Unlike .NET arrays, this has no object header, GC tracking, or bounds checking overhead.
/// </summary>
[<AutoOpen>]
module NativeArray =

    /// <summary>
    /// A native array is a pointer to elements plus a length.
    /// This struct is emitted by Firefly as: !llvm.struct<(ptr, i64)>
    /// </summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type NativeArray<'T when 'T : unmanaged> =
        val Pointer: nativeptr<'T>
        val Length: int

        new (ptr: nativeptr<'T>, len: int) =
            { Pointer = ptr; Length = len }

    /// <summary>
    /// Creates a NativeArray from a pointer and length.
    /// </summary>
    let inline create<'T when 'T : unmanaged> (ptr: nativeptr<'T>) (len: int) : NativeArray<'T> =
        NativeArray(ptr, len)

    /// <summary>
    /// Creates an empty NativeArray.
    /// </summary>
    let inline empty<'T when 'T : unmanaged> : NativeArray<'T> =
        NativeArray(NativePtr.nullPtr<'T>, 0)

    /// <summary>
    /// Returns true if the array is empty.
    /// </summary>
    let inline isEmpty<'T when 'T : unmanaged> (arr: NativeArray<'T>) : bool =
        arr.Length = 0

    /// <summary>
    /// Returns the length of the array.
    /// </summary>
    let inline length<'T when 'T : unmanaged> (arr: NativeArray<'T>) : int =
        arr.Length

    /// <summary>
    /// Gets the element at the specified index.
    /// Returns default value if index is out of bounds.
    /// </summary>
    let inline get<'T when 'T : unmanaged> (index: int) (arr: NativeArray<'T>) : 'T =
        if index < 0 || index >= arr.Length then
            Unchecked.defaultof<'T>
        else
            NativePtr.get arr.Pointer index

    /// <summary>
    /// Sets the element at the specified index.
    /// No-op if index is out of bounds.
    /// </summary>
    let inline set<'T when 'T : unmanaged> (index: int) (value: 'T) (arr: NativeArray<'T>) : unit =
        if index >= 0 && index < arr.Length then
            NativePtr.set arr.Pointer index value

    /// <summary>
    /// Gets a pointer to the element at the specified index.
    /// </summary>
    let inline addressOf<'T when 'T : unmanaged> (index: int) (arr: NativeArray<'T>) : nativeptr<'T> =
        NativePtr.add arr.Pointer index

    /// <summary>
    /// Creates a slice of the array from start index with given length.
    /// </summary>
    let inline slice<'T when 'T : unmanaged> (start: int) (len: int) (arr: NativeArray<'T>) : NativeArray<'T> =
        if start < 0 || start >= arr.Length then
            empty<'T>
        else
            let actualLen = min len (arr.Length - start)
            NativeArray(NativePtr.add arr.Pointer start, actualLen)

    /// <summary>
    /// Copies elements from source to destination array.
    /// Returns the number of elements copied.
    /// </summary>
    let inline copyTo<'T when 'T : unmanaged> (dest: NativeArray<'T>) (src: NativeArray<'T>) : int =
        let count = min src.Length dest.Length
        for i = 0 to count - 1 do
            NativePtr.set dest.Pointer i (NativePtr.get src.Pointer i)
        count

    /// <summary>
    /// Fills the array with the specified value.
    /// </summary>
    let inline fill<'T when 'T : unmanaged> (value: 'T) (arr: NativeArray<'T>) : unit =
        for i = 0 to arr.Length - 1 do
            NativePtr.set arr.Pointer i value

    /// <summary>
    /// Iterates over each element in the array.
    /// </summary>
    let inline iter<'T when 'T : unmanaged> (f: 'T -> unit) (arr: NativeArray<'T>) : unit =
        for i = 0 to arr.Length - 1 do
            f (NativePtr.get arr.Pointer i)

    /// <summary>
    /// Iterates over each element with its index.
    /// </summary>
    let inline iteri<'T when 'T : unmanaged> (f: int -> 'T -> unit) (arr: NativeArray<'T>) : unit =
        for i = 0 to arr.Length - 1 do
            f i (NativePtr.get arr.Pointer i)

    /// <summary>
    /// Folds over the array from left to right.
    /// </summary>
    let inline fold<'T, 'State when 'T : unmanaged> (folder: 'State -> 'T -> 'State) (state: 'State) (arr: NativeArray<'T>) : 'State =
        let mutable acc = state
        for i = 0 to arr.Length - 1 do
            acc <- folder acc (NativePtr.get arr.Pointer i)
        acc

    /// <summary>
    /// Returns true if any element satisfies the predicate.
    /// </summary>
    let inline exists<'T when 'T : unmanaged> (predicate: 'T -> bool) (arr: NativeArray<'T>) : bool =
        let mutable found = false
        let mutable i = 0
        while not found && i < arr.Length do
            if predicate (NativePtr.get arr.Pointer i) then
                found <- true
            i <- i + 1
        found

    /// <summary>
    /// Returns true if all elements satisfy the predicate.
    /// </summary>
    let inline forall<'T when 'T : unmanaged> (predicate: 'T -> bool) (arr: NativeArray<'T>) : bool =
        let mutable allMatch = true
        let mutable i = 0
        while allMatch && i < arr.Length do
            if not (predicate (NativePtr.get arr.Pointer i)) then
                allMatch <- false
            i <- i + 1
        allMatch

    /// <summary>
    /// Finds the first element satisfying the predicate.
    /// Returns ValueNone if not found.
    /// </summary>
    let inline tryFind<'T when 'T : unmanaged> (predicate: 'T -> bool) (arr: NativeArray<'T>) : 'T voption =
        let mutable result = ValueNone
        let mutable i = 0
        while result.IsNone && i < arr.Length do
            let elem = NativePtr.get arr.Pointer i
            if predicate elem then
                result <- ValueSome elem
            i <- i + 1
        result

    /// <summary>
    /// Finds the index of the first element satisfying the predicate.
    /// Returns -1 if not found.
    /// </summary>
    let inline tryFindIndex<'T when 'T : unmanaged> (predicate: 'T -> bool) (arr: NativeArray<'T>) : int =
        let mutable result = -1
        let mutable i = 0
        while result = -1 && i < arr.Length do
            if predicate (NativePtr.get arr.Pointer i) then
                result <- i
            i <- i + 1
        result
