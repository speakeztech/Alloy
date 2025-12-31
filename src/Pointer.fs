#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// <summary>
/// Native pointer operations for native compilation.
/// Provides typed wrapper functions around raw pointers with common operations.
/// These map directly to LLVM pointer operations.
/// </summary>
[<AutoOpen>]
module Pointer =

    /// <summary>
    /// Creates a null pointer of the specified type.
    /// </summary>
    let inline nullPtr<'T when 'T : unmanaged> : nativeptr<'T> =
        NativePtr.nullPtr<'T>

    /// <summary>
    /// Returns true if the pointer is null.
    /// </summary>
    let inline isNull<'T when 'T : unmanaged> (ptr: nativeptr<'T>) : bool =
        NativePtr.toNativeInt ptr = 0n

    /// <summary>
    /// Returns true if the pointer is not null.
    /// </summary>
    let inline isNotNull<'T when 'T : unmanaged> (ptr: nativeptr<'T>) : bool =
        NativePtr.toNativeInt ptr <> 0n

    /// <summary>
    /// Reads the value at the pointer location.
    /// </summary>
    let inline read<'T when 'T : unmanaged> (ptr: nativeptr<'T>) : 'T =
        NativePtr.read ptr

    /// <summary>
    /// Writes a value to the pointer location.
    /// </summary>
    let inline write<'T when 'T : unmanaged> (value: 'T) (ptr: nativeptr<'T>) : unit =
        NativePtr.write ptr value

    /// <summary>
    /// Gets the value at the specified offset from the pointer.
    /// </summary>
    let inline get<'T when 'T : unmanaged> (index: int) (ptr: nativeptr<'T>) : 'T =
        NativePtr.get ptr index

    /// <summary>
    /// Sets the value at the specified offset from the pointer.
    /// </summary>
    let inline set<'T when 'T : unmanaged> (index: int) (value: 'T) (ptr: nativeptr<'T>) : unit =
        NativePtr.set ptr index value

    /// <summary>
    /// Advances the pointer by the specified number of elements.
    /// </summary>
    let inline add<'T when 'T : unmanaged> (offset: int) (ptr: nativeptr<'T>) : nativeptr<'T> =
        NativePtr.add ptr offset

    /// <summary>
    /// Moves the pointer back by the specified number of elements.
    /// </summary>
    let inline sub<'T when 'T : unmanaged> (offset: int) (ptr: nativeptr<'T>) : nativeptr<'T> =
        NativePtr.add ptr -offset

    /// <summary>
    /// Converts a pointer to a native integer (address).
    /// </summary>
    let inline toNativeInt<'T when 'T : unmanaged> (ptr: nativeptr<'T>) : nativeint =
        NativePtr.toNativeInt ptr

    /// <summary>
    /// Creates a pointer from a native integer (address).
    /// </summary>
    let inline ofNativeInt<'T when 'T : unmanaged> (addr: nativeint) : nativeptr<'T> =
        NativePtr.ofNativeInt addr

    /// <summary>
    /// Converts a pointer to a void pointer.
    /// </summary>
    let inline toVoidPtr<'T when 'T : unmanaged> (ptr: nativeptr<'T>) : voidptr =
        NativePtr.toVoidPtr ptr

    /// <summary>
    /// Creates a typed pointer from a void pointer.
    /// </summary>
    let inline ofVoidPtr<'T when 'T : unmanaged> (ptr: voidptr) : nativeptr<'T> =
        NativePtr.ofVoidPtr ptr

    /// <summary>
    /// Computes the difference between two pointers in elements.
    /// </summary>
    let inline diff<'T when 'T : unmanaged> (p1: nativeptr<'T>) (p2: nativeptr<'T>) : int =
        let addr1 = NativePtr.toNativeInt p1
        let addr2 = NativePtr.toNativeInt p2
        int ((addr1 - addr2) / nativeint sizeof<'T>)

    /// <summary>
    /// Copies count elements from source to destination.
    /// </summary>
    let inline copy<'T when 'T : unmanaged> (src: nativeptr<'T>) (dst: nativeptr<'T>) (count: int) : unit =
        for i = 0 to count - 1 do
            NativePtr.set dst i (NativePtr.get src i)

    /// <summary>
    /// Fills count elements starting at ptr with the specified value.
    /// </summary>
    let inline fill<'T when 'T : unmanaged> (ptr: nativeptr<'T>) (count: int) (value: 'T) : unit =
        for i = 0 to count - 1 do
            NativePtr.set ptr i value

    /// <summary>
    /// Compares two memory regions element by element.
    /// Returns 0 if equal, negative if first is less, positive if first is greater.
    /// </summary>
    let inline compare<'T when 'T : unmanaged and 'T : comparison> (p1: nativeptr<'T>) (p2: nativeptr<'T>) (count: int) : int =
        let mutable result = 0
        let mutable i = 0
        while result = 0 && i < count do
            let v1 = NativePtr.get p1 i
            let v2 = NativePtr.get p2 i
            if v1 < v2 then result <- -1
            elif v1 > v2 then result <- 1
            i <- i + 1
        result

    /// <summary>
    /// Checks if two memory regions are equal.
    /// </summary>
    let inline equals<'T when 'T : unmanaged and 'T : equality> (p1: nativeptr<'T>) (p2: nativeptr<'T>) (count: int) : bool =
        let mutable equal = true
        let mutable i = 0
        while equal && i < count do
            if NativePtr.get p1 i <> NativePtr.get p2 i then
                equal <- false
            i <- i + 1
        equal

    /// <summary>
    /// Searches for a value in memory.
    /// Returns the index if found, -1 otherwise.
    /// </summary>
    let inline indexOf<'T when 'T : unmanaged and 'T : equality> (ptr: nativeptr<'T>) (count: int) (value: 'T) : int =
        let mutable result = -1
        let mutable i = 0
        while result = -1 && i < count do
            if NativePtr.get ptr i = value then
                result <- i
            i <- i + 1
        result

    /// <summary>
    /// Swaps values at two pointer locations.
    /// </summary>
    let inline swap<'T when 'T : unmanaged> (p1: nativeptr<'T>) (p2: nativeptr<'T>) : unit =
        let temp = NativePtr.read p1
        NativePtr.write p1 (NativePtr.read p2)
        NativePtr.write p2 temp

    /// <summary>
    /// Reverses elements in place.
    /// </summary>
    let inline reverse<'T when 'T : unmanaged> (ptr: nativeptr<'T>) (count: int) : unit =
        let mutable i = 0
        let mutable j = count - 1
        while i < j do
            let temp = NativePtr.get ptr i
            NativePtr.set ptr i (NativePtr.get ptr j)
            NativePtr.set ptr j temp
            i <- i + 1
            j <- j - 1
