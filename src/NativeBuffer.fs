#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Native buffer types for stack-allocated memory
/// These types work with Firefly's native compilation without heap allocation
[<AutoOpen>]
module NativeBuffer =

    /// A fixed-size buffer allocated on the stack.
    /// This is the primary type for stack-allocated I/O buffers in Fidelity applications.
    [<Struct>]
    [<NoEquality; NoComparison>]
    type StackBuffer<'T when 'T : unmanaged> =
        val private ptr: nativeptr<'T>
        val private size: int

        /// Creates a new stack-allocated buffer with the specified size.
        new (size: int) =
            { ptr = NativePtr.stackalloc<'T> size
              size = size }

        /// Gets the pointer to the buffer memory.
        member this.Pointer = this.ptr

        /// Gets the length of the buffer in elements.
        member this.Length = this.size

        /// Disposes of the buffer. For StackBuffer, this is a no-op
        /// as stack memory is cleaned up automatically when the function returns.
        member this.Dispose() = ()

        interface System.IDisposable with
            member this.Dispose() = ()

    /// Creates a stack-allocated buffer of the specified size.
    /// Use with 'use' binding for automatic cleanup semantics:
    ///   use buffer = stackBuffer<byte> 256
    let inline stackBuffer<'T when 'T : unmanaged> (size: int) : StackBuffer<'T> =
        new StackBuffer<'T>(size)

    /// Gets a value from a StackBuffer at the specified index.
    let inline getFromBuffer<'T when 'T : unmanaged> (buffer: StackBuffer<'T>) (index: int) : 'T =
        NativePtr.get buffer.Pointer index

    /// Sets a value in a StackBuffer at the specified index.
    let inline setInBuffer<'T when 'T : unmanaged> (buffer: StackBuffer<'T>) (index: int) (value: 'T) : unit =
        NativePtr.set buffer.Pointer index value

    /// Fills a StackBuffer with a specified value.
    let inline fillBuffer<'T when 'T : unmanaged> (buffer: StackBuffer<'T>) (value: 'T) : unit =
        for i = 0 to buffer.Length - 1 do
            NativePtr.set buffer.Pointer i value

    /// Clears a StackBuffer (fills with zero bytes).
    let inline clearBuffer<'T when 'T : unmanaged> (buffer: StackBuffer<'T>) : unit =
        fillBuffer buffer Unchecked.defaultof<'T>
