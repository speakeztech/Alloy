#nowarn "9"
namespace Alloy

/// Marker type indicating a function's implementation is provided by the platform.
/// Alex recognizes calls to functions in Platform.Bindings and generates
/// platform-specific code (syscalls, API calls, etc.).
[<Struct>]
type PlatformProvided = PlatformProvided

/// Platform-provided bindings for Fidelity native compilation.
///
/// NOTE: Core I/O (write/read/exit) are now provided by FNCS Sys intrinsics:
///   - Sys.write: fd:int -> buffer:nativeptr<byte> -> count:int -> int
///   - Sys.read:  fd:int -> buffer:nativeptr<byte> -> maxCount:int -> int
///   - Sys.exit:  code:int -> 'a
///
/// This module contains additional platform bindings (time, string, etc.) that
/// need Alex-provided implementations. These are stubs until the corresponding
/// FNCS intrinsics are implemented.
///
/// DO NOT add platform-specific code here. This module must remain platform-agnostic.
module Platform =

    open FSharp.NativeInterop

    /// Platform binding declarations for functions not yet provided as FNCS intrinsics.
    /// NOTE: Use Sys.write, Sys.read, Sys.exit directly for I/O and process control.
    module Bindings =

        // ═══════════════════════════════════════════════════════════════════════════
        // String Bindings
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get length of a null-terminated string pointer.
        /// Implemented as a native loop - scans for null terminator.
        let inline strlen (str: nativeint) : int =
            let ptr = NativePtr.ofNativeInt<byte> str
            let mutable len = 0
            while NativePtr.get ptr len <> 0uy do
                len <- len + 1
            len

        // ═══════════════════════════════════════════════════════════════════════════
        // Time Bindings (stubs - need FNCS intrinsics: Sys.clock_gettime, etc.)
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get current time in ticks (100-nanosecond intervals since 0001-01-01).
        /// TODO: Needs Sys.clock_gettime intrinsic - returns 0 for now
        let getCurrentTicks () : int64 =
            NativeDefault.zeroed<int64>()

        /// Get high-resolution monotonic ticks for timing.
        /// TODO: Needs Sys.clock_gettime(MONOTONIC) intrinsic - returns 0 for now
        let getMonotonicTicks () : int64 =
            NativeDefault.zeroed<int64>()

        /// Get tick frequency (ticks per second) for high-resolution timer.
        /// TODO: Needs platform-specific frequency intrinsic - returns 0 for now
        let getTickFrequency () : int64 =
            NativeDefault.zeroed<int64>()

        /// Sleep for specified milliseconds.
        /// TODO: Needs Sys.nanosleep intrinsic - does nothing for now
        let sleep (milliseconds: int) : unit =
            ()
