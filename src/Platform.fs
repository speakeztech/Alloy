#nowarn "9"
namespace Alloy

/// Marker type indicating a function's implementation is provided by the platform.
/// Alex recognizes calls to functions in Platform.Bindings and generates
/// platform-specific code (syscalls, API calls, etc.).
[<Struct>]
type PlatformProvided = PlatformProvided

/// Platform-provided bindings for Fidelity native compilation.
///
/// Core I/O bindings (writeBytes, readBytes, abort) are defined in Primitives.Bindings.
/// This module re-exports them and adds additional platform bindings (time, string, etc.).
///
/// Alex recognizes calls to both Primitives.Bindings.* and Platform.Bindings.* functions
/// and dispatches to platform-specific implementations.
///
/// DO NOT add platform-specific code here. This module must remain platform-agnostic.
/// Alex provides platform-specific implementations via Bindings modules, dispatching
/// by function name and target platform to generate platform-appropriate MLIR.
module Platform =

    /// Platform binding declarations. Alex intercepts all calls to this module.
    module Bindings =

        // ═══════════════════════════════════════════════════════════════════════════
        // Console I/O Bindings (delegated to Primitives.Bindings)
        // ═══════════════════════════════════════════════════════════════════════════

        /// Write bytes to a file descriptor.
        /// Alex implementations: Linux/macOS: write() syscall, Windows: WriteFile() API
        let inline writeBytes (fd: int) (buffer: nativeint) (count: int) : int =
            Primitives.Bindings.writeBytes fd buffer count

        /// Read bytes from a file descriptor.
        /// Alex implementations: Linux/macOS: read() syscall, Windows: ReadFile() API
        let inline readBytes (fd: int) (buffer: nativeint) (maxCount: int) : int =
            Primitives.Bindings.readBytes fd buffer maxCount

        // ═══════════════════════════════════════════════════════════════════════════
        // String Bindings
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get length of a null-terminated string pointer.
        /// Alex implementations: Inline loop counting bytes until null terminator
        let strlen (str: nativeint) : int =
            Unchecked.defaultof<int>

        // ═══════════════════════════════════════════════════════════════════════════
        // Time Bindings
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get current time in ticks (100-nanosecond intervals since 0001-01-01).
        /// Alex implementations: Linux: clock_gettime, macOS: gettimeofday, Windows: GetSystemTimeAsFileTime
        let getCurrentTicks () : int64 =
            Unchecked.defaultof<int64>

        /// Get high-resolution monotonic ticks for timing.
        /// Alex implementations: Linux: clock_gettime(MONOTONIC), macOS: mach_absolute_time, Windows: QueryPerformanceCounter
        let getMonotonicTicks () : int64 =
            Unchecked.defaultof<int64>

        /// Get tick frequency (ticks per second) for high-resolution timer.
        /// Alex implementations: Linux: 1e9, macOS: mach_timebase_info, Windows: QueryPerformanceFrequency
        let getTickFrequency () : int64 =
            Unchecked.defaultof<int64>

        /// Sleep for specified milliseconds.
        /// Alex implementations: Linux/macOS: nanosleep, Windows: Sleep
        let sleep (milliseconds: int) : unit =
            ()

        // ═══════════════════════════════════════════════════════════════════════════
        // Process Control Bindings (delegated to Primitives.Bindings)
        // ═══════════════════════════════════════════════════════════════════════════

        /// Abort the process immediately with an exit code.
        /// Alex implementations: Linux/macOS: exit() syscall, Windows: ExitProcess() API
        /// This function never returns.
        let inline abort (exitCode: int) : unit =
            Primitives.Bindings.abort exitCode
