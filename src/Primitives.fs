#nowarn "9"
namespace Alloy

open System.Runtime.InteropServices

/// Abstract primitives for Fidelity native compilation.
///
/// These are extern declarations that Alex (the compiler targeting layer) provides
/// platform-specific implementations for. The "__fidelity" library name is a marker
/// that tells Alex to generate platform-appropriate code (syscalls, API calls, etc.).
///
/// This follows the F# DllImport pattern but with compile-time resolution instead
/// of runtime P/Invoke. The PSG captures the full metadata (library, entry point,
/// calling convention) and Alex dispatches to the appropriate binding.
///
/// DO NOT add platform-specific code here. This module must remain platform-agnostic.
/// Platform implementations belong in Alex/Bindings/{Linux,MacOS,Windows,Embedded}/.
module Primitives =

    // ═══════════════════════════════════════════════════════════════════════════
    // Console I/O Primitives
    // ═══════════════════════════════════════════════════════════════════════════

    // Write bytes to a file descriptor.
    // Alex implementations: Linux/macOS: write() syscall, Windows: WriteFile() API
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_write_bytes")>]
    extern int writeBytes(int fd, nativeint buffer, int count)

    // Read bytes from a file descriptor.
    // Alex implementations: Linux/macOS: read() syscall, Windows: ReadFile() API
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_read_bytes")>]
    extern int readBytes(int fd, nativeint buffer, int maxCount)

    // ═══════════════════════════════════════════════════════════════════════════
    // Time Primitives
    // ═══════════════════════════════════════════════════════════════════════════

    // Get current time in ticks (100-nanosecond intervals since 0001-01-01).
    // Alex implementations: Linux: clock_gettime, macOS: gettimeofday, Windows: GetSystemTimeAsFileTime
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_get_current_ticks")>]
    extern int64 getCurrentTicks()

    // Get high-resolution monotonic ticks for timing.
    // Alex implementations: Linux: clock_gettime(MONOTONIC), macOS: mach_absolute_time, Windows: QueryPerformanceCounter
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_get_monotonic_ticks")>]
    extern int64 getMonotonicTicks()

    // Get tick frequency (ticks per second) for high-resolution timer.
    // Alex implementations: Linux: 1e9, macOS: mach_timebase_info, Windows: QueryPerformanceFrequency
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_get_tick_frequency")>]
    extern int64 getTickFrequency()

    // Sleep for specified milliseconds.
    // Alex implementations: Linux/macOS: nanosleep, Windows: Sleep
    [<DllImport("__fidelity", CallingConvention = CallingConvention.Cdecl, EntryPoint = "fidelity_sleep")>]
    extern void sleep(int milliseconds)

