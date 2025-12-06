#nowarn "9"

namespace Alloy.Time

open FSharp.NativeInterop
open Alloy.Time.NativeInterop
open Alloy.Time.Platform
open Alloy.Math.Operators

/// <summary>
/// macOS platform-specific time implementation
/// </summary>
module MacOS =
    /// <summary>
    /// macOS time structures
    /// </summary>
    [<Struct>]
    type Timespec =
        val mutable tv_sec: int64
        val mutable tv_nsec: int64
        
        /// <summary>
        /// Converts Timespec to ticks (100-nanosecond intervals)
        /// </summary>
        member this.ToTicks() =
            add (multiply this.tv_sec 10000000L) (divide this.tv_nsec 100L)

    /// <summary>
    /// macOS time functions using enhanced P/Invoke-like API
    /// </summary>
    module LibC =
        let gettimeofday_import : NativeImport<nativeint -> nativeint -> int> = 
            {
                LibraryName = "libSystem"
                FunctionName = "gettimeofday"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let clock_gettime_import : NativeImport<int -> nativeint -> int> = 
            {
                LibraryName = "libSystem"
                FunctionName = "clock_gettime"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let nanosleep_import : NativeImport<nativeint -> nativeint -> int> = 
            {
                LibraryName = "libSystem"
                FunctionName = "nanosleep"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let localtime_r_import : NativeImport<nativeint -> nativeint -> nativeint> = 
            {
                LibraryName = "libSystem"
                FunctionName = "localtime_r"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        /// <summary>
        /// Clock IDs for macOS
        /// </summary>
        type ClockID =
            | CLOCK_REALTIME = 0
            | CLOCK_MONOTONIC = 6
            | CLOCK_MONOTONIC_RAW = 4
        
        /// <summary>
        /// Gets the current time using gettimeofday
        /// </summary>
        let getTimeOfDay() =
            // Timeval structure (8 bytes tv_sec, 4 bytes tv_usec)
            let timeval = NativePtr.stackalloc<byte> 16
            let result = invokeFunc2 gettimeofday_import (NativePtr.toNativeInt timeval) nativeint.Zero
            
            if result = 0 then
                // Get seconds and microseconds from timeval
                let sec = NativePtr.read (NativePtr.ofNativeInt<int64> (NativePtr.toNativeInt timeval))
                let usec = NativePtr.read (NativePtr.ofNativeInt<int32> (NativePtr.add (NativePtr.ofNativeInt<int64> (NativePtr.toNativeInt timeval)) 1))
                
                // Convert to ticks (100-nanosecond intervals)
                add (multiply sec 10000000L) (multiply (int64 usec) 10L)
            else
                failwith $"gettimeofday failed with error code {result}"
        
        /// <summary>
        /// Gets the current time from the specified clock
        /// </summary>
        let clockGetTime(clockId: ClockID) =
            let timespec = NativePtr.stackalloc<Timespec> 1
            let result = invokeFunc2 
                            clock_gettime_import
                            (int clockId) 
                            (NativePtr.toNativeInt timespec)
            
            if result = 0 then
                let ts = NativePtr.read timespec
                ts.ToTicks()
            else
                failwith $"clock_gettime failed with error code {result}"
        
        /// <summary>
        /// Suspends execution for the specified time interval
        /// </summary>
        let nanosleep(seconds: int64, nanoseconds: int64) =
            let req = NativePtr.stackalloc<Timespec> 1
            let mutable reqTs = NativePtr.get req 0
            reqTs.tv_sec <- seconds
            reqTs.tv_nsec <- nanoseconds
            NativePtr.set req 0 reqTs
            
            let rem = NativePtr.stackalloc<Timespec> 1
            
            let result = invokeFunc2 
                            nanosleep_import
                            (NativePtr.toNativeInt req) 
                            (NativePtr.toNativeInt rem)
                            
            if result < 0 then
                failwith "nanosleep failed"

    /// <summary>
    /// Helper constants for time conversion
    /// </summary>
    module TimeConversion =
        let private unixToNetTicksOffset = 621355968000000000L
        
        /// <summary>
        /// Convert Unix time (seconds since 1970) to .NET ticks
        /// </summary>
        let unixTimeToTicks (unixTime: int64) =
            add (multiply unixTime 10000000L) unixToNetTicksOffset
        
        /// <summary>
        /// Convert .NET ticks to Unix time
        /// </summary>
        let ticksToUnixTime (ticks: int64) =
            divide (subtract ticks unixToNetTicksOffset) 10000000L

    /// <summary>
    /// macOS platform implementation of IPlatformTime and ITimeCapability
    /// </summary>
    type MacOSTimeImplementation() =
        // Singleton instance for static methods
        static let instance = MacOSTimeImplementation()
        
        // Static implementation
        static member GetCurrentTicks() =
            let unixTime = LibC.getTimeOfDay()
            TimeConversion.unixTimeToTicks(divide unixTime 10000000L)
        
        static member GetUtcNow() =
            let unixTime = LibC.getTimeOfDay()
            TimeConversion.unixTimeToTicks(divide unixTime 10000000L)
        
        static member GetSystemTimeAsFileTime() =
            let unixTime = LibC.getTimeOfDay()
            add unixTime 116444736000000000L
        
        static member GetHighResolutionTicks() =
            LibC.clockGetTime(LibC.ClockID.CLOCK_MONOTONIC)
        
        static member GetTickFrequency() =
            1000000000L // nanosecond precision
        
        static member Sleep(milliseconds: int) =
            let seconds = int64 (divide milliseconds 1000)
            let nanoseconds = int64 (multiply (modulo milliseconds 1000) 1000000)
            LibC.nanosleep(seconds, nanoseconds)
            
        static member GetTimezoneOffsetMinutes() =
            0 // We'd need to implement timezone detection for macOS
        
        // Interface implementation for runtime polymorphism
        interface IPlatformTime with
            member _.GetCurrentTicks() = MacOSTimeImplementation.GetCurrentTicks()
            member _.GetUtcNow() = MacOSTimeImplementation.GetUtcNow()
            member _.GetSystemTimeAsFileTime() = MacOSTimeImplementation.GetSystemTimeAsFileTime()
            member _.GetHighResolutionTicks() = MacOSTimeImplementation.GetHighResolutionTicks()
            member _.GetTickFrequency() = MacOSTimeImplementation.GetTickFrequency()
            member _.Sleep(milliseconds) = MacOSTimeImplementation.Sleep(milliseconds)
            member _.GetTimezoneOffsetMinutes() = MacOSTimeImplementation.GetTimezoneOffsetMinutes()
        
        // Static interface implementation for compile-time resolution
        interface ITimeCapability<MacOSTimeImplementation> with
            static member GetCurrentTicks() = MacOSTimeImplementation.GetCurrentTicks()
            static member GetUtcNow() = MacOSTimeImplementation.GetUtcNow()
            static member GetSystemTimeAsFileTime() = MacOSTimeImplementation.GetSystemTimeAsFileTime()
            static member GetHighResolutionTicks() = MacOSTimeImplementation.GetHighResolutionTicks()
            static member GetTickFrequency() = MacOSTimeImplementation.GetTickFrequency()
            static member Sleep(milliseconds) = MacOSTimeImplementation.Sleep(milliseconds)
            static member GetTimezoneOffsetMinutes() = MacOSTimeImplementation.GetTimezoneOffsetMinutes()

    /// <summary>
    /// Factory function to create a macOS time implementation
    /// </summary>
    let createImplementation() =
        MacOSTimeImplementation() :> IPlatformTime