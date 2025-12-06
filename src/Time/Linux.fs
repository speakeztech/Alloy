// In Linux.fs
namespace Alloy.Time

open FSharp.NativeInterop
open Alloy.Time.NativeInterop
open Alloy.Time.Platform
open Alloy.Math.Operators

// [Existing code remains unchanged...]

/// <summary>
/// Linux platform implementation of IPlatformTime and ITimeCapability
/// </summary>
type LinuxTimeImplementation() =
    // Singleton instance for static methods
    static let instance = LinuxTimeImplementation()
    
    // Static implementation
    static member GetCurrentTicks() =
        let unixTime = LibC.clockGettime(ClockID.CLOCK_REALTIME)
        add (TimeConversion.unixTimeToTicks(divide unixTime 10000000L)) (modulo unixTime 10000000L)
    
    static member GetUtcNow() =
        let unixTime = LibC.clockGettime(ClockID.CLOCK_REALTIME)
        add (TimeConversion.unixTimeToTicks(divide unixTime 10000000L)) (modulo unixTime 10000000L)
    
    static member GetSystemTimeAsFileTime() =
        let unixTime = LibC.clockGettime(ClockID.CLOCK_REALTIME)
        add unixTime 116444736000000000L
    
    static member GetHighResolutionTicks() =
        LibC.clockGettime(ClockID.CLOCK_MONOTONIC)
    
    static member GetTickFrequency() =
        1000000000L // nanosecond precision
    
    static member Sleep(milliseconds: int) =
        let seconds = int64 (divide milliseconds 1000)
        let nanoseconds = int64 (multiply (modulo milliseconds 1000) 1000000)
        LibC.nanosleep(seconds, nanoseconds)
        
    static member GetTimezoneOffsetMinutes() =
        try
            LibC.getTimezoneOffset()
        with
        | _ -> 0 // Fallback to UTC if timezone detection fails
    
    // Interface implementation for runtime polymorphism
    interface IPlatformTime with
        member _.GetCurrentTicks() = LinuxTimeImplementation.GetCurrentTicks()
        member _.GetUtcNow() = LinuxTimeImplementation.GetUtcNow()
        member _.GetSystemTimeAsFileTime() = LinuxTimeImplementation.GetSystemTimeAsFileTime()
        member _.GetHighResolutionTicks() = LinuxTimeImplementation.GetHighResolutionTicks()
        member _.GetTickFrequency() = LinuxTimeImplementation.GetTickFrequency()
        member _.Sleep(milliseconds) = LinuxTimeImplementation.Sleep(milliseconds)
        member _.GetTimezoneOffsetMinutes() = LinuxTimeImplementation.GetTimezoneOffsetMinutes()
    
    // Static interface implementation for compile-time resolution
    interface ITimeCapability<LinuxTimeImplementation> with
        static member GetCurrentTicks() = LinuxTimeImplementation.GetCurrentTicks()
        static member GetUtcNow() = LinuxTimeImplementation.GetUtcNow()
        static member GetSystemTimeAsFileTime() = LinuxTimeImplementation.GetSystemTimeAsFileTime()
        static member GetHighResolutionTicks() = LinuxTimeImplementation.GetHighResolutionTicks()
        static member GetTickFrequency() = LinuxTimeImplementation.GetTickFrequency()
        static member Sleep(milliseconds) = LinuxTimeImplementation.Sleep(milliseconds)
        static member GetTimezoneOffsetMinutes() = LinuxTimeImplementation.GetTimezoneOffsetMinutes()

/// <summary>
/// Factory function to create a Linux time implementation
/// </summary>
let createImplementation() =
    LinuxTimeImplementation() :> IPlatformTime