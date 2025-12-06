namespace Alloy.Time

open FSharp.NativeInterop
open Alloy.Time.NativeInterop
open Alloy.Time.Platform
open Alloy.Math.Operators

// [Existing code remains unchanged...]

/// <summary>
/// Windows platform implementation of IPlatformTime and ITimeCapability
/// </summary>
type WindowsTimeImplementation() =
    // Singleton instance for static methods
    static let instance = WindowsTimeImplementation()
    
    // Static implementation
    static member GetCurrentTicks() =
        let fileTime = Kernel32.getSystemTimeAsFileTime()
        TimeConversion.fileTimeToTicks fileTime
        
    static member GetUtcNow() =
        let fileTime = Kernel32.getSystemTimeAsFileTime()
        TimeConversion.fileTimeToTicks fileTime
    
    static member GetSystemTimeAsFileTime() =
        Kernel32.getSystemTimeAsFileTime()
    
    static member GetHighResolutionTicks() =
        Kernel32.queryPerformanceCounter()
    
    static member GetTickFrequency() =
        Kernel32.queryPerformanceFrequency()
    
    static member Sleep(milliseconds: int) =
        Kernel32.sleep(uint32 milliseconds)
        
    static member GetTimezoneOffsetMinutes() =
        TimeConversion.getCurrentTimeZoneOffsetMinutes()
    
    // Interface implementation for runtime polymorphism
    interface IPlatformTime with
        member _.GetCurrentTicks() = WindowsTimeImplementation.GetCurrentTicks()
        member _.GetUtcNow() = WindowsTimeImplementation.GetUtcNow()
        member _.GetSystemTimeAsFileTime() = WindowsTimeImplementation.GetSystemTimeAsFileTime()
        member _.GetHighResolutionTicks() = WindowsTimeImplementation.GetHighResolutionTicks()
        member _.GetTickFrequency() = WindowsTimeImplementation.GetTickFrequency()
        member _.Sleep(milliseconds) = WindowsTimeImplementation.Sleep(milliseconds)
        member _.GetTimezoneOffsetMinutes() = WindowsTimeImplementation.GetTimezoneOffsetMinutes()
    
    // Static interface implementation for compile-time resolution
    interface ITimeCapability<WindowsTimeImplementation> with
        static member GetCurrentTicks() = WindowsTimeImplementation.GetCurrentTicks()
        static member GetUtcNow() = WindowsTimeImplementation.GetUtcNow()
        static member GetSystemTimeAsFileTime() = WindowsTimeImplementation.GetSystemTimeAsFileTime()
        static member GetHighResolutionTicks() = WindowsTimeImplementation.GetHighResolutionTicks()
        static member GetTickFrequency() = WindowsTimeImplementation.GetTickFrequency()
        static member Sleep(milliseconds) = WindowsTimeImplementation.Sleep(milliseconds)
        static member GetTimezoneOffsetMinutes() = WindowsTimeImplementation.GetTimezoneOffsetMinutes()

/// <summary>
/// Factory function to create a Windows time implementation
/// </summary>
let createImplementation() =
    WindowsTimeImplementation() :> IPlatformTime