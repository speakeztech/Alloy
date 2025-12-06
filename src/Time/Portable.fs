namespace Alloy.Time

open Alloy.Time.Platform
open Alloy.Math.Operators

/// <summary>
/// Portable time implementation using minimal system dependencies
/// </summary>
module Portable =
    /// <summary>
    /// Portable implementation of IPlatformTime that works across platforms
    /// </summary>
    type PortableTimeImplementation() =
        // Singleton instance for static methods
        static let mutable tickCounter = 0L
        static let ticksPerSecond = 10000000L // 100-nanosecond intervals
        static let startTime = 638585088000000000L // Approximate ticks for 2025-01-01
        
        // Static implementation
        static member GetCurrentTicks() =
            tickCounter <- add tickCounter ticksPerSecond
            add startTime tickCounter
        
        static member GetUtcNow() =
            tickCounter <- add tickCounter ticksPerSecond
            add startTime tickCounter
        
        static member GetSystemTimeAsFileTime() =
            let currentTicks = add startTime tickCounter
            tickCounter <- add tickCounter ticksPerSecond
            // Convert from .NET epoch to Windows file time epoch
            subtract currentTicks 504911232000000000L
        
        static member GetHighResolutionTicks() =
            tickCounter <- add tickCounter 1L
            tickCounter
        
        static member GetTickFrequency() =
            ticksPerSecond
        
        static member Sleep(milliseconds: int) =
            // Simulate sleep by advancing the tick counter
            let ticksToAdd = multiply (int64 milliseconds) (divide ticksPerSecond 1000L)
            tickCounter <- add tickCounter ticksToAdd
            
        static member GetTimezoneOffsetMinutes() =
            0 // Portable fallback: assume UTC
        
        // Instance for runtime polymorphism
        let mutable instanceTickCounter = 0L
        
        // Interface implementation for runtime polymorphism
        interface IPlatformTime with
            member _.GetCurrentTicks() = PortableTimeImplementation.GetCurrentTicks()
            member _.GetUtcNow() = PortableTimeImplementation.GetUtcNow()
            member _.GetSystemTimeAsFileTime() = PortableTimeImplementation.GetSystemTimeAsFileTime()
            member _.GetHighResolutionTicks() = PortableTimeImplementation.GetHighResolutionTicks()
            member _.GetTickFrequency() = PortableTimeImplementation.GetTickFrequency()
            member _.Sleep(milliseconds) = PortableTimeImplementation.Sleep(milliseconds)
            member _.GetTimezoneOffsetMinutes() = PortableTimeImplementation.GetTimezoneOffsetMinutes()
        
        // Static interface implementation for compile-time resolution
        interface ITimeCapability<PortableTimeImplementation> with
            static member GetCurrentTicks() = PortableTimeImplementation.GetCurrentTicks()
            static member GetUtcNow() = PortableTimeImplementation.GetUtcNow()
            static member GetSystemTimeAsFileTime() = PortableTimeImplementation.GetSystemTimeAsFileTime()
            static member GetHighResolutionTicks() = PortableTimeImplementation.GetHighResolutionTicks()
            static member GetTickFrequency() = PortableTimeImplementation.GetTickFrequency()
            static member Sleep(milliseconds) = PortableTimeImplementation.Sleep(milliseconds)
            static member GetTimezoneOffsetMinutes() = PortableTimeImplementation.GetTimezoneOffsetMinutes()

    /// <summary>
    /// Factory function to create a portable time implementation
    /// </summary>
    let createImplementation() =
        PortableTimeImplementation() :> IPlatformTime