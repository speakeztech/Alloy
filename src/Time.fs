namespace Alloy

open Alloy.Time.Platform
open Alloy.Math.Operators
open Alloy.Math.Functions

/// <summary>
/// Pure F# time utilities with no System.DateTime dependencies
/// </summary>
[<RequireQualifiedAccess>]
module Time =
    // [Existing type definitions and constants remain unchanged]
    
    /// <summary>
    /// Gets the platform time implementation at compile time
    /// </summary>
    let inline getImplementation() =
        #if WINDOWS
        Windows.createImplementation()
        #elif LINUX
        Linux.createImplementation()
        #elif MACOS
        MacOS.createImplementation()
        #else
        Portable.createImplementation()
        #endif
    
    /// <summary>
    /// Runtime platform time implementation - used for operations that can't be statically resolved
    /// </summary>
    let private platformTime = getImplementation()
    
    /// <summary>
    /// Type alias for the platform-specific implementation type
    /// </summary>
    type private PlatformType =
        #if WINDOWS
        Windows.WindowsTimeImplementation
        #elif LINUX
        Linux.LinuxTimeImplementation
        #elif MACOS
        MacOS.MacOSTimeImplementation
        #else
        Portable.PortableTimeImplementation
        #endif
    
    /// <summary>
    /// Gets the current time in ticks using static resolution
    /// </summary>
    let inline currentTicks (): int64 =
        TimeOperations.getCurrentTicks<PlatformType>()
    
    /// <summary>
    /// Gets the current Unix timestamp (seconds since 1970-01-01)
    /// </summary>
    let inline currentUnixTimestamp (): int64 =
        let ticks = currentTicks()
        divide (subtract ticks unixEpochTicks) ticksPerSecond
    
    /// <summary>
    /// Gets the current Unix timestamp with nanosecond precision
    /// </summary>
    let inline currentTimestamp (): Timestamp =
        let ticks = currentTicks()
        let ticksSinceEpoch = subtract ticks unixEpochTicks
        let seconds = divide ticksSinceEpoch ticksPerSecond
        let tickRemainder = modulo ticksSinceEpoch ticksPerSecond
        let nanoseconds = int (multiply tickRemainder 100L) // Convert 100ns ticks to nanoseconds
        { Seconds = seconds; Nanoseconds = nanoseconds }
    
    /// <summary>
    /// Gets high-resolution performance counter ticks
    /// </summary>
    let inline highResolutionTicks (): int64 =
        TimeOperations.getHighResolutionTicks<PlatformType>()
        
    /// <summary>
    /// Gets the frequency of the high-resolution performance counter
    /// </summary>
    let inline tickFrequency (): int64 =
        TimeOperations.getTickFrequency<PlatformType>()
        
    /// <summary>
    /// Gets the current timezone offset from UTC in minutes
    /// Returns positive values for west of UTC, negative for east of UTC
    /// </summary>
    let inline getCurrentTimezoneOffsetMinutes (): int =
        TimeOperations.getTimezoneOffsetMinutes<PlatformType>()
    
    // [Rest of the implementation remains unchanged, with any calls to 
    // platformTime methods replaced with TimeOperations static calls]
    
    /// <summary>
    /// Sleeps for the specified number of milliseconds
    /// </summary>
    let inline sleep (milliseconds: int): unit =
        if lessThan milliseconds 0 then
            invalidArg "milliseconds" "Sleep duration must be non-negative"
        
        TimeOperations.sleep<PlatformType> milliseconds