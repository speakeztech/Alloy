// In Platform.fs
namespace Alloy.Time

/// <summary>
/// Platform interface definitions for Alloy time functions
/// </summary>
module Platform =
    /// <summary>
    /// Platform capabilities for time operations - used with SRTP
    /// </summary>
    type ITimeCapability<'TPlatform> =
        /// <summary>Gets the current time in ticks (100-nanosecond intervals since January 1, 0001)</summary>
        static abstract member GetCurrentTicks : unit -> int64
        
        /// <summary>Gets the current UTC time in ticks</summary>
        static abstract member GetUtcNow : unit -> int64
        
        /// <summary>Gets the system time as file time (100-nanosecond intervals since January 1, 1601)</summary>
        static abstract member GetSystemTimeAsFileTime : unit -> int64
        
        /// <summary>Gets high-resolution performance counter ticks</summary>
        static abstract member GetHighResolutionTicks : unit -> int64
        
        /// <summary>Gets the frequency of the high-resolution performance counter</summary>
        static abstract member GetTickFrequency : unit -> int64
        
        /// <summary>Sleeps for the specified number of milliseconds</summary>
        static abstract member Sleep : milliseconds:int -> unit
        
        /// <summary>Gets the current timezone offset from UTC in minutes</summary>
        static abstract member GetTimezoneOffsetMinutes : unit -> int
    
    /// <summary>
    /// Interface for runtime polymorphic time operations
    /// </summary>
    type IPlatformTime =
        /// <summary>Gets the current time in ticks</summary>
        abstract member GetCurrentTicks : unit -> int64
        
        /// <summary>Gets the current UTC time in ticks</summary>
        abstract member GetUtcNow : unit -> int64
        
        /// <summary>Gets the system time as file time</summary>
        abstract member GetSystemTimeAsFileTime : unit -> int64
        
        /// <summary>Gets high-resolution performance counter ticks</summary>
        abstract member GetHighResolutionTicks : unit -> int64
        
        /// <summary>Gets the frequency of the high-resolution performance counter</summary>
        abstract member GetTickFrequency : unit -> int64
        
        /// <summary>Sleeps for the specified number of milliseconds</summary>
        abstract member Sleep : milliseconds:int -> unit
        
        /// <summary>Gets the current timezone offset from UTC in minutes</summary>
        abstract member GetTimezoneOffsetMinutes : unit -> int

    /// <summary>
    /// Generic time operations with statically resolved type parameters
    /// </summary>
    [<RequireQualifiedAccess>]
    module TimeOperations =
        /// <summary>Gets the current time in ticks using SRTP</summary>
        let inline getCurrentTicks< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetCurrentTicks : unit -> int64) ())
            
        /// <summary>Gets the current UTC time using SRTP</summary>
        let inline getUtcNow< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetUtcNow : unit -> int64) ())
            
        /// <summary>Gets the system time as file time using SRTP</summary>
        let inline getSystemTimeAsFileTime< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetSystemTimeAsFileTime : unit -> int64) ())
            
        /// <summary>Gets high-resolution ticks using SRTP</summary>
        let inline getHighResolutionTicks< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetHighResolutionTicks : unit -> int64) ())
            
        /// <summary>Gets the tick frequency using SRTP</summary>
        let inline getTickFrequency< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetTickFrequency : unit -> int64) ())
            
        /// <summary>Sleeps for the specified milliseconds using SRTP</summary>
        let inline sleep< ^T when ^T :> ITimeCapability< ^T >> (milliseconds: int) =
            (^T : (static member Sleep : int -> unit) milliseconds)
            
        /// <summary>Gets the timezone offset using SRTP</summary>
        let inline getTimezoneOffsetMinutes< ^T when ^T :> ITimeCapability< ^T >>() =
            (^T : (static member GetTimezoneOffsetMinutes : unit -> int) ())

    /// <summary>
    /// Platform detection at compile time
    /// </summary>
    [<RequireQualifiedAccess>]
    module CompileTimePlatform =
        /// <summary>
        /// Marker type for platform detection - actual implementation selected at compile time
        /// </summary>
        type PlatformMarker = class end