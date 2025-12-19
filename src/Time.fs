#nowarn "9"
namespace Alloy

open Alloy.Platform

/// BCL-compatible DateTime type for Fidelity native compilation.
/// Uses Platform.Bindings.getCurrentTicks() for actual time retrieval.
[<Struct>]
type DateTime = {
    /// Internal ticks representation (100-nanosecond intervals since 0001-01-01)
    Ticks: int64
}
with
    /// Gets the year component
    member this.Year: int =
        // Simplified calculation - proper calendar math would be more complex
        int ((this.Ticks / 315360000000000L) + 1L)

    /// Gets the month component (1-12)
    member this.Month: int = 1  // TODO: Proper calendar calculation

    /// Gets the day component (1-31)
    member this.Day: int = 1  // TODO: Proper calendar calculation

    /// Gets the hour component (0-23)
    member this.Hour: int =
        int ((this.Ticks / 36000000000L) % 24L)

    /// Gets the minute component (0-59)
    member this.Minute: int =
        int ((this.Ticks / 600000000L) % 60L)

    /// Gets the second component (0-59)
    member this.Second: int =
        int ((this.Ticks / 10000000L) % 60L)

    /// Gets the millisecond component (0-999)
    member this.Millisecond: int =
        int ((this.Ticks / 10000L) % 1000L)

    /// Returns a NativeStr representation of the DateTime
    member this.ToNativeString() : NativeStr =
        // Placeholder - would need Text.Format integration
        ofBytes "DateTime"B

/// BCL-compatible DateTime static members.
/// Delegates to Platform.Bindings.getCurrentTicks() for actual time.
module DateTime =
    /// Gets the current local date and time.
    /// Implementation: calls Platform.Bindings.getCurrentTicks() which Alex binds to platform syscalls.
    let inline Now () : DateTime = { Ticks = Bindings.getCurrentTicks() }

    /// Gets the current UTC date and time.
    /// For now, same as Now (timezone handling would require additional bindings).
    let inline UtcNow () : DateTime = { Ticks = Bindings.getCurrentTicks() }

    /// Gets today's date with time set to 00:00:00.
    let inline Today () : DateTime =
        let ticks = Bindings.getCurrentTicks()
        let ticksPerDay = 864000000000L
        { Ticks = (ticks / ticksPerDay) * ticksPerDay }

/// BCL-compatible TimeSpan type
[<Struct>]
type TimeSpan = {
    /// Total ticks (100-nanosecond intervals)
    Ticks: int64
}
with
    /// Gets the days component
    member this.Days: int = int (this.Ticks / 864000000000L)

    /// Gets the hours component
    member this.Hours: int = int ((this.Ticks / 36000000000L) % 24L)

    /// Gets the minutes component
    member this.Minutes: int = int ((this.Ticks / 600000000L) % 60L)

    /// Gets the seconds component
    member this.Seconds: int = int ((this.Ticks / 10000000L) % 60L)

    /// Gets the milliseconds component
    member this.Milliseconds: int = int ((this.Ticks / 10000L) % 1000L)

    /// Gets the total milliseconds
    member this.TotalMilliseconds: float = float this.Ticks / 10000.0

/// BCL-compatible Thread.Sleep.
/// Delegates to Platform.Bindings.sleep().
module Threading =
    module Thread =
        /// Suspends the current thread for the specified number of milliseconds.
        /// Implementation: calls Platform.Bindings.sleep() which Alex binds to nanosleep/Sleep.
        let inline Sleep (milliseconds: int) : unit =
            Bindings.sleep milliseconds

/// Alloy-specific time utilities (non-BCL).
/// These provide lower-level access for performance-critical code.
/// All operations delegate to Platform.Bindings.
[<RequireQualifiedAccess>]
module Time =
    /// Gets the current time in ticks (100-nanosecond intervals since 0001-01-01).
    /// Delegates to Platform.Bindings.getCurrentTicks().
    let inline currentTicks () : int64 =
        Bindings.getCurrentTicks()

    /// Gets high-resolution monotonic ticks for timing.
    /// Delegates to Platform.Bindings.getMonotonicTicks().
    let inline highResolutionTicks () : int64 =
        Bindings.getMonotonicTicks()

    /// Gets the frequency of the high-resolution timer (ticks per second).
    /// Delegates to Platform.Bindings.getTickFrequency().
    let inline tickFrequency () : int64 =
        Bindings.getTickFrequency()

    /// Gets the current Unix timestamp (seconds since 1970-01-01).
    /// Computed from ticks with epoch conversion.
    let inline currentUnixTimestamp () : int64 =
        let ticks = Bindings.getCurrentTicks()
        // Unix epoch is 1970-01-01, .NET epoch is 0001-01-01
        // Difference: 621355968000000000 ticks
        let unixEpochTicks = 621355968000000000L
        let ticksPerSecond = 10000000L
        (ticks - unixEpochTicks) / ticksPerSecond

    /// Sleeps for the specified number of milliseconds.
    /// Delegates to Platform.Bindings.sleep().
    let inline sleep (milliseconds: int) : unit =
        Bindings.sleep milliseconds
