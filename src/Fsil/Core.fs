/// Alloy.Fsil - Native SRTP-based polymorphic operations
/// A clean reimplementation of Fsil without System.* dependencies
/// Provides zero-cost abstractions through statically resolved type parameters
[<AutoOpen>]
module Alloy.Fsil

// ============================================================================
// Internal SRTP Witness Types
// ============================================================================

/// Internal module containing SRTP witness types for polymorphic dispatch
[<AbstractClass>]
module Internal =

    // ------------------------------------------------------------------------
    // Value Extraction
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Value =
        static member inline Value(x: Result<'t, _>) : 't =
            match x with
            | Ok v -> v
            | Error _ -> panicwith (ofBytes "Result has no value"B)

        static member inline Value(x: ^t) : ^v =
            (^t: (member Value: ^v) x)

        static member inline Invoke< ^I, ^v
            when (^I or Value): (static member Value: ^I -> ^v)>
            (source: ^I) : ^v =
            ((^I or Value): (static member Value: ^I -> ^v) source)

    // ------------------------------------------------------------------------
    // Tuple Accessors
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Item1 =
        static member inline Item1(struct (x, _)) : 't = x
        static member inline Item1(struct (x, _, _)) : 't = x
        static member inline Item1(struct (x, _, _, _)) : 't = x
        static member inline Item1(x: ^t) : ^r = (^t: (member Item1: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item1): (static member Item1: ^I -> ^r)>
            (source: ^I) : ^r =
            ((^I or Item1): (static member Item1: ^I -> ^r) source)

    [<AbstractClass; Sealed>]
    type Item2 =
        static member inline Item2(struct (_, x)) : 't = x
        static member inline Item2(struct (_, x, _)) : 't = x
        static member inline Item2(struct (_, x, _, _)) : 't = x
        static member inline Item2(x: ^t) : ^r = (^t: (member Item2: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item2): (static member Item2: ^I -> ^r)>
            (source: ^I) : ^r =
            ((^I or Item2): (static member Item2: ^I -> ^r) source)

    [<AbstractClass; Sealed>]
    type Item3 =
        static member inline Item3(struct (_, _, x)) : 't = x
        static member inline Item3(struct (_, _, x, _)) : 't = x
        static member inline Item3(x: ^t) : ^r = (^t: (member Item3: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item3): (static member Item3: ^I -> ^r)>
            (source: ^I) : ^r =
            ((^I or Item3): (static member Item3: ^I -> ^r) source)

    [<AbstractClass; Sealed>]
    type Item4 =
        static member inline Item4(struct (_, _, _, x)) : 't = x
        static member inline Item4(x: ^t) : ^r = (^t: (member Item4: ^r) x)

        static member inline Invoke< ^I, ^r
            when (^I or Item4): (static member Item4: ^I -> ^r)>
            (source: ^I) : ^r =
            ((^I or Item4): (static member Item4: ^I -> ^r) source)

    // ------------------------------------------------------------------------
    // Default Values for Primitives
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Default =
        // F# built-in types
        static member inline Default(_f: option<'t> -> unit) : option<'t> = None
        static member inline Default(_f: voption<'t> -> unit) : voption<'t> = ValueNone
        static member inline Default(_f: list<'t> -> unit) : list<'t> = []
        static member inline Default(_f: array<'t> -> unit) : array<'t> = [||]

        static member inline Default(_f: Result<'t, ^U> -> unit) : Result<'t, ^U> =
            let inline call source =
                ((^U or Default): (static member Default: _ -> ^U) ((source, _f)))
            Error(call (fun _ -> ()))

        // Primitive types - these are F# language primitives, not System types
        static member inline Default(_f: byte -> unit) : byte = 0uy
        static member inline Default(_f: sbyte -> unit) : sbyte = 0y
        static member inline Default(_f: int16 -> unit) : int16 = 0s
        static member inline Default(_f: uint16 -> unit) : uint16 = 0us
        static member inline Default(_f: int -> unit) : int = 0
        static member inline Default(_f: uint -> unit) : uint = 0u
        static member inline Default(_f: int64 -> unit) : int64 = 0L
        static member inline Default(_f: uint64 -> unit) : uint64 = 0UL
        static member inline Default(_f: nativeint -> unit) : nativeint = 0n
        static member inline Default(_f: unativeint -> unit) : unativeint = 0un
        static member inline Default(_f: float32 -> unit) : float32 = 0.0f
        static member inline Default(_f: float -> unit) : float = 0.0
        static member inline Default(_f: decimal -> unit) : decimal = 0M
        static member inline Default(_f: char -> unit) : char = '\000'
        static member inline Default(_f: bool -> unit) : bool = false
        static member inline Default(_f: unit -> unit) : unit = ()

        static member inline Invoke< ^I
            when (^I or Default): (static member Default: (^I -> unit) -> ^I)>
            () : ^I =
            ((^I or Default): (static member Default: (^I -> unit) -> ^I) (fun _ -> ()))

    // ------------------------------------------------------------------------
    // DefaultWith - Provide fallback value via function
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type DefaultWith =
        static member inline DefaultWith(x: option<'t>, f: unit -> 't) : 't =
            match x with
            | Some v -> v
            | None -> f()

        static member inline DefaultWith(x: voption<'t>, f: unit -> 't) : 't =
            match x with
            | ValueSome v -> v
            | ValueNone -> f()

        static member inline DefaultWith(x: Result<'t, 'e>, f: 'e -> 't) : 't =
            match x with
            | Ok v -> v
            | Error e -> f e

        static member inline Invoke(action: _ -> 't, source: ^I) : 't =
            ((^I or DefaultWith): (static member DefaultWith: ^I * _ -> 't) (source, action))

    // ------------------------------------------------------------------------
    // IsEmpty - Check if collection/option is empty
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type IsEmpty =
        static member inline IsEmpty(x: ^t) : bool =
            (^t: (member Length: int) x) = 0
        static member inline IsEmpty(x: option<'t>) : bool = x.IsNone
        static member inline IsEmpty(x: voption<'t>) : bool = x.IsNone
        static member inline IsEmpty(x: list<'t>) : bool = x.IsEmpty
        static member inline IsEmpty(x: Result<'t, _>) : bool =
            match x with
            | Ok _ -> false
            | Error _ -> true

        static member inline Invoke< ^I
            when (^I or IsEmpty): (static member IsEmpty: ^I -> bool)>
            (source: ^I) : bool =
            ((^I or IsEmpty): (static member IsEmpty: ^I -> bool) source)

    // ------------------------------------------------------------------------
    // Length - Get length of collection
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Length =
        static member inline Length(x: option<'t>) : int =
            match x with Some _ -> 1 | None -> 0

        static member inline Length(x: voption<'t>) : int =
            match x with ValueSome _ -> 1 | ValueNone -> 0

        static member inline Length(x: Result<'t, _>) : int =
            match x with Ok _ -> 1 | Error _ -> 0

        static member inline Length(x: list<'t>) : int = x.Length
        static member inline Length(x: array<'t>) : int = x.Length
        static member inline Length(x: ^t) : int = (^t: (member Length: int) x)

        static member inline Invoke< ^I
            when (^I or Length): (static member Length: ^I -> int)>
            (source: ^I) : int =
            ((^I or Length): (static member Length: ^I -> int) source)

    // ------------------------------------------------------------------------
    // Item - Unchecked indexer
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Item =
        static member inline Item(x: array<'t>, key: int) : 't = x.[key]
        static member inline Item(x: list<'t>, key: int) : 't = x.Item key
        static member inline Item< ^I, ^k, ^v when ^I: (member Item: ^k -> ^v)>
            (x: ^I, key: ^k) : ^v = x.Item key

        static member inline Invoke< ^I, ^k, ^v
            when (^I or Item): (static member Item: ^I * ^k -> ^v)>
            (source: ^I, key: ^k) : ^v =
            ((^I or Item): (static member Item: ^I * ^k -> ^v) (source, key))

    // ------------------------------------------------------------------------
    // TryItem - Checked indexer returning voption
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type TryItem =
        static member inline TryItem(x: array<'t>, key: int) : voption<'t> =
            if key >= 0 && key < x.Length then ValueSome x.[key] else ValueNone

        static member inline TryItem(x: list<'t>, key: int) : voption<'t> =
            if key >= 0 && key < x.Length then ValueSome (x.Item key) else ValueNone

        static member inline Invoke< ^I, ^k, ^v
            when (^I or TryItem): (static member TryItem: ^I * ^k -> voption<^v>)>
            (source: ^I, key: ^k) : voption<^v> =
            ((^I or TryItem): (static member TryItem: ^I * ^k -> voption<^v>) (source, key))

    // ------------------------------------------------------------------------
    // Iterate - Apply function to each element
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Iterate =
        static member inline Iterate(x: option<'t>, f: 't -> unit) : unit =
            match x with Some v -> f v | None -> ()

        static member inline Iterate(x: voption<'t>, f: 't -> unit) : unit =
            match x with ValueSome v -> f v | ValueNone -> ()

        static member inline Iterate(x: Result<'t, _>, f: 't -> unit) : unit =
            match x with Ok v -> f v | Error _ -> ()

        static member inline Iterate(x: array<'t>, f: 't -> unit) : unit =
            let mutable i = 0
            while i < x.Length do
                f x.[i]
                i <- i + 1

        static member inline Iterate(x: list<'t>, f: 't -> unit) : unit =
            let rec loop lst =
                match lst with
                | [] -> ()
                | h :: t -> f h; loop t
            loop x

        static member inline Invoke< ^I, ^t
            when (^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit)>
            (source: ^I, action: ^t -> unit) : unit =
            ((^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit) (source, action))

    // ------------------------------------------------------------------------
    // Condition Capability - Safe encapsulation of mutable boolean state
    // Per "ByRef Resolved" design: encapsulate operations, not references
    // ------------------------------------------------------------------------

    /// A capability type that safely encapsulates a mutable boolean condition.
    /// This can be passed to closures and async code without byref restrictions.
    [<Struct; NoEquality; NoComparison>]
    type Condition =
        val mutable private value: bool
        new (initial: bool) = { value = initial }
        member this.Value with get() = this.value and set(v) = this.value <- v
        member this.Stop() = this.value <- false
        member this.Continue with get() = this.value

    // ------------------------------------------------------------------------
    // IterateWhile - Iterate while condition is true (capability-based)
    // Uses Condition capability instead of byref for closure safety
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type IterateWhile =
        static member inline IterateWhile(x: array<'t>, cond: Condition, f: 't -> unit) : Condition =
            let mutable i = 0
            let mutable c = cond
            while c.Continue && i < x.Length do
                f x.[i]
                i <- i + 1
            c

        static member inline IterateWhile(x: list<'t>, cond: Condition, f: 't -> unit) : Condition =
            let mutable current = x
            let mutable c = cond
            while c.Continue && not current.IsEmpty do
                f current.Head
                current <- current.Tail
            c

        static member inline IterateWhile(x: option<'t>, cond: Condition, f: 't -> unit) : Condition =
            let mutable c = cond
            if c.Continue then
                match x with Some v -> f v | None -> ()
            c

        static member inline IterateWhile(x: voption<'t>, cond: Condition, f: 't -> unit) : Condition =
            let mutable c = cond
            if c.Continue then
                match x with ValueSome v -> f v | ValueNone -> ()
            c

        static member inline Invoke< ^I, ^t
            when (^I or IterateWhile): (static member IterateWhile: ^I * Condition * (^t -> unit) -> Condition)>
            (source: ^I, cond: Condition, action: ^t -> unit) : Condition =
            ((^I or IterateWhile): (static member IterateWhile: ^I * Condition * (^t -> unit) -> Condition)
                (source, cond, action))

    // ------------------------------------------------------------------------
    // IterateIndexed - Iterate with index
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type IterateIndexed =
        static member inline Invoke(source: ^I, action: int -> 't -> unit) : unit =
            let mutable index = 0
            Iterate.Invoke<_, ^t>(source, fun v ->
                action index v
                index <- index + 1)

    // ------------------------------------------------------------------------
    // Map - Transform elements
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Map =
        static member inline Map(x: option<'t>, f: 't -> 'u) : option<'u> =
            match x with Some v -> Some (f v) | None -> None

        static member inline Map(x: voption<'t>, f: 't -> 'u) : voption<'u> =
            match x with ValueSome v -> ValueSome (f v) | ValueNone -> ValueNone

        static member inline Map(x: Result<'t, 'e>, f: 't -> 'u) : Result<'u, 'e> =
            match x with Ok v -> Ok (f v) | Error e -> Error e

        static member inline Map(x: array<'t>, f: 't -> 'u) : array<'u> =
            let result = Array.zeroCreate x.Length
            let mutable i = 0
            while i < x.Length do
                result.[i] <- f x.[i]
                i <- i + 1
            result

        static member inline Map(x: list<'t>, f: 't -> 'u) : list<'u> =
            let rec loop acc lst =
                match lst with
                | [] -> List.rev acc
                | h :: t -> loop (f h :: acc) t
            loop [] x

        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Map): (static member Map: ^I * (^t -> ^u) -> ^r)>
            (source: ^I, action: ^t -> ^u) : ^r =
            ((^I or Map): (static member Map: ^I * (^t -> ^u) -> ^r) (source, action))

    // ------------------------------------------------------------------------
    // MapIndexed - Transform with index
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type MapIndexed =
        static member inline Invoke(source: ^I, mapping: int -> 't -> 'u) =
            let mutable index = 0
            Map.Invoke(source, fun v ->
                let result = mapping index v
                index <- index + 1
                result)

    // ------------------------------------------------------------------------
    // Bind - Monadic bind (flatMap)
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Bind =
        static member inline Bind(x: option<'t>, f: 't -> option<'u>) : option<'u> =
            match x with Some v -> f v | None -> None

        static member inline Bind(x: voption<'t>, f: 't -> voption<'u>) : voption<'u> =
            match x with ValueSome v -> f v | ValueNone -> ValueNone

        static member inline Bind(x: Result<'t, 'e>, f: 't -> Result<'u, 'e>) : Result<'u, 'e> =
            match x with Ok v -> f v | Error e -> Error e

        static member inline Bind(x: list<'t>, f: 't -> list<'u>) : list<'u> =
            let rec loop acc lst =
                match lst with
                | [] -> List.rev acc
                | h :: t -> loop (List.rev (f h) @ acc) t
            loop [] x

        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Bind): (static member Bind: ^I * (^t -> ^u) -> ^r)>
            (fn: ^t -> ^u, source: ^I) : ^r =
            ((^I or Bind): (static member Bind: ^I * (^t -> ^u) -> ^r) (source, fn))

    // ------------------------------------------------------------------------
    // Fold - Reduce collection to single value
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Fold =
        static member inline Invoke(source: ^I, initial: ^acc, fn: ^acc -> ^t -> ^acc) : ^acc =
            let mutable state = initial
            Iterate.Invoke(source, fun v -> state <- fn state v)
            state

    // ------------------------------------------------------------------------
    // Exists - Check if any element satisfies predicate
    // Uses Condition capability for early termination
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Exists =
        static member inline Invoke(source: ^I, pred: 't -> bool) : bool =
            let mutable cond = Condition(true)
            let mutable found = false
            cond <- IterateWhile.Invoke(source, cond, fun v ->
                if pred v then
                    found <- true
                    cond.Stop())
            found

    // ------------------------------------------------------------------------
    // Forall - Check if all elements satisfy predicate
    // Uses Condition capability for early termination
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Forall =
        static member inline Invoke(source: ^I, pred: 't -> bool) : bool =
            let mutable cond = Condition(true)
            let mutable allMatch = true
            cond <- IterateWhile.Invoke(source, cond, fun v ->
                if not (pred v) then
                    allMatch <- false
                    cond.Stop())
            allMatch

    // ------------------------------------------------------------------------
    // Find - Find first element matching predicate
    // Uses Condition capability for early termination
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type Find =
        static member inline Invoke(source: ^I, pred: 't -> bool) : voption<'t> =
            let mutable cond = Condition(true)
            let mutable result = ValueNone
            cond <- IterateWhile.Invoke(source, cond, fun v ->
                if pred v then
                    result <- ValueSome v
                    cond.Stop())
            result

    // ------------------------------------------------------------------------
    // FindIndex - Find index of first matching element
    // Uses Condition capability for early termination
    // ------------------------------------------------------------------------

    [<AbstractClass; Sealed>]
    type FindIndex =
        static member inline Invoke(source: ^I, pred: 't -> bool) : voption<int> =
            let mutable cond = Condition(true)
            let mutable i = 0
            let mutable foundAt = ValueNone
            cond <- IterateWhile.Invoke(source, cond, fun v ->
                if pred v then
                    foundAt <- ValueSome i
                    cond.Stop()
                else
                    i <- i + 1)
            foundAt


// ============================================================================
// Public API - Clean functional interface
// ============================================================================

/// Public operations module - snake_case API matching original Fsil
[<AbstractClass; Sealed; AutoOpen>]
module Operations =

    /// Check if collection/option is empty
    let inline is_empty (source: ^I) : bool = Internal.IsEmpty.Invoke source

    /// Extract value from wrapper type (Result, Option with member Value, etc.)
    let inline value (source: ^I) : ^v = Internal.Value.Invoke source

    /// Get value or compute default via function
    let inline default_with (orElse: _ -> 't) (x: ^I) : 't =
        Internal.DefaultWith.Invoke(orElse, x)

    /// Unwrap value or fail with error message (like Rust's unwrap)
    let inline unwrap (x: ^I) : 't =
        Internal.DefaultWith.Invoke((fun _ -> panicwith (ofBytes "unwrap called on empty value"B)), x)

    /// Get the zero value for a numeric type
    let inline zero< ^a when ^a: (static member Zero: ^a)> : ^a =
        (^a : (static member Zero: ^a) ())

    /// Get the one value for a numeric type
    let inline one< ^a when ^a: (static member One: ^a)> : ^a =
        (^a : (static member One: ^a) ())

    /// Get None for option-like types
    let inline none< ^a when ^a: (static member None: ^a)> : ^a =
        (^a : (static member None: ^a) ())

    /// Wrap value in Some for option-like types
    let inline some< ^a, 'b when ^a: (static member Some: 'b -> ^a)> (v: 'b) : ^a =
        (^a : (static member Some: 'b -> ^a) v)

    /// Check if option-like type has a value
    let inline is_some< ^a when ^a: (member IsSome: bool)> (arg: ^a) : bool =
        (^a : (member IsSome: bool) arg)

    /// Check if option-like type is empty
    let inline is_none< ^a when ^a: (member IsNone: bool)> (arg: ^a) : bool =
        (^a : (member IsNone: bool) arg)

    /// Check if Result is Ok
    let inline is_ok< ^a when ^a: (member IsOk: bool)> (arg: ^a) : bool =
        (^a : (member IsOk: bool) arg)

    /// Check if all elements satisfy predicate
    let inline forall (pred: 't -> bool) (x: ^I) : bool =
        Internal.Forall.Invoke(x, pred)

    /// Check if any element satisfies predicate
    let inline exists (pred: 't -> bool) (x: ^I) : bool =
        Internal.Exists.Invoke(x, pred)

    /// Iterate from 0 to limit-1
    let inline iter_len (f: int -> unit) (limit: int) : unit =
        let mutable i = 0
        while i < limit do
            f i
            i <- i + 1

    /// Apply function to each element
    let inline iter (f: 't -> unit) (x: ^I) : unit =
        Internal.Iterate.Invoke(x, f)

    /// Iterate while condition is true (capability-based)
    /// Returns the updated Condition after iteration
    let inline iter_while (cond: Internal.Condition) (f: 't -> unit) (x: ^I) : Internal.Condition =
        Internal.IterateWhile.Invoke(x, cond, f)

    /// Create a new Condition capability for use with iter_while
    let inline condition (initial: bool) : Internal.Condition = Internal.Condition(initial)

    /// Apply function with index to each element
    let inline iteri (f: int -> 't -> unit) (x: ^I) : unit =
        Internal.IterateIndexed.Invoke(x, f)

    /// Fold collection to single value
    let inline fold (initial: ^acc) (f: ^acc -> 't -> ^acc) (x: ^I) : ^acc =
        Internal.Fold.Invoke(x, initial, f)

    /// Transform each element
    let inline map (f: 't -> 'u) (x: ^I) = Internal.Map.Invoke(x, f)

    /// Transform each element with index
    let inline mapi (f: int -> 't -> 'u) (x: ^I) = Internal.MapIndexed.Invoke(x, f)

    /// Monadic bind (flatMap)
    let inline bind (f: 't -> ^r) (x: ^I) = Internal.Bind.Invoke(f, x)

    /// Get length of collection
    let inline len (source: ^I) : int = Internal.Length.Invoke source

    /// Checked indexer returning voption
    let inline try_item (k: ^k) (source: ^I) = Internal.TryItem.Invoke(source, k)

    /// Alias for try_item
    let inline get (k: ^k) (source: ^I) = Internal.TryItem.Invoke(source, k)

    /// Unchecked indexer
    let inline item (k: ^k) (source: ^I) = Internal.Item.Invoke(source, k)

    /// Find first element matching predicate
    let inline find (pred: 't -> bool) (source: ^I) = Internal.Find.Invoke(source, pred)

    /// Find index of first matching element
    let inline position (pred: 't -> bool) (source: ^I) = Internal.FindIndex.Invoke(source, pred)

    /// First tuple element
    let inline _1 (source: ^I) = Internal.Item1.Invoke source

    /// Second tuple element
    let inline _2 (source: ^I) = Internal.Item2.Invoke source

    /// Third tuple element
    let inline _3 (source: ^I) = Internal.Item3.Invoke source

    /// Fourth tuple element
    let inline _4 (source: ^I) = Internal.Item4.Invoke source

    /// Get default value for type
    let inline default_of< ^t
        when (^t or Internal.Default): (static member Default: (^t -> unit) -> ^t)>
        () : ^t =
        Internal.Default.Invoke< ^t>()

    /// Catch exceptions and return Result
    let inline catch (fn: unit -> 't) : Result<'t, exn> =
        try Ok (fn())
        with e -> Error e
