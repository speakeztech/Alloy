#nowarn "77"
#nowarn "9"
namespace Alloy

[<AutoOpen>]
module Core =
    
    // ===================================
    // Zero-allocation option type using F# voption
    // ===================================
    
    /// <summary>
    /// Zero-allocation option type that shadows F#'s option with voption
    /// </summary>
    type option<'T> = voption<'T>
    
    /// <summary>
    /// Creates an option value containing the given value.
    /// </summary>
    let inline Some (value: 'T) : 'T option = ValueSome value
    
    /// <summary>
    /// Represents an option value with no value.
    /// </summary>
    let inline None<'T> : 'T option = ValueNone
    
    let inline (|Some|None|) (opt: 'T option) =
        match opt with
        | ValueSome v -> Some v
        | ValueNone -> None

    /// <summary>
    /// Zero-dependency disposable interface for resource management
    /// </summary>
    type IDisposable =
        abstract Dispose : unit -> unit

    /// <summary>
    /// Resource management helper functions
    /// </summary>
    module Resource =
        /// <summary>
        /// Use pattern implementation for disposable resources
        /// </summary>
        let inline use' (resource: ^T when ^T : (member Dispose : unit -> unit)) (action: ^T -> 'U) : 'U =
            try
                action resource
            finally
                let dispose = (^T : (member Dispose : unit -> unit) resource)
                ()
    
    // ===================================
    // Basic collection operations
    // ===================================
    
    /// <summary>
    /// Applies a function to each element of an array.
    /// </summary>
    let inline iter (f: 'T -> unit) (array: 'T[]) =
        for i = 0 to array.Length - 1 do
            f array[i]

    /// <summary>
    /// Applies a function with the index to each element of an array.
    /// </summary>
    let inline iteri (f: int -> 'T -> unit) (array: 'T[]) =
        for i = 0 to array.Length - 1 do
            f i array[i]

    /// <summary>
    /// Maps a function over an array, producing a new array.
    /// </summary>
    let inline map (f: 'T -> 'U) (array: 'T[]) =
        Array.init array.Length (fun i -> f array[i])

    /// <summary>
    /// Maps a function with index over an array, producing a new array.
    /// </summary>
    let inline mapi (f: int -> 'T -> 'U) (array: 'T[]) =
        Array.init array.Length (fun i -> f i array[i])

    /// <summary>
    /// Gets the length of an array.
    /// </summary>
    let inline len (array: 'T[]) = array.Length

    // ===================================
    // Zero/One/Default operations
    // ===================================
    
    /// <summary>
    /// Gets the zero value for a type.
    /// </summary>
    let inline zero<'T when 'T: (static member Zero: 'T)> = 
        (^T: (static member Zero: 'T) ())

    /// <summary>
    /// Gets the one/unit value for a type.
    /// </summary>
    let inline one<'T when 'T: (static member One: 'T)> = 
        (^T: (static member One: 'T) ())

    /// <summary>
    /// Gets the default value for a type.
    /// </summary>
    let inline default_value<'T> = Unchecked.defaultof<'T>

    /// <summary>
    /// Uses a fallback function if a value is None.
    /// </summary>
    let inline default_with (f: unit -> 'T) (x: 'T option) =
        match x with
        | ValueSome value -> value
        | ValueNone -> f ()

    /// <summary>
    /// Converts a value to string using static resolution.
    /// </summary>
    let inline string< ^T when ^T : (member ToString : unit -> string)> (x: ^T) =
        (^T : (member ToString : unit -> string) x)

    /// <summary>
    /// Print a value to stdout.
    /// </summary>
    let inline print (x: 'T) = printf "%A" x

    // ===================================
    // Option functions
    // ===================================

    /// <summary>
    /// Checks if an option contains a value.
    /// </summary>
    let inline is_some (x: 'T option) =
        match x with
        | ValueSome _ -> true
        | ValueNone -> false

    /// <summary>
    /// Checks if an option is None.
    /// </summary>
    let inline is_none (x: 'T option) =
        match x with
        | ValueSome _ -> false
        | ValueNone -> true

    /// <summary>
    /// Gets the value from an option, throws if None.
    /// </summary>
    let inline value (x: 'T option) =
        match x with
        | ValueSome v -> v
        | ValueNone -> failwith "Option was None"

    /// <summary>
    /// Creates a None option.
    /// </summary>
    let inline none<'T> : 'T option = ValueNone

    /// <summary>
    /// Wraps a value in Some.
    /// </summary>
    let inline some (x: 'T) = ValueSome x

    // ===================================
    // Collection operations with explicit implementations
    // ===================================

    /// <summary>
    /// Fold implementation for arrays.
    /// </summary>
    let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) =
        let mutable acc = state
        for i = 0 to array.Length - 1 do
            acc <- folder acc array[i]
        acc

    /// <summary>
    /// Filter implementation for arrays.
    /// </summary>
    let inline filter (predicate: 'T -> bool) (source: 'T[]) =
        let mutable count = 0
        for i = 0 to source.Length - 1 do
            if predicate source[i] then
                count <- count + 1
        
        let result = Array.zeroCreate count
        
        let mutable j = 0
        for i = 0 to source.Length - 1 do
            if predicate source[i] then
                result[j] <- source[i]
                j <- j + 1
        
        result

    /// <summary>
    /// Choose implementation for arrays.
    /// </summary>
    let inline choose (chooser: 'T -> 'U option) (source: 'T[]) =
        let mutable count = 0
        for i = 0 to source.Length - 1 do
            match chooser source[i] with
            | ValueSome _ -> count <- count + 1
            | ValueNone -> ()
        
        let result = Array.zeroCreate count
        
        let mutable j = 0
        for i = 0 to source.Length - 1 do
            match chooser source[i] with
            | ValueSome value -> 
                result[j] <- value
                j <- j + 1
            | ValueNone -> ()
        
        result

    /// <summary>
    /// Find an element in an array that matches a predicate.
    /// </summary>
    let inline find (predicate: 'T -> bool) (array: 'T[]) =
        let mutable found = false
        let mutable result = Unchecked.defaultof<'T>
        let mutable i = 0
        while not found && i < array.Length do
            if predicate array[i] then
                found <- true
                result <- array[i]
            i <- i + 1
        
        if found then result
        else failwith "No element found matching the predicate"

    /// <summary>
    /// Try to find an element in an array that matches a predicate.
    /// </summary>
    let inline tryFind (predicate: 'T -> bool) (array: 'T[]) =
        let mutable found = false
        let mutable result = Unchecked.defaultof<'T>
        let mutable i = 0
        while not found && i < array.Length do
            if predicate array[i] then
                found <- true
                result <- array[i]
            i <- i + 1
        
        if found then ValueSome result
        else ValueNone

    /// <summary>
    /// Generic equality check.
    /// </summary>
    let inline equals (a: 'T) (b: 'T) : bool = a = b

    /// <summary>
    /// Generic inequality check.
    /// </summary>
    let inline not_equals (a: 'T) (b: 'T) : bool = a <> b

    // ===================================
    // Result type and operations
    // ===================================

    /// <summary>
    /// Result type for error handling without exceptions
    /// </summary>
    [<Struct>]
    type Result<'T, 'Error> =
        | Ok of ok: 'T
        | Error of error: 'Error

    /// <summary>
    /// Creates an Ok result.
    /// </summary>
    let inline Ok (value: 'T) : Result<'T, 'Error> = Ok value

    /// <summary>
    /// Creates an Error result.
    /// </summary>
    let inline Error (error: 'Error) : Result<'T, 'Error> = Error error

    /// <summary>
    /// Result module with operations
    /// </summary>
    [<RequireQualifiedAccess>]
    module Result =
        
        /// <summary>
        /// Maps a function over the Ok value of a result.
        /// </summary>
        let inline map (f: 'T -> 'U) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> Ok (f value)
            | Error e -> Error e

        /// <summary>
        /// Maps a function over the Error value of a result.
        /// </summary>
        let inline mapError (f: 'Error -> 'Error2) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> Ok value
            | Error e -> Error (f e)

        /// <summary>
        /// Gets the value from a result or returns a default.
        /// </summary>
        let inline defaultValue (def: 'T) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> value
            | Error _ -> def

        /// <summary>
        /// Gets the value from a result or computes a default.
        /// </summary>
        let inline defaultWith (f: 'Error -> 'T) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> value
            | Error e -> f e

        /// <summary>
        /// Binds a function over a result.
        /// </summary>
        let inline bind (f: 'T -> Result<'U, 'Error>) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> f value
            | Error e -> Error e

        /// <summary>
        /// Applies a Result function to a Result value.
        /// </summary>
        let inline apply (fResult: Result< 'T -> 'U, 'Error>) (xResult: Result<'T, 'Error>) =
            match fResult, xResult with
            | Ok f, Ok x -> Ok (f x)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        /// <summary>
        /// Maps both Ok and Error values.
        /// </summary>
        let inline bimap (mapOk: 'T -> 'U) (mapError: 'Error -> 'Error2) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> Ok (mapOk value)
            | Error e -> Error (mapError e)

        /// <summary>
        /// Folds a result into a single value.
        /// </summary>
        let inline fold (onOk: 'T -> 'State) (onError: 'Error -> 'State) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> onOk value
            | Error e -> onError e

        /// <summary>
        /// Checks if a result is Ok.
        /// </summary>
        let inline isOk (result: Result<'T, 'Error>) =
            match result with
            | Ok _ -> true
            | Error _ -> false

        /// <summary>
        /// Checks if a result is Error.
        /// </summary>
        let inline isError (result: Result<'T, 'Error>) =
            match result with
            | Ok _ -> false
            | Error _ -> true

        /// <summary>
        /// Gets the Ok value from a result, throws if Error.
        /// </summary>
        let inline get (result: Result<'T, 'Error>) =
            match result with
            | Ok v -> v
            | Error _ -> failwith "Result was Error"

        /// <summary>
        /// Gets the Error value from a result, throws if Ok.
        /// </summary>
        let inline getError (result: Result<'T, 'Error>) =
            match result with
            | Ok _ -> failwith "Result was Ok"
            | Error e -> e

        /// <summary>
        /// Converts a Result to an option, discarding the error.
        /// </summary>
        let inline toOption (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> ValueSome value
            | Error _ -> ValueNone

        /// <summary>
        /// Converts an option to a Result with a given error.
        /// </summary>
        let inline ofOption (error: 'Error) (opt: 'T option) =
            match opt with
            | ValueSome value -> Ok value
            | ValueNone -> Error error

        /// <summary>
        /// Flattens a nested Result.
        /// </summary>
        let inline flatten (result: Result<Result<'T, 'Error>, 'Error>) =
            match result with
            | Ok innerResult -> innerResult
            | Error e -> Error e

        /// <summary>
        /// Converts a boolean to a Result.
        /// </summary>
        let inline ofBool (error: 'Error) (value: 'T) (condition: bool) =
            if condition then Ok value else Error error

        /// <summary>
        /// Tries to create a Result from a function that might throw.
        /// </summary>
        let inline tryWith (f: unit -> 'T) : Result<'T, exn> =
            try
                Ok (f())
            with
            | ex -> Error ex

        /// <summary>
        /// Sequences a list of Results into a Result of list.
        /// </summary>
        let inline sequence (results: Result<'T, 'Error> list) : Result<'T list, 'Error> =
            let rec loop acc = function
                | [] -> Ok (List.rev acc)
                | Error e :: _ -> Error e
                | Ok x :: xs -> loop (x :: acc) xs
            loop [] results

        /// <summary>
        /// Traverses a list with a Result-returning function.
        /// </summary>
        let inline traverse (f: 'T -> Result<'U, 'Error>) (list: 'T list) : Result<'U list, 'Error> =
            let rec loop acc = function
                | [] -> Ok (List.rev acc)
                | x :: xs ->
                    match f x with
                    | Ok y -> loop (y :: acc) xs
                    | Error e -> Error e
            loop [] list

        /// <summary>
        /// Combines two Results into a tuple if both are Ok.
        /// </summary>
        let inline zip (result1: Result<'T1, 'Error>) (result2: Result<'T2, 'Error>) =
            match result1, result2 with
            | Ok x, Ok y -> Ok (x, y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        /// <summary>
        /// Returns the first Ok result or the last Error.
        /// </summary>
        let inline orElse (result2: Result<'T, 'Error>) (result1: Result<'T, 'Error>) =
            match result1 with
            | Ok _ -> result1
            | Error _ -> result2

        /// <summary>
        /// Returns the first Ok result from a list of Results.
        /// </summary>
        let inline choice (results: Result<'T, 'Error> list) =
            List.fold orElse (Error (failwith "Empty choice list")) results

        /// <summary>
        /// Iterates over an Ok value, does nothing for Error.
        /// </summary>
        let inline iter (f: 'T -> unit) (result: Result<'T, 'Error>) =
            match result with
            | Ok value -> f value
            | Error _ -> ()

        /// <summary>
        /// Checks if a Result contains a specific Ok value.
        /// </summary>
        let inline contains (value: 'T) (result: Result<'T, 'Error>) =
            match result with
            | Ok v -> v = value
            | Error _ -> false

        /// <summary>
        /// Counts Ok results (returns 1 for Ok, 0 for Error).
        /// </summary>
        let inline count (result: Result<'T, 'Error>) =
            match result with
            | Ok _ -> 1
            | Error _ -> 0