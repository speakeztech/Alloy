namespace Alloy

/// Alloy Operators module.
///
/// FSharp.Core operators (+, -, *, /, >, <, =, etc.) work directly on primitives
/// and compile to IL opcodes without BCL dependency. No need to shadow them.
///
/// This module provides only operators that extend beyond FSharp.Core:
/// - Applicative/Functor operators for custom types
///
/// Note: The ($) operator is NOT defined here because it's used as an SRTP
/// dispatch mechanism in Console.fs and other places. Types define their own
/// static member ($) for type-class-style polymorphism.
[<AutoOpen>]
module Operators =

    /// Applicative apply operator.
    /// Requires static member Apply on the types.
    let inline (<*>) f x = ((^f or ^x) : (static member Apply: ^f * ^x -> 'r) (f, x))

    /// Functor map operator.
    /// Requires static member Map on the types.
    let inline (<!>) f x = ((^f or ^x) : (static member Map: ^f * ^x -> 'r) (f, x))
