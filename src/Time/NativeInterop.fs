#nowarn "9"

namespace Alloy.Time

open FSharp.NativeInterop

/// <summary>
/// Native interoperability layer for Alloy, providing P/Invoke-like functionality 
/// without System.Runtime.InteropServices dependencies in final compilation
/// </summary>
module NativeInterop =
    /// <summary>
    /// Calling convention for native functions
    /// </summary>
    type CallingConvention =
        | Cdecl = 0
        | StdCall = 1
        | ThisCall = 2
        | FastCall = 3
        | WinApi = 4

    /// <summary>
    /// Character set to use for string marshalling
    /// </summary>
    type CharSet =
        | Ansi = 0
        | Unicode = 1
        | Auto = 2

    /// <summary>
    /// Native function import definition - used for compiler-recognized external functions
    /// </summary>
    [<Struct; NoEquality; NoComparison>]
    type NativeImport<'TDelegate> = {
        /// <summary>The name of the library containing the function</summary>
        LibraryName: string
        /// <summary>The name of the function to import</summary>
        FunctionName: string
        /// <summary>Optional calling convention (defaults to Cdecl)</summary>
        CallingConvention: CallingConvention
        /// <summary>Optional character set for string marshalling (defaults to Ansi)</summary>
        CharSet: CharSet
        /// <summary>Set to true to suppress the standard error handling</summary>
        SupressErrorHandling: bool
    }

    /// <summary>
    /// Internal operations for platform detection - used for conditional compilation
    /// </summary>
    [<RequireQualifiedAccess>]
    module Platform =
        /// <summary>
        /// Platforms that Alloy can target
        /// </summary>
        type TargetPlatform =
            | Windows = 0
            | Linux = 1
            | MacOS = 2
            | Unknown = 3
        
        /// <summary>
        /// Gets the current platform at compile time
        /// </summary>
        let inline getCurrentPlatform() =
            #if WINDOWS
            TargetPlatform.Windows
            #elif LINUX
            TargetPlatform.Linux
            #elif MACOS
            TargetPlatform.MacOS
            #else
            TargetPlatform.Unknown
            #endif
            
    /// <summary>
    /// Creates a native function import definition
    /// </summary>
    let inline dllImport<'TDelegate> libraryName functionName =
        {
            LibraryName = libraryName
            FunctionName = functionName
            CallingConvention = CallingConvention.Cdecl
            CharSet = CharSet.Ansi
            SupressErrorHandling = false
        }

    /// <summary>
    /// Invokes a native function with no arguments - compiler recognized pattern
    /// </summary>
    let inline invokeFunc0<'TResult> (import: NativeImport<unit -> 'TResult>) : 'TResult =
        // Placeholder implementation - the compiler will replace this with direct LLVM calls
        match import.LibraryName, import.FunctionName with
        | _, _ -> Unchecked.defaultof<'TResult>

    /// <summary>
    /// Invokes a native function with one argument - compiler recognized pattern
    /// </summary>
    let inline invokeFunc1<'T1, 'TResult> (import: NativeImport<'T1 -> 'TResult>) (arg1: 'T1) : 'TResult =
        // Placeholder implementation - the compiler will replace this with direct LLVM calls
        match import.LibraryName, import.FunctionName with
        | _, _ -> Unchecked.defaultof<'TResult>

    /// <summary>
    /// Invokes a native function with two arguments - compiler recognized pattern
    /// </summary>
    let inline invokeFunc2<'T1, 'T2, 'TResult> 
        (import: NativeImport<'T1 -> 'T2 -> 'TResult>) (arg1: 'T1) (arg2: 'T2) : 'TResult =
        // Placeholder implementation - the compiler will replace this with direct LLVM calls
        match import.LibraryName, import.FunctionName with
        | _, _ -> Unchecked.defaultof<'TResult>
        
    /// <summary>
    /// Invokes a native function with three arguments - compiler recognized pattern
    /// </summary>
    let inline invokeFunc3<'T1, 'T2, 'T3, 'TResult> 
        (import: NativeImport<'T1 -> 'T2 -> 'T3 -> 'TResult>) (arg1: 'T1) (arg2: 'T2) (arg3: 'T3) : 'TResult =
        // Placeholder implementation - the compiler will replace this with direct LLVM calls
        match import.LibraryName, import.FunctionName with
        | _, _ -> Unchecked.defaultof<'TResult>