#nowarn "9"
#nowarn "1182"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Text.UTF8

/// Low-level console operations that map to native I/O
module Console =
    
    /// Standard file descriptors
    [<Literal>]
    let STDIN_FILENO = 0
    
    [<Literal>]
    let STDOUT_FILENO = 1
    
    [<Literal>]
    let STDERR_FILENO = 2
    
    /// Native write operation - maps to write(fd, buffer, count) syscall
    [<CompiledName("ConsoleWriteBytes")>]
    let inline writeBytes (fd: int) (buffer: nativeptr<byte>) (count: int) : int =
        count // Placeholder - compiler will replace with LLVM intrinsic
    
    /// Native read operation - maps to read(fd, buffer, count) syscall  
    [<CompiledName("ConsoleReadBytes")>]
    let inline readBytes (fd: int) (buffer: nativeptr<byte>) (maxCount: int) : int =
        0 // Placeholder - compiler will replace with LLVM intrinsic
    
    /// Writes a null-terminated string to stdout
    let inline writeString (str: nativeptr<byte>) : int =
        let mutable len = 0
        while NativePtr.get str len <> 0uy do
            len <- len + 1
        writeBytes STDOUT_FILENO str len
    
    /// Writes a string with known length to specified fd
    let inline writeStringToFd (fd: int) (str: nativeptr<byte>) (len: int) : int =
        writeBytes fd str len
    
    /// Writes a single character to specified fd
    let inline writeCharTo (fd: int) (ch: byte) : int =
        let mutable b = ch
        let ptr = NativePtr.ofVoidPtr<byte>(NativePtr.toVoidPtr &&b)
        writeBytes fd ptr 1
    
    /// Writes a single character to stdout
    let inline writeChar (ch: byte) : int =
        writeCharTo STDOUT_FILENO ch
    
    /// Writes a newline to stdout
    let inline writeNewLine() : int =
        writeChar 10uy // '\n'
    
    /// Writes a newline to specified fd
    let inline writeNewLineTo (fd: int) : int =
        writeCharTo fd 10uy
    
    /// Reads a line from stdin into a buffer
    let inline readLine (buffer: nativeptr<byte>) (maxLength: int) : int =
        let mutable count = 0
        let mutable ch = 0uy
        
        while count < (maxLength - 1) do
            let mutable b = 0uy
            let ptr = NativePtr.ofVoidPtr<byte>(NativePtr.toVoidPtr &&b)
            let bytesRead = readBytes STDIN_FILENO ptr 1
            
            if bytesRead <= 0 then
                count <- maxLength
            else
                ch <- b
                if ch = 10uy then
                    count <- maxLength
                else
                    NativePtr.set buffer count ch
                    count <- count + 1
        
        NativePtr.set buffer count 0uy
        count
    
    // ===================================
    // Core output functions (shadows System.Console)
    // ===================================
    
    /// Writes a string to the console
    let Write (message: string) =
        let buffer = NativePtr.stackalloc<byte> 4096
        let len = stringToBytes message buffer 4096
        writeBytes STDOUT_FILENO buffer len |> ignore
    
    /// Writes a line to the console
    let WriteLine (message: string) =
        Write(message)
        writeNewLine() |> ignore

    /// Writes an empty line to the console  
    let writeLine() =
        writeNewLine() |> ignore
    
    /// Reads a line from the console
    let ReadLine() : string =
        let buffer = NativePtr.stackalloc<byte> 1024
        let length = readLine buffer 1024
        bytesToString buffer length

    // ===================================
    // Error output functions
    // ===================================
    
    /// Writes a string to the error console
    let WriteError (message: string) =
        let buffer = NativePtr.stackalloc<byte> 4096
        let len = stringToBytes message buffer 4096
        writeBytes STDERR_FILENO buffer len |> ignore

    /// Writes a line to the error console
    let WriteErrorLine (message: string) =
        WriteError(message)
        writeNewLineTo STDERR_FILENO |> ignore
    
    /// Writes an empty line to the error console
    let writeErrorLine() =
        writeNewLineTo STDERR_FILENO |> ignore
    
    // ===================================
    // Reads into buffers using SRTP
    // ===================================
    
    /// Reads user input into any buffer type with Pointer and Length properties
    let inline readInto< ^T when ^T : (member Pointer : nativeptr<byte>) and ^T : (member Length : int)> 
            (buffer: ^T) : Result<int, string> =
        let pointer = (^T : (member Pointer : nativeptr<byte>) buffer)
        let bufferLength = (^T : (member Length : int) buffer)
        let length = readLine pointer bufferLength
        if length >= 0 then
            Ok length
        else
            Error "Read error"
    
    /// Simple formatting for the buffer
    module private Format =
        /// Helper to format an integer to a string buffer
        let inline formatInt (value: int) (buffer: nativeptr<byte>) (maxLength: int) : int =
            if value = 0 then
                if maxLength > 0 then
                    NativePtr.set buffer 0 (byte '0')
                    NativePtr.set buffer 1 0uy
                    1
                else
                    0
            else
                let isNegative = value < 0
                let mutable v = if isNegative then -value else value
                
                // Count digits
                let mutable digitCount = 0
                let mutable temp = v
                while temp > 0 do
                    digitCount <- digitCount + 1
                    temp <- temp / 10
                
                // Ensure buffer is large enough (including sign and null terminator)
                let requiredSize = if isNegative then digitCount + 1 else digitCount
                if requiredSize >= maxLength then
                    0 // Buffer too small
                else
                    // Add negative sign if needed
                    let mutable pos = 0
                    if isNegative then
                        NativePtr.set buffer pos (byte '-')
                        pos <- pos + 1
                    
                    // Convert digits (in reverse)
                    let digitsStart = pos
                    pos <- pos + digitCount
                    temp <- pos
                    
                    while v > 0 do
                        temp <- temp - 1
                        NativePtr.set buffer temp (byte (int '0' + (v % 10)))
                        v <- v / 10
                    
                    // Null terminate
                    NativePtr.set buffer pos 0uy
                    pos
    
    // ===================================
    // Printf-style functions with custom implementation
    // ===================================
    
    /// Printf-style write to console
    let printf (format: string) (arg: 'T) =
        let buffer = NativePtr.stackalloc<byte> 4096
        let tempBuffer = NativePtr.stackalloc<byte> 1024
        
        // Create formatted string
        let formatted =
            match box arg with
            | :? int as i -> 
                let pos = Format.formatInt i tempBuffer 1024
                if pos > 0 then
                    Text.String.replace format "{0}" (bytesToString tempBuffer pos)
                else
                    format
            | :? string as s ->
                Text.String.replace format "{0}" s
            | :? bool as b ->
                Text.String.replace format "{0}" (if b then "true" else "false")
            | _ ->
                // Fallback
                format
                
        // Write the formatted string
        let len = stringToBytes formatted buffer 4096
        writeBytes STDOUT_FILENO buffer len |> ignore
    
    /// Printf-style write line to console
    let printfn (format: string) (arg: 'T) = 
        printf format arg
        writeNewLine() |> ignore
    
    /// Printf-style write to error console
    let eprintf (format: string) (arg: 'T) = 
        let buffer = NativePtr.stackalloc<byte> 4096
        let tempBuffer = NativePtr.stackalloc<byte> 1024
        
        // Create formatted string
        let formatted =
            match box arg with
            | :? int as i -> 
                let pos = Format.formatInt i tempBuffer 1024
                if pos > 0 then
                    Text.String.replace format "{0}" (bytesToString tempBuffer pos)
                else
                    format
            | :? string as s ->
                Text.String.replace format "{0}" s
            | :? bool as b ->
                Text.String.replace format "{0}" (if b then "true" else "false")
            | _ ->
                // Fallback
                format
                
        // Write the formatted string
        let len = stringToBytes formatted buffer 4096
        writeBytes STDERR_FILENO buffer len |> ignore
    
    /// Printf-style write line to error console
    let eprintfn (format: string) (arg: 'T) = 
        eprintf format arg
        writeNewLineTo STDERR_FILENO |> ignore
    
    /// Sprintf replacement for your format needs - simplistic single-argument version
    let sprintf (format: string) (arg: 'T) : string =
        match box arg with
        | null -> format
        | :? string as s -> Text.String.replace format "{0}" s
        | :? int as i -> 
            let s = 
                if i = 0 then "0"
                else
                    let isNeg = i < 0
                    let digits = 
                        let rec countDigits n count =
                            if n = 0 then count
                            else countDigits (n / 10) (count + 1)
                        countDigits (abs i) 0
                    
                    let len = if isNeg then digits + 1 else digits
                    let chars = Array.zeroCreate<char> len
                    
                    let rec writeDigits n pos =
                        if n = 0 then ()
                        else
                            let digit = abs (n % 10)
                            chars[pos] <- char (digit + int '0')
                            writeDigits (n / 10) (pos - 1)
                    
                    let startPos = if isNeg then digits else digits - 1
                    writeDigits i startPos
                    
                    if isNeg then chars[0] <- '-'
                    
                    new string(chars)
            
            Text.String.replace format "{0}" s
        | :? bool as b -> Text.String.replace format "{0}" (if b then "true" else "false")
        | _ -> Text.String.replace format "{0}" "[unsupported type]"
    
    // ===================================
    // Helper functions
    // ===================================
    
    /// Displays a prompt to the user
    let inline Prompt (message: string) =
        Write message