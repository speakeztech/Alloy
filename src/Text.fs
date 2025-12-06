#nowarn "9"
namespace Alloy

open FSharp.NativeInterop
open Alloy.Memory

/// <summary>
/// Zero-allocation text processing
/// </summary>
module Text =
    
    /// <summary>
    /// Internal text manipulation helpers
    /// </summary>
    [<AutoOpen>]
    module private TextOperations =
        let inline increment (x: int) = x + 1
        let inline add (x: int) (y: int) = x + y
        let inline isNull (str: string) = obj.ReferenceEquals(str, null)
    
    /// <summary>
    /// UTF-8 encoding operations
    /// </summary>
    module UTF8 =
        /// <summary>
        /// Converts a string to UTF-8 bytes
        /// </summary>
        let inline stringToBytes (s: string) (buffer: nativeptr<byte>) (maxLength: int) : int =
            if isNull s then
                NativePtr.set buffer 0 0uy
                0
            else
                let mutable byteCount = 0
                let mutable charIndex = 0
                
                while charIndex < s.Length && byteCount < maxLength - 1 do
                    let c = int s[charIndex]  // Convert char to int for bit operations
                    
                    // UTF-8 encoding
                    if c < 0x80 then
                        // ASCII character (0-127)
                        NativePtr.set buffer byteCount (byte c)
                        byteCount <- byteCount + 1
                    elif c < 0x800 then
                        // 2-byte sequence
                        if byteCount + 1 < maxLength - 1 then
                            NativePtr.set buffer byteCount (0xC0uy ||| (byte (c >>> 6)))
                            NativePtr.set buffer (byteCount + 1) (0x80uy ||| (byte (c &&& 0x3F)))
                            byteCount <- byteCount + 2
                    else
                        // 3-byte sequence
                        if byteCount + 2 < maxLength - 1 then
                            NativePtr.set buffer byteCount (0xE0uy ||| (byte (c >>> 12)))
                            NativePtr.set buffer (byteCount + 1) (0x80uy ||| (byte ((c >>> 6) &&& 0x3F)))
                            NativePtr.set buffer (byteCount + 2) (0x80uy ||| (byte (c &&& 0x3F)))
                            byteCount <- byteCount + 3
                            
                    charIndex <- charIndex + 1
                
                // Null-terminate the string
                NativePtr.set buffer byteCount 0uy
                byteCount
                
        /// <summary>
        /// Converts UTF-8 bytes to a string
        /// </summary>
        let inline bytesToString (buffer: nativeptr<byte>) (length: int) : string =
            if length <= 0 then
                ""
            else
                // First pass: count chars
                let mutable charCount = 0
                let mutable i = 0
                
                while i < length && NativePtr.get buffer i <> 0uy do
                    let b = NativePtr.get buffer i
                    
                    if b < 0x80uy then
                        // ASCII
                        charCount <- charCount + 1
                        i <- i + 1
                    elif b < 0xE0uy then
                        // 2-byte sequence
                        charCount <- charCount + 1
                        i <- i + 2
                    else
                        // 3-byte sequence
                        charCount <- charCount + 1
                        i <- i + 3
                
                // Build the string
                let chars = Array.zeroCreate<char> charCount
                i <- 0
                let mutable charIndex = 0
                
                while i < length && NativePtr.get buffer i <> 0uy && charIndex < charCount do
                    let b = NativePtr.get buffer i
                    
                    if b < 0x80uy then
                        // ASCII
                        chars[charIndex] <- char b
                        charIndex <- charIndex + 1
                        i <- i + 1
                    elif b < 0xE0uy && i + 1 < length then
                        // 2-byte sequence
                        let b2 = NativePtr.get buffer (i + 1)
                        let c = ((int (b &&& 0x1Fuy)) <<< 6) ||| (int (b2 &&& 0x3Fuy))
                        chars[charIndex] <- char c
                        charIndex <- charIndex + 1
                        i <- i + 2
                    elif i + 2 < length then
                        // 3-byte sequence
                        let b2 = NativePtr.get buffer (i + 1)
                        let b3 = NativePtr.get buffer (i + 2)
                        let c = ((int (b &&& 0x0Fuy)) <<< 12) ||| 
                                ((int (b2 &&& 0x3Fuy)) <<< 6) ||| 
                                (int (b3 &&& 0x3Fuy))
                        chars[charIndex] <- char c
                        charIndex <- charIndex + 1
                        i <- i + 3
                    else
                        // Invalid sequence
                        i <- i + 1
                
                new string(chars)
                
        /// <summary>
        /// Converts a span of bytes to a string
        /// </summary>
        let inline spanToString (span: ReadOnlySpan<byte>) : string =
            // Create a temporary buffer on the stack
            let mutable tempBuffer = stackBuffer<byte> 1024
            
            // Copy the span to the buffer
            let length = min span.Length tempBuffer.Length
            let mutable tempSpan = tempBuffer.AsSpan()
            for i = 0 to length - 1 do
                tempSpan[i] <- span[i]
                
            // Convert the buffer to a string
            bytesToString tempBuffer.Pointer length
    
    /// <summary>
    /// Common string operations implemented with zero allocations where possible
    /// </summary>
    [<RequireQualifiedAccess>]
    module String =
        /// <summary>
        /// Checks if a string is null or empty
        /// </summary>
        let inline isEmpty (s: string) : bool =
            isNull s || s.Length = 0
            
        /// <summary>
        /// Checks if a string is not null and not empty
        /// </summary>
        let inline isNotEmpty (s: string) : bool =
            not (isNull s) && s.Length > 0
            
        /// <summary>
        /// Gets the length of a string
        /// </summary>
        let inline length (s: string) : int =
            if isNull s then 0 else s.Length
            
        /// <summary>
        /// Safely gets the character at the specified position
        /// </summary>
        let inline tryCharAt (index: int) (s: string) (result: byref<char>) : bool =
            if isNull s || index < 0 || index >= s.Length then
                false
            else
                result <- s[index]
                true
                
        /// <summary>
        /// Checks if a string starts with a prefix
        /// </summary>
        let inline startsWith (prefix: string) (s: string) : bool =
            if isNull s || isNull prefix then false
            elif prefix.Length > s.Length then false
            else
                let mutable i = 0
                let mutable matches = true
                while i < prefix.Length && matches do
                    if s[i] <> prefix[i] then matches <- false
                    i <- increment i
                matches
                
        /// <summary>
        /// Checks if a string ends with a suffix
        /// </summary>
        let inline endsWith (suffix: string) (s: string) : bool =
            if isNull s || isNull suffix then false
            elif suffix.Length > s.Length then false
            else
                let offset = s.Length - suffix.Length
                let mutable i = 0
                let mutable matches = true
                while i < suffix.Length && matches do
                    if s[offset + i] <> suffix[i] then matches <- false
                    i <- increment i
                matches
                
        /// <summary>
        /// Concatenates two strings
        /// </summary>
        let inline concat (s1: string) (s2: string) : string =
            let s1Safe = if isNull s1 then "" else s1
            let s2Safe = if isNull s2 then "" else s2
            
            let len1 = s1Safe.Length
            let len2 = s2Safe.Length
            let totalLen = add len1 len2
            
            let result = Array.zeroCreate<char> totalLen
            
            for i = 0 to len1 - 1 do
                result[i] <- s1Safe[i]
            
            for i = 0 to len2 - 1 do
                result[add i len1] <- s2Safe[i]
                
            new string(result)
            
        /// <summary>
        /// Replaces all occurrences of a substring in a string
        /// </summary>
        let inline replace (original: string) (oldValue: string) (newValue: string) : string =
            if isNull original then ""
            elif isNull oldValue || oldValue.Length = 0 then original
            else
                // First count occurrences to determine final size
                let mutable count = 0
                let mutable pos = 0
                
                while pos <= original.Length - oldValue.Length do
                    let mutable found = true
                    for i = 0 to oldValue.Length - 1 do
                        if original[pos + i] <> oldValue[i] then
                            found <- false
                    
                    if found then
                        count <- count + 1
                        pos <- pos + oldValue.Length
                    else
                        pos <- pos + 1
                
                if count = 0 then
                    original
                else
                    let newValueSafe = if isNull newValue then "" else newValue
                    let resultLength = original.Length + count * (newValueSafe.Length - oldValue.Length)
                    let result = Array.zeroCreate<char> resultLength
                    
                    let mutable srcPos = 0
                    let mutable dstPos = 0
                    
                    while srcPos < original.Length do
                        if srcPos <= original.Length - oldValue.Length then
                            let mutable found = true
                            for i = 0 to oldValue.Length - 1 do
                                if original[srcPos + i] <> oldValue[i] then
                                    found <- false
                            
                            if found then
                                // Copy the replacement string
                                for i = 0 to newValueSafe.Length - 1 do
                                    result[dstPos + i] <- newValueSafe[i]
                                srcPos <- srcPos + oldValue.Length
                                dstPos <- dstPos + newValueSafe.Length
                            else
                                // Copy the original character
                                result[dstPos] <- original[srcPos]
                                srcPos <- srcPos + 1
                                dstPos <- dstPos + 1
                        else
                            // Copy the remaining characters
                            result[dstPos] <- original[srcPos]
                            srcPos <- srcPos + 1
                            dstPos <- dstPos + 1
                    
                    new string(result)
                    
        /// <summary>
        /// Formats a value into a template string
        /// </summary>
        let inline format (template: string) (value: 'T) : string =
            if isNull template then ""
            else
                match box value with
                | :? string as s ->
                    replace template "{0}" s
                | null -> template
                | :? int as i ->
                    // Integer formatting
                    let mutable buffer = stackBuffer<byte> 32
                    let mutable temp = i
                    let mutable pos = 0
                    
                    // Handle negative numbers
                    let isNegative = temp < 0
                    if isNegative then
                        temp <- -temp
                    
                    // Special case for 0
                    if temp = 0 then
                        NativePtr.set buffer.Pointer pos (byte '0')
                        pos <- pos + 1
                    else
                        // Convert each digit
                        let mutable digits = stackBuffer<byte> 32
                        let mutable digitCount = 0
                        
                        while temp > 0 do
                            let digit = temp % 10
                            NativePtr.set digits.Pointer digitCount (byte (digit + int '0'))
                            digitCount <- digitCount + 1
                            temp <- temp / 10
                        
                        // Add negative sign if needed
                        if isNegative then
                            NativePtr.set buffer.Pointer pos (byte '-')
                            pos <- pos + 1
                            
                        // Write digits in reverse order
                        for i = 0 to digitCount - 1 do
                            NativePtr.set buffer.Pointer (pos + i) (NativePtr.get digits.Pointer (digitCount - 1 - i))
                        
                        pos <- pos + digitCount
                    
                    // Null terminate
                    NativePtr.set buffer.Pointer pos 0uy
                    
                    // Convert to string
                    let formattedValue = UTF8.bytesToString buffer.Pointer pos
                    replace template "{0}" formattedValue
                | _ ->
                    // Default case - convert to string and replace
                    let stringValue = string value
                    replace template "{0}" stringValue

    /// <summary>
    /// String manipulation operations implemented as static members
    /// </summary>
    [<NoEquality; NoComparison>]
    type StringOps =
        /// <summary>Concatenates two strings</summary>
        static member inline Concat(s1: string, s2: string) : string =
            String.concat s1 s2
            
        /// <summary>Concatenates three strings</summary>
        static member inline Concat(s1: string, s2: string, s3: string) : string =
            let s1Safe = if isNull s1 then "" else s1
            let s2Safe = if isNull s2 then "" else s2
            let s3Safe = if isNull s3 then "" else s3
            
            let len1 = s1Safe.Length
            let len2 = s2Safe.Length
            let len3 = s3Safe.Length
            let totalLen = add (add len1 len2) len3
            
            let result = Array.zeroCreate<char> totalLen
            
            for i = 0 to len1 - 1 do
                result[i] <- s1Safe[i]
            
            for i = 0 to len2 - 1 do
                result[add i len1] <- s2Safe[i]
                
            for i = 0 to len3 - 1 do
                result[add (add i len1) len2] <- s3Safe[i]
                
            new string(result)
            
        /// <summary>Concatenates four strings</summary>
        static member inline Concat(s1: string, s2: string, s3: string, s4: string) : string =
            let parts = [| s1; s2; s3; s4 |]
            let totalLen = 
                let mutable sum = 0
                for s in parts do
                    if not (isNull s) then
                        sum <- add sum s.Length
                sum
            
            let result = Array.zeroCreate<char> totalLen
            let mutable pos = 0
            
            for s in parts do
                if not (isNull s) then
                    for i = 0 to s.Length - 1 do
                        result[pos] <- s[i]
                        pos <- add pos 1
                        
            new string(result)
        
        /// <summary>Concatenates an array of strings</summary>
        static member inline Concat(values: string[]) : string =
            let totalLen = 
                let mutable sum = 0
                for s in values do
                    if not (isNull s) then
                        sum <- add sum s.Length
                sum
            
            let result = Array.zeroCreate<char> totalLen
            let mutable pos = 0
            
            for s in values do
                if not (isNull s) then
                    for i = 0 to s.Length - 1 do
                        result[pos] <- s[i]
                        pos <- add pos 1
                        
            new string(result)
        
        /// <summary>Joins strings with a separator</summary>
        static member inline Join(separator: string, values: string[]) : string =
            if values.Length = 0 then ""
            elif values.Length = 1 then values[0]
            else
                let sepLen = if isNull separator then 0 else separator.Length
                let totalLen = 
                    let mutable sum = 0
                    for i = 0 to values.Length - 1 do
                        if not (isNull values[i]) then
                            sum <- add sum values[i].Length
                        if i < values.Length - 1 then
                            sum <- add sum sepLen
                    sum
                
                let result = Array.zeroCreate<char> totalLen
                let mutable pos = 0
                
                for i = 0 to values.Length - 1 do
                    let s = values[i]
                    if not (isNull s) then
                        for j = 0 to s.Length - 1 do
                            result[pos] <- s[j]
                            pos <- add pos 1
                    
                    if i < values.Length - 1 && sepLen > 0 then
                        for j = 0 to sepLen - 1 do
                            result[pos] <- separator[j]
                            pos <- add pos 1
                            
                new string(result)
                
    /// <summary>
    /// Sprintf-like functionality for creating formatted strings
    /// </summary>
    module Format =
        /// <summary>
        /// Basic integer-to-string conversion
        /// </summary>
        let inline intToString (value: int) : string =
            // Special case for 0
            if value = 0 then "0"
            else
                // Handle negative numbers
                let isNegative = value < 0
                let absValue = if isNegative then -value else value
                
                // Calculate number of digits
                let mutable temp = absValue
                let mutable digits = 0
                while temp > 0 do
                    digits <- digits + 1
                    temp <- temp / 10
                
                let length = if isNegative then digits + 1 else digits
                let chars = Array.zeroCreate<char> length
                
                // Fill in the digits from right to left
                temp <- absValue
                for i = digits - 1 downto 0 do
                    let digit = temp % 10
                    chars[if isNegative then i + 1 else i] <- char (digit + int '0')
                    temp <- temp / 10
                
                // Add negative sign if needed
                if isNegative then
                    chars[0] <- '-'
                
                new string(chars)
                
        /// <summary>
        /// Simple sprintf-like function for basic format specifiers
        /// </summary>
        let inline sprintf (format: string) (args: obj[]) : string =
            let mutable result = format
            
            for i = 0 to args.Length - 1 do
                let placeholder = "{" + intToString i + "}"
                
                // Handle different types of arguments
                let value = 
                    match args[i] with
                    | :? int as n -> intToString n
                    | :? bool as b -> if b then "true" else "false" 
                    | null -> "null"
                    | x -> x.ToString()
                
                result <- String.replace result placeholder value
                
            result
            
        /// <summary>
        /// Single argument sprintf
        /// </summary>
        let inline sprintf1 (format: string) (arg: 'T) : string =
            let mutable result = format
            let placeholder = "{0}"
            
            // Handle arg
            let value = 
                match box arg with
                | :? int as n -> intToString n
                | :? bool as b -> if b then "true" else "false"
                | null -> "null" 
                | x -> x.ToString()
                
            String.replace result placeholder value