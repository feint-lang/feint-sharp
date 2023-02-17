module Feint.Compiler.Util

let dprint (str: string) =
#if DEBUG
    System.Console.Error.Write(str)
#else
    ()
#endif

let dprintn (str: string) =
#if DEBUG
    System.Console.Error.WriteLine(str)
#else
    ()
#endif
