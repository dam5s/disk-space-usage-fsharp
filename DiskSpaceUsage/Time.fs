module DiskSpaceUsage.Time

type Posix =
    Posix of milliseconds: int64

[<RequireQualifiedAccess>]
module Posix =
    open System
    let fromDateTimeOffset (d: DateTimeOffset) = Posix (d.ToUnixTimeMilliseconds())
    let now () = fromDateTimeOffset DateTimeOffset.Now
    let milliseconds (Posix m) = m
