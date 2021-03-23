module DiskSpaceUsage.SizeView

open DiskItem

[<RequireQualifiedAccess>]
module SizeView =
    let private tryUnit bytesPerUnit size =
        match size with
        | Unknown -> None
        | Bytes bytes ->
            let floatBytes = float bytes

            if floatBytes > bytesPerUnit
            then Some (floatBytes / bytesPerUnit)
            else None

    let private (|Gigabytes|_|) size =
        tryUnit (1024.0 ** 3.0) size

    let private (|Megabytes|_|) size =
        tryUnit (1024.0 ** 2.0) size

    let private (|Kilobytes|_|) size =
        tryUnit 1024.0 size

    let text size =
        match size with
        | Gigabytes value -> $"%.1f{value} GB"
        | Megabytes value -> $"%.1f{value} MB"
        | Kilobytes value -> $"%.1f{value} KB"
        | Bytes value -> $"%d{value} B"
        | Unknown -> "Unknown"
