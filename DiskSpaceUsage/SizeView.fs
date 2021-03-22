module DiskSpaceUsage.SizeView

open DiskItem

[<RequireQualifiedAccess>]
module SizeView =
    let private tryUnit bytesPerUnit bytes =
        let floatBytes = float bytes

        if floatBytes > bytesPerUnit
        then Some (floatBytes / bytesPerUnit)
        else None

    let private (|Gigabytes|_|) (Bytes bytes) =
        tryUnit (1024.0 ** 3.0) bytes

    let private (|Megabytes|_|) (Bytes bytes) =
        tryUnit (1024.0 ** 2.0) bytes

    let private (|Kilobytes|_|) (Bytes bytes) =
        tryUnit 1024.0 bytes

    let text size =
        match size with
        | Gigabytes value -> $"%.1f{value} GB"
        | Megabytes value -> $"%.1f{value} MB"
        | Kilobytes value -> $"%.1f{value} KB"
        | Bytes value -> $"%d{value} B"
