module DiskSpaceUsage.FolderPath

type FolderPath =
    private FolderPath of string

[<RequireQualifiedAccess>]
module FolderPath =
    open System.IO

    let create path =
        if Directory.Exists path
        then Some (FolderPath path)
        else None

    let path (FolderPath p) = p
