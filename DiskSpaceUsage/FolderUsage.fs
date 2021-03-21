module DiskSpaceUsage.FolderUsage

type FolderPath =
    FolderPath of string

type FolderSize =
    | Bytes of int64

type FolderUsage =
    private { path: FolderPath
              size: FolderSize
              children: FolderUsage list }

[<RequireQualifiedAccess>]
module FolderPath =
    let value (FolderPath p) = p

[<RequireQualifiedAccess>]
module FolderUsage =

    open System.IO

    let private sizeInBytes (folder: FolderUsage) =
        match folder.size with
        | Bytes bytes -> bytes

    let rec loadAsync (FolderPath path): Async<FolderUsage> =
        async {
            let dir = DirectoryInfo path

            let filesSize =
                dir.GetFiles()
                |> Array.map (fun f -> f.Length)
                |> Array.sum

            let mutable children: FolderUsage list = []

            for d in dir.GetDirectories() do
                let subPath = FolderPath d.FullName
                let! subUsage = loadAsync subPath
                children <- subUsage :: children

            let childrenSize =
                children
                |> List.map sizeInBytes
                |> List.sum

            return { path = FolderPath path
                     size = Bytes (filesSize + childrenSize)
                     children = children }
        }

    let size usage = usage.size
