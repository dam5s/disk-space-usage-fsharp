module DiskSpaceUsage.FolderUsage

open FolderPath

type FolderSize =
    Bytes of int64

type FolderUsage =
    private { path: FolderPath
              size: FolderSize
              children: FolderUsage list }

[<RequireQualifiedAccess>]
module FolderUsage =
    open System.IO

    let private sizeInBytes (folder: FolderUsage) =
        match folder.size with
        | Bytes bytes -> bytes

    let rec loadAsync folderPath: Async<FolderUsage> =
        async {
            let dir = DirectoryInfo (FolderPath.path folderPath)

            let filesSize =
                dir.GetFiles()
                |> Array.map (fun f -> f.Length)
                |> Array.sum

            let mutable children: FolderUsage list = []

            let subFolderPaths =
                dir.GetDirectories()
                |> Array.choose (fun d -> FolderPath.create d.FullName)

            for subFolderPath in subFolderPaths do
                let! subFolderUsage = loadAsync subFolderPath
                children <- subFolderUsage :: children

            let childrenSize =
                children
                |> List.map sizeInBytes
                |> List.sum

            return { path = folderPath
                     size = Bytes (filesSize + childrenSize)
                     children = children }
        }

    let size usage = usage.size

    let path usage = usage.path
