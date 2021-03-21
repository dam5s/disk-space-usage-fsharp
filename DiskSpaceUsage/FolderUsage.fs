module DiskSpaceUsage.FolderUsage

open System.IO
open FolderPath

type SizeOnDisk =
    Bytes of int64

type FilePath =
    private FilePath of string

type FileUsage =
    private { path: FilePath
              size: SizeOnDisk }

type FolderUsage =
    private { path: FolderPath
              size: SizeOnDisk
              subFiles: FileUsage list
              subFolders: FolderUsage list }

[<RequireQualifiedAccess>]
module FileUsage =
    let create (fileInfo: FileInfo) =
        { path = FilePath fileInfo.FullName
          size = Bytes fileInfo.Length }

    let sizeInBytes (file: FileUsage) =
        match file.size with
        | Bytes bytes -> bytes

[<RequireQualifiedAccess>]
module FolderUsage =
    let private sizeInBytes (folder: FolderUsage) =
        match folder.size with
        | Bytes bytes -> bytes

    let rec loadAsync folderPath: Async<FolderUsage> =
        async {
            let dir = DirectoryInfo (FolderPath.path folderPath)

            let subFiles =
                dir.GetFiles()
                |> Array.toList
                |> List.map FileUsage.create

            let mutable subFolders: FolderUsage list = []

            let subFolderPaths =
                dir.GetDirectories()
                |> Array.choose (fun d -> FolderPath.create d.FullName)

            for subFolderPath in subFolderPaths do
                let! subFolder = loadAsync subFolderPath
                subFolders <- subFolder :: subFolders

            let filesSize =
                subFiles
                |> List.map FileUsage.sizeInBytes
                |> List.sum

            let foldersSize =
                subFolders
                |> List.map sizeInBytes
                |> List.sum

            return { path = folderPath
                     size = Bytes (filesSize + foldersSize)
                     subFiles = subFiles
                     subFolders = subFolders }
        }

    let size usage = usage.size

    let path usage = usage.path
