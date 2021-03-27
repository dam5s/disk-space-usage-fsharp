module DiskSpaceUsage.DiskItem

open System.IO
open FolderPath

type SizeOnDisk =
    | Bytes of int64
    | Unknown

type DiskItem =
    { name: string
      size: SizeOnDisk
      itemType: DiskItemType }
and DiskItemType =
    | File
    | Folder of {| path: FolderPath; children: DiskItem list |}

type private SystemDir =
    | Readable of ReadableRecord
    | Unreadable of UnreadableRecord
and private ReadableRecord =
    { name: string; files: FileInfo[]; dirs: DirectoryInfo[] }
and private UnreadableRecord =
    { name: string }

[<RequireQualifiedAccess>]
module private SystemDir =
    let load folderPath =
        let dir = folderPath
                  |> FolderPath.path
                  |> DirectoryInfo
        try
            Readable { name = dir.Name
                       files = dir.GetFiles()
                       dirs = dir.GetDirectories() }
        with _ ->
            Unreadable { name = dir.Name }

[<RequireQualifiedAccess>]
module rec DiskItem =
    let sizeInBytes (item: DiskItem) =
        match item.size with
        | Bytes bytes -> Some bytes
        | Unknown -> None

    let fileSize (fileInfo: FileInfo) =
        try Bytes fileInfo.Length
        with _ -> Unknown

    let private createFile (fileInfo: FileInfo) =
        { name = fileInfo.Name
          size = fileSize fileInfo
          itemType = File }

    let private loadUnreadableRecord folderPath record: Async<DiskItem> =
        async {
            let itemType =
                Folder {| path = folderPath; children = [] |}
            return
                { name = record.name
                  size = Unknown
                  itemType = itemType }
        }

    let private loadReadableRecord notify folderPath record: Async<DiskItem> =
        async {
            let mutable children =
                record.files
                |> Array.toList
                |> List.map createFile

            let subFolderPaths =
                record.dirs
                |> Array.choose (fun d -> FolderPath.create d.FullName)

            for subFolderPath in subFolderPaths do
                let! subFolder = loadAsync notify subFolderPath
                children <- subFolder :: children

            let size =
                children
                |> List.choose sizeInBytes
                |> List.sum

            return { name = record.name
                     size = Bytes size
                     itemType = Folder {| path = folderPath; children = children |} }
        }

    let loadAsync (notify: FolderPath -> unit) folderPath: Async<DiskItem> =
        async {
            notify folderPath

            let asyncDiskItem =
                match SystemDir.load folderPath with
                | Unreadable record -> loadUnreadableRecord folderPath record
                | Readable record -> loadReadableRecord notify folderPath record

            return! asyncDiskItem
        }
