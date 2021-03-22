module DiskSpaceUsage.DiskItem

open System.IO
open FolderPath

type SizeOnDisk =
    Bytes of int64

type DiskItem =
    { name: string
      size: SizeOnDisk
      itemType: DiskItemType }
and DiskItemType =
    | File
    | Folder of {| path: FolderPath; children: DiskItem list |}

[<RequireQualifiedAccess>]
module DiskItem =
    let sizeInBytes (item: DiskItem) =
        match item.size with
        | Bytes bytes -> bytes

    let private createFile (fileInfo: FileInfo) =
        { name = fileInfo.Name
          size = Bytes fileInfo.Length
          itemType = File }

    let rec loadAsync folderPath: Async<DiskItem> =
        async {
            let dir = DirectoryInfo (FolderPath.path folderPath)

            let mutable children =
                dir.GetFiles()
                |> Array.toList
                |> List.map createFile

            let subFolderPaths =
                dir.GetDirectories()
                |> Array.choose (fun d -> FolderPath.create d.FullName)

            for subFolderPath in subFolderPaths do
                let! subFolder = loadAsync subFolderPath
                children <- subFolder :: children

            let size =
                children
                |> List.map sizeInBytes
                |> List.sum

            return { name = dir.Name
                     size = Bytes size
                     itemType = Folder {| path = folderPath; children = children |} }
        }
