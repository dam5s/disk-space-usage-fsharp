module DiskSpaceUsage.MainUI

open Avalonia.Layout
open Avalonia.Media
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL

open Icons
open FolderPath
open FolderUsage

type AsyncFolderUsage =
    | NotLoaded
    | Loading of FolderPath
    | Loaded of FolderUsage

type Model =
    { window: Window
      folderUsage : AsyncFolderUsage }

let init window _ =
    { window = window
      folderUsage = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | FinishLoading of FolderUsage
    | CloseFolder

let private selectFolderAsync window =
    async {
        let dialog = OpenFolderDialog ()
        dialog.Title <- "Select Folder"

        let! folderPath = dialog.ShowAsync(window) |> Async.AwaitTask

        let msg =
            folderPath
            |> FolderPath.create
            |> Option.map SelectFolder
            |> Option.defaultValue CloseFolder

        return msg
    }

let private loadFolderUsageAsync path =
    async {
        let! usage = FolderUsage.loadAsync path
        return FinishLoading usage
    }

let private asyncCmd = Cmd.OfAsync.result

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenFolderSelectDialog ->
        model, asyncCmd (selectFolderAsync model.window)
    | SelectFolder path ->
        { model with folderUsage = Loading path }, asyncCmd (loadFolderUsageAsync path)
    | FinishLoading usage ->
        { model with folderUsage = Loaded usage }, Cmd.none
    | CloseFolder ->
        { model with folderUsage = NotLoaded }, Cmd.none

module FolderSizeView =
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

let private notLoadedView dispatch =
    Grid.create [
        Grid.columnDefinitions "50, *, 50"
        Grid.rowDefinitions "*, 50, 50, *"
        Grid.children [
            TextBlock.create [
                Grid.row 1
                Grid.column 1
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text "No folder selected"
            ]
            Button.create [
                Grid.row 2
                Grid.column 1
                Button.height 50.0
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Folder"
            ]
        ]
    ]

let private loadingView folderPath dispatch =
    Grid.create [
        Grid.columnDefinitions "*"
        Grid.rowDefinitions "*, 50, 50, *"
        Grid.children [
            TextBlock.create [
                Grid.row 1
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 48.0
                TextBlock.text "Loading..."
            ]
            TextBlock.create [
                Grid.row 2
                TextBlock.verticalAlignment VerticalAlignment.Top
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text (FolderPath.path folderPath)
            ]
        ]
    ]

let private loadedView usage dispatch =
    let fullPath =
        usage
        |> FolderUsage.path
        |> FolderPath.path

    let sizeText =
        usage
        |> FolderUsage.size
        |> FolderSizeView.text

    Grid.create [
        Grid.columnDefinitions "*"
        Grid.rowDefinitions "80, *, 50, 50, *, 80"
        Grid.children [
            Button.create [
                Grid.row 0

                Button.verticalAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.margin 20.0
                Button.content Icons.closeCircle
                Button.classes ["icon"]
                Button.onClick (fun _ -> dispatch CloseFolder)
            ]
            TextBlock.create [
                Grid.row 2
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 48.0
                TextBlock.text sizeText
            ]
            TextBlock.create [
                Grid.row 3
                TextBlock.verticalAlignment VerticalAlignment.Top
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text fullPath
            ]
        ]
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model.folderUsage with
    | NotLoaded -> notLoadedView dispatch
    | Loading path -> loadingView path dispatch
    | Loaded usage -> loadedView usage dispatch
