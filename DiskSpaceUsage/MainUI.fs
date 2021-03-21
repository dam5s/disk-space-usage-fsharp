module DiskSpaceUsage.MainUI

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open FolderUsage

type AsyncData<'a> =
    | NotLoaded
    | Loading
    | Loaded of 'a

type Model =
    { window: Window
      path : FolderPath option
      usage: AsyncData<FolderUsage> }

let init window _ =
    { window = window
      path = None
      usage = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | FinishLoading of FolderUsage

let private selectFolderAsync window =
    async {
        let dialog = OpenFolderDialog ()
        dialog.Title <- "Select Folder"

        let! folderPath = dialog.ShowAsync(window) |> Async.AwaitTask
        return SelectFolder (FolderPath folderPath)
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
        { model with path = Some path; usage = Loading }, asyncCmd (loadFolderUsageAsync path)
    | FinishLoading usage ->
        { model with usage = Loaded usage }, Cmd.none

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

    let display size =
        match size with
        | Gigabytes value -> $"%.1f{value} GB"
        | Megabytes value -> $"%.1f{value} MB"
        | Kilobytes value -> $"%.1f{value} KB"
        | Bytes value -> $"%d{value} B"

let view (model: Model) (dispatch: Dispatch<Msg>) =
    let folderText =
        model.path
        |> Option.map FolderPath.value
        |> Option.defaultValue "No folder selected"

    let usageText =
        match model.usage with
        | NotLoaded -> ""
        | Loading -> "Calculating size..."
        | Loaded usage ->
            usage
            |> FolderUsage.size
            |> FolderSizeView.display

    Grid.create [
        Grid.columnDefinitions "*"
        Grid.rowDefinitions "*, *, 50"
        Grid.children [
            TextBlock.create [
                Grid.row 0

                TextBlock.fontSize 24.0
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.horizontalAlignment HorizontalAlignment.Center
                TextBlock.text folderText
            ]
            TextBlock.create [
                Grid.row 1

                TextBlock.dock Dock.Top
                TextBlock.fontSize 48.0
                TextBlock.verticalAlignment VerticalAlignment.Top
                TextBlock.horizontalAlignment HorizontalAlignment.Center
                TextBlock.text usageText
            ]
            Button.create [
                Grid.row 2

                Button.dock Dock.Bottom
                Button.height 50.0
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Folder"
            ]
        ]
    ]
