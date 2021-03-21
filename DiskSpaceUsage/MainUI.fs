module DiskSpaceUsage.MainUI

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

type Model =
    { window: Window
      folder : string option }

let init window _ =
    { window = window
      folder = None }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of path:string

let private selectDirectoryAsync (window: Window) =
    async {
        let dialog = OpenFolderDialog ()
        dialog.Title <- "Select Folder"

        let! folderPath = dialog.ShowAsync(window) |> Async.AwaitTask
        return SelectFolder folderPath
    }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenFolderSelectDialog ->
        model, Cmd.OfAsync.result (selectDirectoryAsync model.window)
    | SelectFolder path ->
        { model with folder = Some path }, Cmd.none

let view (model: Model) (dispatch: Dispatch<Msg>) =
    let folderText = model.folder
                     |> Option.defaultValue "No folder selected"

    DockPanel.create [
        DockPanel.children [
            Button.create [
                Button.dock Dock.Bottom
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Directory"
            ]
            TextBlock.create [
                TextBlock.dock Dock.Top
                TextBlock.fontSize 48.0
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.horizontalAlignment HorizontalAlignment.Center
                TextBlock.text folderText
            ]
        ]
    ]
