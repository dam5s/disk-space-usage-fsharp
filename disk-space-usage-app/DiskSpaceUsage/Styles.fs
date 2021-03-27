module DiskSpaceUsage.Styles

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout

open Icons

[<RequireQualifiedAccess>]
module Button =
    let icon color shape attrs: IView =
        let defaults = [
            Button.content (Icons.create color shape )
            Button.classes ["icon"]
        ]
        Button.create (defaults @ attrs) :> IView

    let navBarIcon color shape attrs =
        let defaults = [
            Button.verticalAlignment VerticalAlignment.Center
        ]
        icon color shape (defaults @ attrs)

[<RequireQualifiedAccess>]
module Grid =
    let main attrs: IView =
        let defaults = [
            Grid.columnDefinitions "*"
            Grid.rowDefinitions "80, 80, 80, *, 80"
        ]
        Grid.create (defaults @ attrs) :> IView

    let resizableRowHeight windowHeight =
        windowHeight - 80.0 * 4.0

[<RequireQualifiedAccess>]
module TextBlock =
    let title text attrs: IView =
        let defaults = [
            TextBlock.classes ["title"]
            TextBlock.text text
        ]
        TextBlock.create (defaults @ attrs) :> IView

    let subTitle text attrs: IView =
        let defaults = [
            TextBlock.classes ["subTitle"]
            TextBlock.text text
        ]
        TextBlock.create (defaults @ attrs) :> IView
