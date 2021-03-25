module DiskSpaceUsage.Styles

open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

[<RequireQualifiedAccess>]
module Button =
    let icon (content: IView) attrs: IView =
        let defaults = [
            Button.verticalAlignment VerticalAlignment.Center
            Button.margin 20.0
            Button.content content
            Button.classes ["icon"]
        ]
        Button.create (defaults @ attrs) :> IView

[<RequireQualifiedAccess>]
module Grid =
    let main attrs: IView =
        let defaults = [
            Grid.columnDefinitions "*"
            Grid.rowDefinitions "80, 50, 50, *, 80"
        ]
        Grid.create (defaults @ attrs) :> IView

    let resizableRowHeight windowHeight =
        windowHeight - 80.0 - 50.0 - 50.0 - 80.0

[<RequireQualifiedAccess>]
module TextBlock =
    let title text attrs: IView =
        let defaults = [
            TextBlock.textAlignment TextAlignment.Center
            TextBlock.verticalAlignment VerticalAlignment.Bottom
            TextBlock.fontSize 48.0
            TextBlock.text text
        ]
        TextBlock.create (defaults @ attrs) :> IView

    let subTitle text attrs: IView =
        let defaults = [
            TextBlock.textAlignment TextAlignment.Center
            TextBlock.verticalAlignment VerticalAlignment.Top
            TextBlock.fontSize 24.0
            TextBlock.text text
        ]
        TextBlock.create (defaults @ attrs) :> IView
