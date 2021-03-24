module DiskSpaceUsage.Styles

open Avalonia
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.Controls.Presenters
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Styling

[<RequireQualifiedAccess>]
module Styles =
    let create selector (setters: Setter list) =
        let style = Style(fun s -> selector s)
        for setter in setters do
            style.Setters.Add setter

        let styles = Styles()
        styles.Add style
        styles

    let cornerRadius value =
        Setter(ContentPresenter.CornerRadiusProperty, CornerRadius(value))

[<RequireQualifiedAccess>]
module Button =
    let icon (content: IView) attrs: IView =
        let selector (s: Selector) =
            s.OfType<Button>().Template().OfType<ContentPresenter>()

        let styles = Styles.create selector [
            Styles.cornerRadius 25.0
        ]

        let defaults = [
            Button.verticalAlignment VerticalAlignment.Center
            Button.background "transparent"
            Button.margin 20.0
            Button.width 40.0
            Button.height 40.0
            Button.content content
            Button.styles styles
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
