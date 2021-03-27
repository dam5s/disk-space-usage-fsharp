module DiskSpaceUsage.Program

open Avalonia.Controls
open Avalonia.Media
open Avalonia.Platform
open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "Disk Space Usage"
        base.Width <- 1200.0
        base.Height <- 800.0
        base.ExtendClientAreaToDecorationsHint <- true
        base.TransparencyLevelHint <- WindowTransparencyLevel.AcrylicBlur
        base.ExtendClientAreaChromeHints <- ExtendClientAreaChromeHints.PreferSystemChrome
        base.Background <- Brush.Parse("#b333")

        ((MainUI.init this), MainUI.update, MainUI.view)
        |||> Elmish.Program.mkProgram
        |> Program.withSubscription MainUI.subscribe
        |> Program.withHost this
        |> Program.run

        this
            .GetPropertyChangedObservable(Controls.TopLevel.BoundsProperty)
            .Subscribe(fun _ -> MainUI.Subscriptions.windowBoundsChanged this.Bounds)
            |> ignore

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Fluent/FluentDark.xaml"
        this.Styles.Load "avares://disk-space-usage-app/Styles.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
