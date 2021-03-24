module DiskSpaceUsage.Program

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
        base.Width <- 600.0
        base.Height <- 400.0

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
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

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
