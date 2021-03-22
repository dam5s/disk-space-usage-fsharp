module DiskSpaceUsage.Icons

[<RequireQualifiedAccess>]
module Icons =
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL

    let private materialDesignIcon (color: string) (path: string) =
        Canvas.create [
            Canvas.width 24.0
            Canvas.height 24.0
            Canvas.children [
                Path.create [
                    Path.fill color
                    Path.data path
                ]
            ]
        ]

    let closeCircle =
        materialDesignIcon
            "White"
            "M12,2C17.53,2 22,6.47 22,12C22,17.53 17.53,22 12,22C6.47,22 2,17.53 2,12C2,6.47 6.47,2 12,2M15.59,7L12,10.59L8.41,7L7,8.41L10.59,12L7,15.59L8.41,17L12,13.41L15.59,17L17,15.59L13.41,12L17,8.41L15.59,7Z"

    let arrowLeftCircle =
        materialDesignIcon
            "White"
            "M2,12A10,10 0 0,1 12,2A10,10 0 0,1 22,12A10,10 0 0,1 12,22A10,10 0 0,1 2,12M18,11H10L13.5,7.5L12.08,6.08L6.16,12L12.08,17.92L13.5,16.5L10,13H18V11Z"
