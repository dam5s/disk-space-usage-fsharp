# Disk Space Usage

This is a small desktop app that will allow you to analyze
the disk space usage of a given directory of your choosing.

It's built in [F#](https://fsharp.org) using [Avalonia](https://avaloniaui.net/)
and its [functional extensions](https://avaloniacommunity.github.io/Avalonia.FuncUI.Docs/).

It runs cross-platform (on Windows, Linux and Mac OS) using **.NET 5**.

## Screenshot

![Screenshot](screenshot.png)

## Local Setup

### Dependencies
[.NET 5](https://dotnet.microsoft.com/download/dotnet/5.0)

### Running the build

```
dotnet build
```

### Running the app

```
dotnet run
```

### Publishing the app

I use [warp-packer](https://github.com/dgiagio/warp) to combine the binary and DLLs into a single file. 

#### Windows x64
```
dotnet publish disk-space-usage-app -c Release -r win-x64
warp-packer --arch windows-x64 --input_dir disk-space-usage-app/bin/Release/net5.0/win-x64/publish --exec disk-space-usage-app.exe --output disk-space-usage-app/bin/Release/disk-space-usage-win-x64.exe
```

#### Linux x64
```
dotnet publish disk-space-usage-app -c Release -r linux-x64
warp-packer --arch linux-x64 --input_dir disk-space-usage-app/bin/Release/net5.0/linux-x64/publish --exec disk-space-usage-app --output disk-space-usage-app/bin/Release/disk-space-usage-linux-x64 
```

#### MacOS x64
```
dotnet publish disk-space-usage-app -c Release -r osx-x64
warp-packer --arch macos-x64 --input_dir disk-space-usage-app/bin/Release/net5.0/osx-x64/publish --exec disk-space-usage-app --output disk-space-usage-app/bin/Release/disk-space-usage-osx-x64 
```
