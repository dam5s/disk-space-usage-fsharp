module DiskSpaceUsage.Debounce

open DiskSpaceUsage.Time

[<RequireQualifiedAccess>]
module Debounce =
    type State =
        private { delayInMs: int; lastNotify: Posix }

    let init ms =
        { delayInMs = ms; lastNotify = Posix.now() }

    let invoke f state =
        let now = Posix.now()
        let msSinceLastNotify = Posix.milliseconds now - Posix.milliseconds state.lastNotify

        if msSinceLastNotify > int64 state.delayInMs
        then
            f()
            { state with lastNotify = now }
        else
            state
