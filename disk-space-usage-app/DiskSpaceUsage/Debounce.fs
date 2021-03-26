module DiskSpaceUsage.Debounce

open DiskSpaceUsage.Time

[<RequireQualifiedAccess>]
module Debounce =
    type State =
        { msBetweenInvocations: int
          lastNotify: Posix }

    let init ms =
        { msBetweenInvocations = ms
          lastNotify = Posix.now() }

    let invoke f state =
        let now = Posix.now()
        let msSinceLastNotify = Posix.milliseconds now - Posix.milliseconds state.lastNotify

        if msSinceLastNotify > int64 state.msBetweenInvocations
        then
            f()
            { state with lastNotify = now }
        else
            state
