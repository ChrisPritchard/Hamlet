module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let advanceGame (runState : RunState) =
    function
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> Some ({ tiles = [] })
    | Some w -> Some w