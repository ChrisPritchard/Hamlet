module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let startWorld = {
    tiles = Bsp.getTiles worldDim
}

let advanceGame (runState : RunState) =
    function
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> Some startWorld
    | Some w -> Some w