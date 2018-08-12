module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let newWorld () = {
    tiles = Bsp.getTiles worldDim
}

let advanceGame (runState : RunState) =
    function
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> Some <| newWorld ()
    | Some _ when runState.WasJustPressed Keys.R -> Some <| newWorld ()
    | other -> other