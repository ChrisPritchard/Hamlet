module Controller

open GameCore
open Model

let advanceGame (runState : RunState) =
    function
    | None -> Some ({ tiles = [] })
    | Some w -> Some w