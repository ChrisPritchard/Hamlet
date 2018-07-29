module View

open GameCore

let screenWidth, screenHeight = 800, 600
let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    TextureMap ("tiles", "./Content/tiles.png", "./Content/tiles-key.csv")
]

let getView runState =
    function
    | _ -> []