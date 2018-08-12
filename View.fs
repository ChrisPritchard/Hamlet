module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 1150, 600
let resolution = Windowed (screenWidth, screenHeight)

let tileWidth, tileHeight = 36, 36
let halfWidth, halfHeight = tileWidth / 2, tileHeight / 4 // cells are double height for faux-3D
let offsetX, offsetY = 60, screenHeight / 2

let xFrom x y = (x * halfWidth) + (y * halfWidth)
let yFrom x y = (y * halfHeight) - (x * halfHeight)

let rectFrom x y = 
    let x, y = x + offsetX, y + offsetY
    (x - tileWidth / 2, y - tileHeight, tileWidth, tileHeight)

let mapKeyFrom =
    function
    | Forest -> "forest"
    | DeepForest -> "deep-forest"
    | Mountains -> "mountains"
    | Plains -> "plains"
    | ScrubLand -> "scrubland"
    | StonyField -> "stony-field"
    | Wetlands -> "wetland"

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    TextureMap ("tiles", "./Content/tiles.png", "./Content/tiles-key.csv")
]

let getView _ =
    function
    | world -> 
        world.tiles 
            |> List.sortBy(fun (x, y, _) -> y, -x)
            |> List.map (fun (x, y, terrain) ->
            let texture = mapKeyFrom terrain
            let tx, ty = xFrom x y, yFrom x y
            MappedImage ("tiles", texture, rectFrom tx ty, Color.White))