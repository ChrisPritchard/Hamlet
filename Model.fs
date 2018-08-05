module Model

type World = {
    tiles: (int * int * Terrain) list
} and Terrain =
    | DeepForest | Forest | ScrubLand | Plains 
    | StonyField | Wetlands | Mountains

let random = new System.Random()

let randomTerrain () =
    match random.Next(13) with
    | 0 -> Mountains
    | 1 -> StonyField
    | 2 -> Plains
    | 3 -> Wetlands
    | 4 | 5 | 6 | 7 | 8 -> Forest
    | _ -> DeepForest

let worldDim = 20
let startWorld = {
    tiles = [0..worldDim-1] |> List.collect (fun x -> 
        [0..worldDim-1] |> List.map (fun y -> (x, y, randomTerrain ())))
}