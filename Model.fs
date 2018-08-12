module Model

type World = {
    tiles: (int * int * Terrain) list
} and Terrain =
    | DeepForest | Forest | ScrubLand | Plains 
    | StonyField | Wetlands | Mountains

let worldDim = 30