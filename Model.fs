module Model

type World = {
    tiles: (int * int * Terrain) list
} and Terrain =
    | DeepForest | Forest | ScrubLand | Plains 
    | StonyField | Wetlands | Mountains

let random = new System.Random()

let randomTerrain () =
    match random.Next(10) with
    | 0 -> Mountains
    | 1 -> StonyField
    | 2 | 3 -> Plains
    | 4 | 5 -> Wetlands
    | 6 | 7 | 8 -> Forest
    | _ -> DeepForest
   
let startWorld = {
    tiles = [0..9] |> List.collect (fun x -> 
        [0..9] |> List.map (fun y -> (x, y, randomTerrain ())))
}