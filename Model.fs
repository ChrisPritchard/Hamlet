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

let range = 
    let maxInt = System.Int32.MaxValue
    List.fold (fun (mix, miy, mox, moy) (x,y) -> 
        (min mix x, min miy y, max mox x, max moy y)) 
        (maxInt, maxInt, 0, 0)

let rec bspDivide minSize list = 
    let minRange = minSize * 2
    let minx, miny, maxx, maxy = range list
    let cantx, canty = maxx - minx < minRange, maxy - miny < minRange
    let xdivide () =
        let mid = random.Next(minx + minSize, maxx - minSize)
        fun (x,_) -> x > mid
    let ydivide () =
        let mid = random.Next(miny + minSize, maxy - minSize)
        fun (_,y) -> y > mid
    if cantx && canty then 
        [list]
    else
        let divider = 
            if cantx then ydivide ()
            else if canty then xdivide ()
            else match random.Next(2) with 0 -> xdivide () | _ -> ydivide ()
        let left, right = List.partition divider list
        List.concat [bspDivide minSize left; bspDivide minSize right]

let startTiles = 
    [0..worldDim-1] |> List.collect (fun x -> 
    [0..worldDim-1] |> List.map (fun y -> (x, y)))

let biomes = bspDivide 3 startTiles

let startWorld = {
    tiles = 
        biomes |>
        List.collect (fun list -> 
            let terrain = randomTerrain ()
            List.map (fun (x, y) -> (x, y, terrain)) list)
}