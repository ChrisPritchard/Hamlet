module Bsp

open Model

let random = new System.Random()

let randomTerrainSet () =
    match random.Next(13) with
    | 0 -> Mountains, StonyField
    | 1 -> StonyField, Plains
    | 2 -> Plains, Forest
    | 3 -> Wetlands, Forest
    | 4 | 5 | 6 | 7 | 8 -> Forest, DeepForest
    | _ -> DeepForest, Forest

let randomTerrain (main, off) =
    match random.Next(5) with
    | 0 -> off
    | _ -> main

let range = 
    let maxInt = System.Int32.MaxValue
    List.fold (fun (mix, miy, mox, moy) (x,y) -> 
        (min mix x, min miy y, max mox x, max moy y)) 
        (maxInt, maxInt, 0, 0)

let rec bspDivide minSize list = 
    let minRange = minSize * 2
    let minx, miny, maxx, maxy = range list
    let cantx, canty = maxx - minx < minRange, maxy - miny < minRange
    if cantx && canty then 
        [list]
    else
        let xdivide () =
            let mid = random.Next(minx + minSize, maxx - minSize)
            fun (x,_) -> x > mid
        let ydivide () =
            let mid = random.Next(miny + minSize, maxy - minSize)
            fun (_,y) -> y > mid
        let divider = 
            if cantx then ydivide ()
            else if canty then xdivide ()
            else match random.Next(2) with 0 -> xdivide () | _ -> ydivide ()
        let left, right = List.partition divider list
        List.concat [bspDivide minSize left; bspDivide minSize right]



let getTiles worldDim =
    let startTiles = 
        [0..worldDim-1] |> List.collect (fun x -> 
        [0..worldDim-1] |> List.map (fun y -> (x, y)))

    let biomes = bspDivide 3 startTiles
    biomes |>
        List.collect (fun list -> 
            let terrainSet = randomTerrainSet ()
            List.map (fun (x, y) -> (x, y, randomTerrain terrainSet)) list)