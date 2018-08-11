module Bsp

open Model

let random = new System.Random()

let terrainRates = [
    (0.1, Mountains, StonyField)
    (0.1, StonyField, Plains)
    (0.1, Plains, Forest)
    (0.1, Wetlands, Forest)
    (0.3, Forest, DeepForest)
    (0.3, DeepForest, Forest)
]

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

let randomTerrain (main, off) =
    match random.Next(5) with
    | 0 -> off
    | _ -> main

let biomeTypes length = 
    let total = length |> float
    let result = 
        terrainRates 
        |> List.collect (fun (ratio, main, off) ->
            let count = ratio * total |> int
            List.replicate count (main, off))
    let padded = List.replicate (length - List.length result) (DeepForest, Forest)
    result @ padded |> List.mapi (fun i o -> i, o)

let getTiles worldDim =
    let startTiles = 
        [0..worldDim-1] |> List.collect (fun x -> 
        [0..worldDim-1] |> List.map (fun y -> (x, y)))
    let tiles = bspDivide 3 startTiles

    let biomes = biomeTypes <| List.length startTiles
    tiles |>
        List.fold (fun (biomes, result) localTiles -> 
            let biomesLength = List.length biomes
            let (i, terrainSet) = biomes.[random.Next(biomesLength)]
            let newBiomes = List.except [(i, terrainSet)] biomes
            let newTiles = localTiles |> List.map (fun (x, y) -> x, y, randomTerrain terrainSet)
            newBiomes, result @ newTiles) 
            (biomes, [])
        |> snd